"""
Tiny character-level GRU trained on Aesop's Fables.
Exports INT8-quantized weights for FPGA deployment.
"""

import torch
import torch.nn as nn
import numpy as np
import json
import struct
import os

# ---------------------------------------------------------------------------
# 1. Dataset
# ---------------------------------------------------------------------------

CORPUS = """
The Fox and the Grapes
A hungry fox saw some fine bunches of grapes hanging from a vine.
He used all his jumping skill to reach them, but failed.
At last he turned away saying it is sure they are sour.

The Tortoise and the Hare
A hare one day ridiculed the short feet and slow pace of the tortoise.
The tortoise laughed and said though you be swift as the wind I will beat you in a race.
The hare lying down took a nap. The tortoise plodded on and on and won the race.

The Lion and the Mouse
A lion was awakened from sleep by a mouse running over his face.
He caught it and was about to kill it when the mouse begged for its life.
The lion let it go. Later the lion was caught in a net and the mouse gnawed the ropes and set him free.

The Crow and the Pitcher
A crow perishing with thirst saw a pitcher and hoped to find water.
He found water but so low he could not reach it.
He dropped in pebbles one by one until the water rose and he could drink.

The Ant and the Grasshopper
In summer the ant worked hard storing food for winter.
The grasshopper sang all day. When winter came the grasshopper starved.
Work today and you shall eat tomorrow.

The Wolf in Sheep Clothing
A wolf found a sheepskin and put it on.
He grazed with the sheep and the shepherd knew not.
At night the shepherd locked the wolf in with the sheep and killed him for supper.

The Dog and the Bone
A dog crossing a bridge saw his reflection in the water below.
Thinking it another dog with a bigger bone he let go to get the other.
He lost his bone and got nothing.
""".strip()

# ---------------------------------------------------------------------------
# 2. Vocabulary
# ---------------------------------------------------------------------------

chars = sorted(set(CORPUS))
vocab_size = len(chars)
char2idx = {c: i for i, c in enumerate(chars)}
idx2char = {i: c for c, i in char2idx.items()}

print(f"Vocab size: {vocab_size}")
print(f"Corpus length: {len(CORPUS)} chars")

# ---------------------------------------------------------------------------
# 3. Model
# ---------------------------------------------------------------------------

HIDDEN_SIZE = 128   # fits comfortably in 20K LUT BRAM at INT8
SEQ_LEN     = 64
BATCH_SIZE  = 32
EPOCHS      = 300
LR          = 0.003

class TinyGRU(nn.Module):
    def __init__(self, vocab_size, hidden_size):
        super().__init__()
        self.hidden_size = hidden_size
        self.embed  = nn.Embedding(vocab_size, hidden_size)
        self.gru    = nn.GRU(hidden_size, hidden_size, batch_first=True)
        self.fc     = nn.Linear(hidden_size, vocab_size)

    def forward(self, x, h=None):
        e = self.embed(x)
        out, h = self.gru(e, h)
        logits = self.fc(out)
        return logits, h

    def init_hidden(self, batch_size):
        return torch.zeros(1, batch_size, self.hidden_size)

# ---------------------------------------------------------------------------
# 4. Training data
# ---------------------------------------------------------------------------

def make_batches(text, seq_len, batch_size):
    encoded = [char2idx[c] for c in text]
    n = (len(encoded) - 1) // seq_len
    xs, ys = [], []
    for i in range(n):
        xs.append(encoded[i*seq_len:(i+1)*seq_len])
        ys.append(encoded[i*seq_len+1:(i+1)*seq_len+1])
    xs = torch.tensor(xs, dtype=torch.long)
    ys = torch.tensor(ys, dtype=torch.long)
    dataset = torch.utils.data.TensorDataset(xs, ys)
    return torch.utils.data.DataLoader(dataset, batch_size=batch_size, shuffle=True)

loader = make_batches(CORPUS, SEQ_LEN, BATCH_SIZE)

# ---------------------------------------------------------------------------
# 5. Train
# ---------------------------------------------------------------------------

model    = TinyGRU(vocab_size, HIDDEN_SIZE)
optimizer = torch.optim.Adam(model.parameters(), lr=LR)
criterion = nn.CrossEntropyLoss()

print("\nTraining...")
for epoch in range(1, EPOCHS + 1):
    total_loss = 0
    for xb, yb in loader:
        h = model.init_hidden(xb.size(0))
        logits, _ = model(xb, h)
        loss = criterion(logits.view(-1, vocab_size), yb.view(-1))
        optimizer.zero_grad()
        loss.backward()
        torch.nn.utils.clip_grad_norm_(model.parameters(), 1.0)
        optimizer.step()
        total_loss += loss.item()
    if epoch % 50 == 0:
        print(f"  Epoch {epoch:4d}  loss={total_loss/len(loader):.4f}")

# ---------------------------------------------------------------------------
# 6. Quick generation test (float32)
# ---------------------------------------------------------------------------

def generate(model, seed, length=200, temperature=0.8):
    model.eval()
    h = model.init_hidden(1)
    result = seed
    x = torch.tensor([[char2idx[seed[-1]]]])
    with torch.no_grad():
        # warm up on seed
        for c in seed[:-1]:
            xi = torch.tensor([[char2idx[c]]])
            _, h = model(xi, h)
        for _ in range(length):
            logits, h = model(x, h)
            probs = torch.softmax(logits[0, 0] / temperature, dim=0)
            idx = torch.multinomial(probs, 1).item()
            result += idx2char[idx]
            x = torch.tensor([[idx]])
    return result

print("\nSample (float32):")
print(generate(model, "A "))

# ---------------------------------------------------------------------------
# 7. INT8 quantization & weight export
# ---------------------------------------------------------------------------

def quantize_tensor(t, bits=8):
    """Symmetric per-tensor quantization → returns (int8 array, scale float)."""
    t = t.detach().float().numpy()
    amax  = np.max(np.abs(t))
    scale = amax / (2**(bits-1) - 1) if amax != 0 else 1.0
    q     = np.clip(np.round(t / scale), -(2**(bits-1)), 2**(bits-1)-1).astype(np.int8)
    return q, float(scale)

os.makedirs("weights", exist_ok=True)

# GRU weight matrices (PyTorch packs them as [3*H, input_size] for GRU)
# Weight_ih: [3*H, H]  (input→ reset, update, new gates)
# Weight_hh: [3*H, H]  (hidden→ reset, update, new gates)
# Bias_ih, Bias_hh: [3*H]
# Embed: [vocab, H]
# FC weight: [vocab, H],  FC bias: [vocab]

state = model.state_dict()

weights_meta = {}

def export(name, tensor):
    q, scale = quantize_tensor(tensor)
    q.tofile(f"weights/{name}.bin")
    weights_meta[name] = {"shape": list(q.shape), "scale": scale}
    print(f"  {name:30s}  shape={q.shape}  scale={scale:.6f}")

print("\nExporting INT8 weights:")
export("embed",         state["embed.weight"])
export("gru_weight_ih", state["gru.weight_ih_l0"])
export("gru_weight_hh", state["gru.weight_hh_l0"])
export("gru_bias_ih",   state["gru.bias_ih_l0"])
export("gru_bias_hh",   state["gru.bias_hh_l0"])
export("fc_weight",     state["fc.weight"])
export("fc_bias",       state["fc.bias"])

# Save vocab and metadata
with open("weights/vocab.json", "w") as f:
    json.dump({"char2idx": char2idx, "idx2char": {str(k): v for k,v in idx2char.items()},
               "vocab_size": vocab_size, "hidden_size": HIDDEN_SIZE}, f, indent=2)

with open("weights/meta.json", "w") as f:
    json.dump(weights_meta, f, indent=2)

print("\nWeights written to ./weights/")
print("Done.")
