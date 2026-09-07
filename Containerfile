# Containerfile for the FPGA-exp open-source toolchain (Tang Nano 20K / Gowin GW2A).
#
# -------------------------------------------------------------------------
# Builds Yosys, nextpnr-himbaechel (gowin uarch) and openFPGALoader from
# source, since the toolchain requires features/architectures newer than
# what Ubuntu 22.04's apt packages ship (see all_projects/project02/Fedora_WSL.md).
# Also sets up an opam switch with dune + hardcaml for the projects that
# generate Verilog from OCaml/Hardcaml.

FROM ubuntu:24.04 AS FPGA-tools-build

ARG YOSYS_REF=yosys-0.38
ARG NEXTPNR_REF=nextpnr-0.11.1
ARG OPENFPGALOADER_REF=v0.12.1
ARG OCAML_VERSION=5.1.1

ENV DEBIAN_FRONTEND=noninteractive \
    TZ=Etc/UTC

# --- system / build dependencies ---------------------------------------
RUN apt-get update && apt-get install -y --no-install-recommends \
        build-essential cmake git pkg-config \
        bison flex gperf \
        libboost-all-dev libeigen3-dev libreadline-dev \
        tcl-dev libffi-dev zlib1g-dev \
        libftdi1-dev libhidapi-dev libudev-dev libusb-1.0-0-dev \
        python3 python3-pip python3-venv \
        opam m4 unzip curl ca-certificates sudo rsync \
    && rm -rf /var/lib/apt/lists/*

# --- Yosys (from source: guarantees a synth_gowin pass with current cell libs) --
# Checkout the release tag *before* initializing submodules: cloning with
# --recurse-submodules up front pulls in whatever the default branch points
# submodules at, which can leave stale/untracked directories (e.g. `abc`)
# once the tag is checked out, breaking the release's own vendoring logic.
RUN git clone https://github.com/YosysHQ/yosys.git /tmp/yosys \
    && cd /tmp/yosys \
    && git checkout ${YOSYS_REF} \
    && git submodule update --init --recursive \
    && make CONFIG=gcc -j"$(nproc)" \
    && make install \
    && rm -rf /tmp/yosys

# --- nextpnr-himbaechel (gowin micro-architecture) ----------------------
# Generating the gowin chipdb binaries needs apycula's device database
# (gowin_arch_gen.py does `from apycula import chipdb`) available to the
# system Python at build time, and HIMBAECHEL_GOWIN_DEVICES defaults to
# empty (no devices at all) unless explicitly requested.
RUN pip3 install --no-cache-dir --break-system-packages apycula
RUN git clone https://github.com/YosysHQ/nextpnr.git /tmp/nextpnr \
    && cd /tmp/nextpnr \
    && git checkout ${NEXTPNR_REF} \
    && git submodule update --init --recursive \
    && mkdir build && cd build \
    && cmake .. -DARCH=himbaechel -DHIMBAECHEL_UARCH=gowin \
        -DHIMBAECHEL_GOWIN_DEVICES="GW2A-18C;GW1NS-4" \
        -DCMAKE_INSTALL_PREFIX=/usr/local \
    && make -j"$(nproc)" \
    && make install \
    && rm -rf /tmp/nextpnr

# --- openFPGALoader (flashing the Tang Nano 20K over USB) ---------------
RUN git clone https://github.com/trabucayre/openFPGALoader.git /tmp/openFPGALoader \
    && cd /tmp/openFPGALoader \
    && git checkout ${OPENFPGALOADER_REF} \
    && mkdir build && cd build \
    && cmake .. -DCMAKE_INSTALL_PREFIX=/usr/local \
    && make -j"$(nproc)" \
    && make install \
    && rm -rf /tmp/openFPGALoader

WORKDIR /tmp

CMD ["/bin/bash"]

# --- FPGA-exp-dev (for building the Hardcaml projects) ----------------------

FROM ubuntu:24.04 AS FPGA-exp-dev

ARG OCAML_VERSION=5.1.1

ENV DEBIAN_FRONTEND=noninteractive \
    TZ=Etc/UTC

# --- system / build dependencies ---------------------------------------
RUN apt-get update && apt-get install -y --no-install-recommends \
        build-essential cmake git pkg-config \
        bison flex gperf \
        libboost-all-dev libeigen3-dev libreadline-dev \
        tcl-dev libffi-dev zlib1g-dev libgmp-dev \
        libftdi1-dev libhidapi-dev libudev-dev libusb-1.0-0-dev \
        python3 python3-pip python3-venv \
        opam m4 unzip curl ca-certificates sudo rsync \
    && rm -rf /var/lib/apt/lists/*

COPY --from=FPGA-tools-build /usr/local/bin/ /usr/local/bin/
COPY --from=FPGA-tools-build /usr/local/share/ /usr/local/share/

# --- dedicated non-root build user --------------------------------------
ARG USERNAME=builder
ARG USER_UID=1000
ARG USER_GID=1000

RUN userdel -r ubuntu 2>/dev/null || true \
    && groupdel ubuntu 2>/dev/null || true \
    && groupadd --gid ${USER_GID} ${USERNAME} \
    && useradd --uid ${USER_UID} --gid ${USER_GID} -m -s /bin/bash ${USERNAME} \
    && groupadd -f plugdev \
    && usermod -aG dialout,plugdev ${USERNAME} \
    && echo "${USERNAME} ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/${USERNAME} \
    && chmod 0440 /etc/sudoers.d/${USERNAME}

USER ${USERNAME}
WORKDIR /home/${USERNAME}

# --- OCaml / dune / hardcaml toolchain (as builder, no root needed) -----
RUN opam init --bare --disable-sandboxing -y \
    && opam switch create default ${OCAML_VERSION} \
    && eval $(opam env) \
    && opam install -y dune core \
        hardcaml ppx_hardcaml hardcaml_waveterm \
        ppx_jane ppx_inline_test ppx_expect \
    && opam clean -a -c -s --logs

ENV PATH=/home/${USERNAME}/.opam/default/bin:$PATH
RUN echo 'eval $(opam env)' >> /home/${USERNAME}/.bashrc

# --- Apycula (provides gowin_pack, used to produce the .fs bitstream) --
RUN pip3 install --no-cache-dir --break-system-packages apycula

# --- FPGA-exp-flash (slim image for flashing over USB, no build toolchain) --

FROM ubuntu:24.04 AS FPGA-exp-flash

ENV DEBIAN_FRONTEND=noninteractive \
    TZ=Etc/UTC

# --- runtime libraries only (openFPGALoader's shared-lib deps) ----------
RUN apt-get update && apt-get install -y --no-install-recommends \
        libftdi1-2 libhidapi-hidraw0 libudev1 libusb-1.0-0 \
        zlib1g libcap2 \
        ca-certificates \
    && rm -rf /var/lib/apt/lists/*

COPY --from=FPGA-tools-build /usr/local/bin/openFPGALoader /usr/local/bin/openFPGALoader

# --- dedicated non-root user (dialout/plugdev for USB device access) ----
ARG USERNAME=builder
ARG USER_UID=1000
ARG USER_GID=1000

RUN userdel -r ubuntu 2>/dev/null || true \
    && groupdel ubuntu 2>/dev/null || true \
    && groupadd --gid ${USER_GID} ${USERNAME} \
    && useradd --uid ${USER_UID} --gid ${USER_GID} -m -s /bin/bash ${USERNAME} \
    && groupadd -f plugdev \
    && usermod -aG dialout,plugdev ${USERNAME}

USER ${USERNAME}
WORKDIR /home/${USERNAME}

CMD ["/bin/bash"]

