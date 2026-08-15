# Containerfile for the FPGA-exp open-source toolchain (Tang Nano 20K / Gowin GW2A).
#
# Builds Yosys, nextpnr-himbaechel (gowin uarch) and openFPGALoader from
# source, since the toolchain requires features/architectures newer than
# what Ubuntu 22.04's apt packages ship (see all_projects/project02/Fedora_WSL.md).
# Also sets up an opam switch with dune + hardcaml for the projects that
# generate Verilog from OCaml/Hardcaml.

FROM ubuntu:22.04

ARG YOSYS_REF=yosys-0.38
ARG NEXTPNR_REF=nextpnr-0.7
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
        gtkwave verilator \
    && rm -rf /var/lib/apt/lists/*

# --- Yosys (from source: guarantees a synth_gowin pass with current cell libs) --
RUN git clone --recurse-submodules https://github.com/YosysHQ/yosys.git /tmp/yosys \
    && cd /tmp/yosys \
    && git checkout ${YOSYS_REF} \
    && git submodule update --init --recursive \
    && make -j"$(nproc)" \
    && make install \
    && rm -rf /tmp/yosys

# --- nextpnr-himbaechel (gowin micro-architecture) ----------------------
RUN git clone --recurse-submodules https://github.com/YosysHQ/nextpnr.git /tmp/nextpnr \
    && cd /tmp/nextpnr \
    && git checkout ${NEXTPNR_REF} \
    && git submodule update --init --recursive \
    && mkdir build && cd build \
    && cmake .. -DARCH=himbaechel -DHIMBAECHEL_UARCH=gowin -DCMAKE_INSTALL_PREFIX=/usr/local \
    && make -j"$(nproc)" \
    && make install \
    && rm -rf /tmp/nextpnr

# --- Apycula (provides gowin_pack, used to produce the .fs bitstream) --
RUN pip3 install --no-cache-dir apycula

# --- openFPGALoader (flashing the Tang Nano 20K over USB) ---------------
RUN git clone https://github.com/trabucayre/openFPGALoader.git /tmp/openFPGALoader \
    && cd /tmp/openFPGALoader \
    && git checkout ${OPENFPGALOADER_REF} \
    && mkdir build && cd build \
    && cmake .. -DCMAKE_INSTALL_PREFIX=/usr/local \
    && make -j"$(nproc)" \
    && make install \
    && rm -rf /tmp/openFPGALoader

# --- dedicated non-root build user --------------------------------------
ARG USERNAME=builder
ARG USER_UID=1000
ARG USER_GID=1000

RUN groupadd --gid ${USER_GID} ${USERNAME} \
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
        hardcaml hardcaml_waveterm \
        ppx_jane ppx_inline_test ppx_expect \
    && opam clean -a -c -s --logs

ENV PATH=/home/builder/.opam/default/bin:$PATH
RUN echo 'eval $(opam env)' >> /home/builder/.bashrc

CMD ["/bin/bash"]
