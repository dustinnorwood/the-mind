# ==============================================================================
# Build stage (shared for backend + frontend)
# ==============================================================================
FROM ubuntu:22.04 AS builder

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y \
    curl gcc g++ make libgmp-dev libtinfo-dev zlib1g-dev \
    libffi-dev libncurses-dev pkg-config ca-certificates git gnupg \
    && curl -sSL https://get.haskellstack.org/ | sh \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy dependency files first for Docker layer caching
COPY stack.yaml ./
COPY common/common.cabal common/
COPY backend/backend.cabal backend/
COPY frontend/frontend.cabal frontend/

# Setup GHC (cached layer)
RUN stack setup

# Build dependencies only (cached layer)
RUN stack build --only-dependencies

# Copy source code
COPY common/ common/
COPY backend/ backend/
COPY frontend/ frontend/

# Build all packages and install binaries
RUN stack build && stack install --local-bin-path /opt/bin

# ==============================================================================
# Backend runtime
# ==============================================================================
FROM ubuntu:22.04 AS backend-runtime

RUN apt-get update && apt-get install -y \
    libgmp10 libtinfo6 ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=builder /opt/bin/backend /usr/local/bin/
COPY static/ ./static/

EXPOSE 8000
CMD ["backend"]

# ==============================================================================
# Frontend runtime
# ==============================================================================
FROM ubuntu:22.04 AS frontend-runtime

RUN apt-get update && apt-get install -y \
    libgmp10 libtinfo6 ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=builder /opt/bin/frontend /usr/local/bin/

EXPOSE 3003
CMD ["frontend"]
