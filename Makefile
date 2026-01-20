.DEFAULT_GOAL := all
.PHONY: all build clean reset test show-config rebuild remake docs \
		psnd-tsf default psnd-tsf-csound csound psnd-fluid psnd-fluid-csound \
		psnd-tsf-web web psnd-fluid-web psnd-fluid-csound-web full \
		mhs-small mhs-src mhs-src-small no-mhs

BUILD_DIR ?= build
CMAKE ?= cmake

all: build

# ============================================================================
# Configure targets
# ============================================================================

configure-tsf:
	@mkdir -p $(BUILD_DIR) && $(CMAKE) -S . -B $(BUILD_DIR) -DBUILD_TESTING=ON

configure-tsf-csound:
	@mkdir -p $(BUILD_DIR) && $(CMAKE) -S . -B $(BUILD_DIR) -DBUILD_CSOUND_BACKEND=ON -DBUILD_TESTING=ON

configure-fluid:
	@mkdir -p $(BUILD_DIR) && $(CMAKE) -S . -B $(BUILD_DIR) -DBUILD_FLUID_BACKEND=ON -DBUILD_TESTING=ON

configure-fluid-csound:
	@mkdir -p $(BUILD_DIR) && $(CMAKE) -S . -B $(BUILD_DIR) -DBUILD_FLUID_BACKEND=ON -DBUILD_CSOUND_BACKEND=ON -DBUILD_TESTING=ON

configure-tsf-web:
	@mkdir -p $(BUILD_DIR) && $(CMAKE) -S . -B $(BUILD_DIR) -DBUILD_WEB_HOST=ON -DBUILD_TESTING=ON

configure-fluid-web:
	@mkdir -p $(BUILD_DIR) && $(CMAKE) -S . -B $(BUILD_DIR) -DBUILD_FLUID_BACKEND=ON -DBUILD_WEB_HOST=ON -DBUILD_TESTING=ON

configure-fluid-csound-web:
	@mkdir -p $(BUILD_DIR) && $(CMAKE) -S . -B $(BUILD_DIR) -DBUILD_FLUID_BACKEND=ON -DBUILD_CSOUND_BACKEND=ON -DBUILD_WEB_HOST=ON -DBUILD_TESTING=ON

# MHS variants
configure-mhs-small:
	@mkdir -p $(BUILD_DIR) && $(CMAKE) -S . -B $(BUILD_DIR) -DMHS_ENABLE_COMPILATION=OFF -DBUILD_TESTING=ON

configure-mhs-src:
	@mkdir -p $(BUILD_DIR) && $(CMAKE) -S . -B $(BUILD_DIR) -DMHS_EMBED_MODE=SRC_ZSTD -DBUILD_TESTING=ON

configure-mhs-src-small:
	@mkdir -p $(BUILD_DIR) && $(CMAKE) -S . -B $(BUILD_DIR) -DMHS_EMBED_MODE=SRC_ZSTD -DMHS_ENABLE_COMPILATION=OFF -DBUILD_TESTING=ON

configure-no-mhs:
	@mkdir -p $(BUILD_DIR) && $(CMAKE) -S . -B $(BUILD_DIR) -DENABLE_MHS_INTEGRATION=OFF -DBUILD_TESTING=ON

# ============================================================================
# Build presets
# ============================================================================

# TinySoundFont only
psnd-tsf: configure-tsf
	@$(CMAKE) --build $(BUILD_DIR) --config Release

default: psnd-tsf  # alias
build: psnd-tsf    # alias

# TinySoundFont + Csound
psnd-tsf-csound: configure-tsf-csound
	@$(CMAKE) --build $(BUILD_DIR) --config Release

csound: psnd-tsf-csound  # alias

# FluidSynth only
psnd-fluid: configure-fluid
	@$(CMAKE) --build $(BUILD_DIR) --config Release

# FluidSynth + Csound
psnd-fluid-csound: configure-fluid-csound
	@$(CMAKE) --build $(BUILD_DIR) --config Release

# TinySoundFont + Web
psnd-tsf-web: configure-tsf-web
	@$(CMAKE) --build $(BUILD_DIR) --config Release

web: psnd-tsf-web  # alias

# FluidSynth + Web
psnd-fluid-web: configure-fluid-web
	@$(CMAKE) --build $(BUILD_DIR) --config Release

# FluidSynth + Csound + Web (everything)
psnd-fluid-csound-web: configure-fluid-csound-web
	@$(CMAKE) --build $(BUILD_DIR) --config Release

full: psnd-fluid-csound-web  # alias

# ============================================================================
# MHS build variants
# ============================================================================

# MHS without compilation support (~4.5MB binary, no -o executable output)
mhs-small: configure-mhs-small
	@$(CMAKE) --build $(BUILD_DIR) --config Release

# MHS with source embedding (~17s startup, ~4MB binary with compilation)
mhs-src: configure-mhs-src
	@$(CMAKE) --build $(BUILD_DIR) --config Release

# MHS source mode without compilation (smallest MHS binary, ~17s startup)
mhs-src-small: configure-mhs-src-small
	@$(CMAKE) --build $(BUILD_DIR) --config Release

# Disable MHS entirely
no-mhs: configure-no-mhs
	@$(CMAKE) --build $(BUILD_DIR) --config Release

# ============================================================================

rebuild: clean psnd-tsf-csound test

library: configure-tsf
	@$(CMAKE) --build $(BUILD_DIR) --target libloki --config Release

# Primary target: unified psnd binary
psnd: configure-tsf
	@$(CMAKE) --build $(BUILD_DIR) --target psnd_bin --config Release

show-config: configure-tsf
	@$(CMAKE) --build $(BUILD_DIR) --target show-config --config Release

test:
	@$(CMAKE) -E chdir $(BUILD_DIR) ctest --output-on-failure

clean:
	@$(CMAKE) --build $(BUILD_DIR) --target clean 2>/dev/null || true

reset:
	@$(CMAKE) -E rm -rf $(BUILD_DIR)

remake: reset build

# Generate architecture diagrams from D2 sources
docs:
	@command -v d2 >/dev/null 2>&1 || { echo "d2 not found. Install from https://d2lang.com"; exit 1; }
	@echo "Generating architecture diagrams..."
	d2 docs/arch-highlevel.d2 docs/arch-highlevel.svg
