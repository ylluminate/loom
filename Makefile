.PHONY: nucleus qemu qemu-test qemu-interactive compile test-kernel \
       test-native test-native-x86 test-native-uefi test-native-all \
       check-erl clean help

# ── Variables ──────────────────────────────────────────────────────────
SRC_DIR = src
NUCLEUS_DIR = $(SRC_DIR)/nucleus
KERNEL_DIR = $(SRC_DIR)/kernel
BEAM_VM_DIR = $(SRC_DIR)/beam_vm
COMPAT_DIR = $(SRC_DIR)/compat
NATIVE_DIR = $(SRC_DIR)/native
EBIN_DIR = ebin

# ── Default ────────────────────────────────────────────────────────────
help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

# ── OS Nucleus ─────────────────────────────────────────────────────────
nucleus: ## Build nucleus.efi (UEFI bootable)
	@echo "Building Loom OS nucleus..."
	@mkdir -p $(EBIN_DIR)
	@for f in $(NUCLEUS_DIR)/*.erl $(NATIVE_DIR)/vbeam_native_pe.erl; do \
		[ -f "$$f" ] && erlc -o $(EBIN_DIR) "$$f" 2>/dev/null; \
	done
	@erl -noshell -pa $(EBIN_DIR) \
		-eval 'vbeam_nucleus_boot:build("$(NUCLEUS_DIR)/nucleus.efi")' -s init stop
	@echo "Built: $(NUCLEUS_DIR)/nucleus.efi"
	@ls -la $(NUCLEUS_DIR)/nucleus.efi

qemu: qemu-test ## Boot nucleus in QEMU (alias)

qemu-test: ## Boot nucleus in QEMU and verify serial output
	./scripts/test_nucleus_qemu.sh

qemu-interactive: ## Boot nucleus in QEMU with interactive serial console
	./scripts/test_nucleus_qemu.sh --interactive

# ── Compile ────────────────────────────────────────────────────────────
compile: ## Compile all Erlang source modules
	@mkdir -p $(EBIN_DIR)
	@for dir in $(NUCLEUS_DIR) $(BEAM_VM_DIR) $(KERNEL_DIR) $(COMPAT_DIR) $(NATIVE_DIR); do \
		for f in $$dir/*.erl; do \
			[ -f "$$f" ] && erlc -o $(EBIN_DIR) "$$f" 2>/dev/null; \
		done; \
	done
	@echo "All modules compiled to $(EBIN_DIR)/"
	@ls $(EBIN_DIR)/*.beam 2>/dev/null | wc -l | xargs -I{} echo "{} beam files"

# ── Tests ──────────────────────────────────────────────────────────────
test-kernel: ## Run kernel module tests
	@mkdir -p $(EBIN_DIR)
	@for f in $(KERNEL_DIR)/*_test.erl; do \
		[ -f "$$f" ] || continue; \
		mod=$$(basename "$$f" .erl); \
		echo "Testing $$mod..."; \
		erlc -o $(EBIN_DIR) "$$f" && \
		erlc -o $(EBIN_DIR) $(KERNEL_DIR)/$$(echo $$mod | sed 's/_test$$//').erl 2>/dev/null && \
		erl -noshell -pa $(EBIN_DIR) -eval "$$mod:test()." -s init stop || echo "FAIL: $$mod"; \
	done

test-native: ## Run native ARM64 tests
	./scripts/test_native_arm64.sh

test-native-x86: ## Run native x86_64 ELF tests (via Docker/QEMU)
	./scripts/test_native_x86_64.sh

test-native-uefi: ## Run native x86_64 UEFI PE tests (via QEMU)
	./scripts/test_native_x86_64_uefi.sh

test-native-all: test-native test-native-x86 ## Run native tests on all platforms

# ── Quality ────────────────────────────────────────────────────────────
check-erl: ## Syntax-check all Erlang source modules
	@for dir in $(NUCLEUS_DIR) $(BEAM_VM_DIR) $(KERNEL_DIR) $(COMPAT_DIR) $(NATIVE_DIR); do \
		for f in $$dir/*.erl; do \
			[ -f "$$f" ] || continue; \
			echo "Checking $$f..."; \
			erlc -Wall -o /tmp "$$f" || exit 1; \
		done; \
	done
	@echo "All Erlang files OK"

# ── Housekeeping ───────────────────────────────────────────────────────
clean: ## Clean build artifacts
	rm -rf $(EBIN_DIR)
	rm -f $(NUCLEUS_DIR)/nucleus.efi
	rm -f $(NUCLEUS_DIR)/*.beam
	rm -f $(KERNEL_DIR)/*.beam
	rm -f $(BEAM_VM_DIR)/*.beam
	rm -f $(COMPAT_DIR)/*.beam
	rm -f $(NATIVE_DIR)/*.beam
	rm -rf /tmp/vbeam_*
