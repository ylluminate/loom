# ╔══════════════════════════════════════════════════════════════════════╗
# ║  Loom OS — Build System                                            ║
# ║  An operating system where BEAM is the kernel                      ║
# ╚══════════════════════════════════════════════════════════════════════╝
#
# Usage:
#   make              Show available targets
#   make all          Build everything
#   make nucleus      Build UEFI bootable nucleus.efi
#   make test         Run all tests
#   make clean        Remove build artifacts
#
# Variables (override with make VAR=value):
#   ERLC             Erlang compiler (default: erlc)
#   ERL              Erlang runtime (default: erl)
#   EBIN             Output directory for .beam files (default: _build/)
#   VERBOSE          Show commands being run (default: 0)
#   JOBS             Parallel compilation jobs (default: $(nproc))

# ── Configuration ─────────────────────────────────────────────────────

ERLC     ?= erlc
ERL      ?= erl
EBIN     := _build
VERBOSE  ?= 0

# Silence unless VERBOSE=1
ifeq ($(VERBOSE),1)
  Q :=
  ERLC_FLAGS := -Wall
else
  Q := @
  ERLC_FLAGS := -Wall
endif

# Colors (disabled if NO_COLOR is set, per https://no-color.org/)
ifdef NO_COLOR
  _R :=
  _G :=
  _Y :=
  _B :=
  _C :=
  _D :=
  _N :=
else
  _R := \033[0;31m
  _G := \033[0;32m
  _Y := \033[0;33m
  _B := \033[0;34m
  _C := \033[0;36m
  _D := \033[0;90m
  _N := \033[0m
endif

# ── Source Discovery ──────────────────────────────────────────────────
# Automatically find all .erl files in the source tree

BOOT_SRC    := $(wildcard boot/*.erl boot/fonts/*.erl)
KERNEL_SRC  := $(wildcard kernel/boot/*.erl kernel/mm/*.erl kernel/sched/*.erl \
                          kernel/io/*.erl kernel/arch/*.erl)
VM_SRC      := $(wildcard vm/interp/*.erl vm/parser/*.erl vm/jit/*.erl)
ARCH_SRC    := $(wildcard arch/x86_64/*.erl arch/arm64/*.erl arch/ir/*.erl \
                          arch/link/*.erl arch/formats/*.erl)
COMPAT_SRC  := $(wildcard compat/syscall/*.erl compat/elf/*.erl compat/kpi/*.erl)

ALL_SRC     := $(BOOT_SRC) $(KERNEL_SRC) $(VM_SRC) $(ARCH_SRC) $(COMPAT_SRC)
ALL_BEAM    := $(patsubst %.erl,$(EBIN)/%.beam,$(notdir $(ALL_SRC)))

# Test sources (separate from main build)
TEST_SRC    := $(wildcard tests/kernel/*.erl tests/vm/*.erl)

# ── Phony Targets ─────────────────────────────────────────────────────

.PHONY: all help \
        compile boot-compile kernel-compile vm-compile arch-compile compat-compile \
        nucleus qemu qemu-test qemu-interactive \
        test test-kernel test-vm test-native test-native-x86 test-native-uefi \
        check lint \
        clean distclean \
        info

# ── Default Target ────────────────────────────────────────────────────

help: ## Show available targets
	@echo ""
	@echo "  $(_B)Loom OS$(_N) — Build System"
	@echo "  $(_D)An operating system where BEAM is the kernel$(_N)"
	@echo ""
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "  $(_C)%-20s$(_N) %s\n", $$1, $$2}'
	@echo ""

# ── Build ─────────────────────────────────────────────────────────────

all: compile nucleus ## Build everything

$(EBIN):
	$(Q)mkdir -p $(EBIN)

# Master compile target — builds all subsystems
compile: $(EBIN) boot-compile kernel-compile vm-compile arch-compile compat-compile ## Compile all Erlang modules
	@printf "  $(_G)OK$(_N)  %s beam files in $(EBIN)/\n" "$$(ls $(EBIN)/*.beam 2>/dev/null | wc -l | tr -d ' ')"

# Per-subsystem compile targets
boot-compile: $(EBIN) ## Compile boot subsystem
	$(Q)for f in $(BOOT_SRC); do \
		mod=$$(basename "$$f" .erl); \
		$(ERLC) $(ERLC_FLAGS) -o $(EBIN) "$$f" 2>/dev/null && \
			printf "  $(_G)OK$(_N)  $$mod\n" || \
			printf "  $(_R)FAIL$(_N)  $$mod\n"; \
	done

kernel-compile: $(EBIN) ## Compile kernel subsystem
	$(Q)for f in $(KERNEL_SRC); do \
		mod=$$(basename "$$f" .erl); \
		$(ERLC) $(ERLC_FLAGS) -o $(EBIN) "$$f" 2>/dev/null && \
			printf "  $(_G)OK$(_N)  $$mod\n" || \
			printf "  $(_R)FAIL$(_N)  $$mod\n"; \
	done

vm-compile: $(EBIN) ## Compile VM subsystem
	$(Q)for f in $(VM_SRC); do \
		mod=$$(basename "$$f" .erl); \
		$(ERLC) $(ERLC_FLAGS) -o $(EBIN) "$$f" 2>/dev/null && \
			printf "  $(_G)OK$(_N)  $$mod\n" || \
			printf "  $(_R)FAIL$(_N)  $$mod\n"; \
	done

arch-compile: $(EBIN) ## Compile architecture backends
	$(Q)for f in $(ARCH_SRC); do \
		mod=$$(basename "$$f" .erl); \
		$(ERLC) $(ERLC_FLAGS) -o $(EBIN) "$$f" 2>/dev/null && \
			printf "  $(_G)OK$(_N)  $$mod\n" || \
			printf "  $(_R)FAIL$(_N)  $$mod\n"; \
	done

compat-compile: $(EBIN) ## Compile compatibility layer
	$(Q)for f in $(COMPAT_SRC); do \
		mod=$$(basename "$$f" .erl); \
		$(ERLC) $(ERLC_FLAGS) -o $(EBIN) "$$f" 2>/dev/null && \
			printf "  $(_G)OK$(_N)  $$mod\n" || \
			printf "  $(_R)FAIL$(_N)  $$mod\n"; \
	done

# ── Nucleus (UEFI Boot) ──────────────────────────────────────────────

nucleus: boot-compile arch-compile ## Build UEFI bootable nucleus.efi
	@printf "\n  $(_B)Building nucleus.efi...$(_N)\n"
	$(Q)$(ERL) -noshell -pa $(EBIN) \
		-eval 'vbeam_nucleus_boot:build("boot/nucleus.efi")' -s init stop
	@printf "  $(_G)OK$(_N)  boot/nucleus.efi ($$(wc -c < boot/nucleus.efi | tr -d ' ') bytes)\n"

qemu: qemu-test ## Boot in QEMU (alias)

qemu-test: nucleus ## Boot nucleus in QEMU, verify serial output
	$(Q)./tools/test_nucleus_qemu.escript

qemu-interactive: nucleus ## Boot nucleus in QEMU with interactive console
	$(Q)./tools/test_nucleus_qemu.escript --interactive

# ── Tests ─────────────────────────────────────────────────────────────

test: test-kernel test-vm ## Run all test suites

test-kernel: compile ## Run kernel unit tests
	@printf "\n  $(_B)Kernel Tests$(_N)\n"
	$(Q)for f in tests/kernel/*_test.erl; do \
		[ -f "$$f" ] || continue; \
		mod=$$(basename "$$f" .erl); \
		$(ERLC) $(ERLC_FLAGS) -o $(EBIN) "$$f" 2>/dev/null; \
		if $(ERL) -noshell -pa $(EBIN) -eval "case $$mod:test() of ok -> halt(0); _ -> halt(1) end" 2>/dev/null; then \
			printf "  $(_G)PASS$(_N)  $$mod\n"; \
		else \
			printf "  $(_R)FAIL$(_N)  $$mod\n"; \
		fi; \
	done

test-vm: compile ## Run VM unit tests
	@printf "\n  $(_B)VM Tests$(_N)\n"
	$(Q)for f in tests/vm/*.erl; do \
		[ -f "$$f" ] || continue; \
		mod=$$(basename "$$f" .erl); \
		$(ERLC) $(ERLC_FLAGS) -o $(EBIN) "$$f" 2>/dev/null; \
		if $(ERL) -noshell -pa $(EBIN) -eval "case $$mod:test() of ok -> halt(0); _ -> halt(1) end" 2>/dev/null; then \
			printf "  $(_G)PASS$(_N)  $$mod\n"; \
		else \
			printf "  $(_R)FAIL$(_N)  $$mod\n"; \
		fi; \
	done

test-native: ## Run native ARM64 tests
	$(Q)./tools/test_native.escript --arch=arm64

test-native-x86: ## Run native x86_64 ELF tests (Docker/QEMU)
	$(Q)./tools/test_native.escript --arch=x86_64

test-native-uefi: ## Run native x86_64 UEFI PE tests (QEMU)
	$(Q)./tools/test_native.escript --arch=uefi

# ── Quality ───────────────────────────────────────────────────────────

check: $(EBIN) ## Syntax-check all source modules (strict)
	@printf "\n  $(_B)Checking all modules...$(_N)\n"
	$(Q)PASS=0; FAIL=0; \
	for f in $(ALL_SRC); do \
		mod=$$(basename "$$f" .erl); \
		if $(ERLC) -Wall -Werror -o /tmp "$$f" 2>/dev/null; then \
			PASS=$$((PASS + 1)); \
		else \
			printf "  $(_R)FAIL$(_N)  $$f\n"; \
			FAIL=$$((FAIL + 1)); \
		fi; \
	done; \
	printf "  $(_G)$$PASS passed$(_N)"; \
	if [ $$FAIL -gt 0 ]; then printf ", $(_R)$$FAIL failed$(_N)"; fi; \
	printf "\n"; \
	test $$FAIL -eq 0

lint: check ## Alias for check

# ── Info ──────────────────────────────────────────────────────────────

info: ## Show project statistics
	@echo ""
	@echo "  $(_B)Loom OS — Project Info$(_N)"
	@echo ""
	@printf "  $(_C)%-20s$(_N) %s\n" "Boot modules:" "$$(echo $(BOOT_SRC) | wc -w | tr -d ' ')"
	@printf "  $(_C)%-20s$(_N) %s\n" "Kernel modules:" "$$(echo $(KERNEL_SRC) | wc -w | tr -d ' ')"
	@printf "  $(_C)%-20s$(_N) %s\n" "VM modules:" "$$(echo $(VM_SRC) | wc -w | tr -d ' ')"
	@printf "  $(_C)%-20s$(_N) %s\n" "Arch modules:" "$$(echo $(ARCH_SRC) | wc -w | tr -d ' ')"
	@printf "  $(_C)%-20s$(_N) %s\n" "Compat modules:" "$$(echo $(COMPAT_SRC) | wc -w | tr -d ' ')"
	@printf "  $(_C)%-20s$(_N) %s\n" "Total source:" "$$(echo $(ALL_SRC) | wc -w | tr -d ' ')"
	@printf "  $(_C)%-20s$(_N) %s\n" "Test modules:" "$$(echo $(TEST_SRC) | wc -w | tr -d ' ')"
	@printf "  $(_C)%-20s$(_N) %s\n" "Native tests:" "$$(find tests/native -name '*.v' 2>/dev/null | wc -l | tr -d ' ')"
	@echo ""
	@if [ -f boot/nucleus.efi ]; then \
		printf "  $(_G)nucleus.efi$(_N)  %s bytes\n" "$$(wc -c < boot/nucleus.efi | tr -d ' ')"; \
	else \
		printf "  $(_Y)nucleus.efi$(_N)  not built (run: make nucleus)\n"; \
	fi
	@if [ -d $(EBIN) ]; then \
		printf "  $(_G)$(EBIN)/$(_N)        %s beam files\n" "$$(ls $(EBIN)/*.beam 2>/dev/null | wc -l | tr -d ' ')"; \
	else \
		printf "  $(_Y)$(EBIN)/$(_N)        not built (run: make compile)\n"; \
	fi
	@echo ""

# ── Housekeeping ──────────────────────────────────────────────────────

clean: ## Remove build artifacts (_build/, nucleus.efi)
	$(Q)rm -rf $(EBIN)
	$(Q)rm -f boot/nucleus.efi
	$(Q)rm -f erl_crash.dump
	@printf "  $(_D)Cleaned build artifacts$(_N)\n"

distclean: clean ## Remove everything (build + QEMU images + temp files)
	$(Q)rm -rf /tmp/vbeam_*
	$(Q)rm -f boot/esp/EFI/BOOT/BOOTX64.EFI
	$(Q)find . -name "erl_crash.dump" -delete 2>/dev/null
	@printf "  $(_D)Cleaned all generated files$(_N)\n"
