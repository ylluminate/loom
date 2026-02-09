#!/bin/bash
# V-to-Native x86_64 UEFI Test Harness
# Tests V programs through the native x86_64 PE pipeline using QEMU + OVMF
#
# Pipeline: V -> IR -> PE (UEFI) -> QEMU boot -> capture serial output
#
# Usage:
#   ./scripts/test_native_x86_64_uefi.sh                    # Run all tests
#   ./scripts/test_native_x86_64_uefi.sh tests/native/      # Specific directory
#   ./scripts/test_native_x86_64_uefi.sh hello.v             # Specific file
#   ./scripts/test_native_x86_64_uefi.sh --verbose           # Verbose output
#
# Prerequisites:
#   - qemu-system-x86_64 (QEMU full-system emulator)
#   - OVMF firmware (UEFI firmware for QEMU)
#   - Erlang/OTP (for vbeam_native compiler)
#
# Test case format:
#   foo.v          - V source file
#   foo.expected   - Expected serial output
#
# Exit codes:
#   0 - All tests passed
#   1 - Some tests failed
#   2 - Configuration error (missing prerequisites)

set -o pipefail

# === Configuration ===
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
V_COMPILER="/Users/u/tank/ops/tools/dev/vlang/v"
DEFAULT_TEST_DIR="$PROJECT_ROOT/tests/native"
WORK_DIR="$PROJECT_ROOT/beam_output/native_uefi_tests"
VBEAM_RT_DIR="$PROJECT_ROOT/vbeam_rt"
TIMEOUT_SEC=15

# OVMF firmware search paths (common install locations)
OVMF_SEARCH_PATHS=(
    "/usr/share/OVMF/OVMF_CODE.fd"
    "/usr/share/edk2/ovmf/OVMF_CODE.fd"
    "/usr/share/qemu/OVMF.fd"
    "/usr/share/ovmf/OVMF.fd"
    "/opt/homebrew/share/qemu/edk2-x86_64-code.fd"
    "/usr/local/share/qemu/edk2-x86_64-code.fd"
    "$HOME/.local/share/qemu/OVMF.fd"
    "$PROJECT_ROOT/tools/OVMF.fd"
)

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Options
VERBOSE=0
KEEP_ARTIFACTS=0
OVMF_PATH=""

# Counters
TOTAL=0
PASSED=0
FAILED=0
COMPILE_ERRORS=0
IR_ERRORS=0
LINK_ERRORS=0
RUNTIME_ERRORS=0

# Arrays for tracking failures
declare -a FAILED_TESTS

# === Functions ===

usage() {
    echo "Usage: $0 [OPTIONS] [test_path...]"
    echo ""
    echo "Options:"
    echo "  -v, --verbose          Show detailed output"
    echo "  -k, --keep             Keep intermediate artifacts"
    echo "  --ovmf PATH            Specify OVMF firmware path"
    echo "  --timeout SECONDS      Per-test timeout (default: $TIMEOUT_SEC)"
    echo "  -h, --help             Show this help"
    echo ""
    echo "Arguments:"
    echo "  test_path              Directory of tests or specific .v file(s)"
    echo "                         Default: $DEFAULT_TEST_DIR"
    echo ""
    echo "Prerequisites:"
    echo "  qemu-system-x86_64     Full-system x86_64 emulator"
    echo "  OVMF firmware          UEFI firmware for QEMU"
    echo ""
    echo "Install QEMU:"
    echo "  macOS:   brew install qemu"
    echo "  Ubuntu:  sudo apt install qemu-system-x86 ovmf"
    echo "  Fedora:  sudo dnf install qemu-system-x86 edk2-ovmf"
    exit 0
}

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_pass() {
    echo -e "${GREEN}[PASS]${NC} $1"
}

log_fail() {
    echo -e "${RED}[FAIL]${NC} $1"
}

log_skip() {
    echo -e "${YELLOW}[SKIP]${NC} $1"
}

log_verbose() {
    if [[ $VERBOSE -eq 1 ]]; then
        echo -e "${CYAN}       ${NC}$1"
    fi
}

# Check prerequisites
check_prerequisites() {
    local missing=0

    # Check qemu-system-x86_64
    if ! command -v qemu-system-x86_64 &>/dev/null; then
        echo -e "${RED}Error:${NC} qemu-system-x86_64 not found"
        echo "  Install:"
        echo "    macOS:   brew install qemu"
        echo "    Ubuntu:  sudo apt install qemu-system-x86"
        echo "    Fedora:  sudo dnf install qemu-system-x86"
        missing=1
    fi

    # Check OVMF firmware
    if [[ -n "$OVMF_PATH" ]]; then
        if [[ ! -f "$OVMF_PATH" ]]; then
            echo -e "${RED}Error:${NC} OVMF firmware not found at specified path: $OVMF_PATH"
            missing=1
        fi
    else
        OVMF_PATH=""
        for path in "${OVMF_SEARCH_PATHS[@]}"; do
            if [[ -f "$path" ]]; then
                OVMF_PATH="$path"
                break
            fi
        done
        if [[ -z "$OVMF_PATH" ]]; then
            echo -e "${RED}Error:${NC} OVMF firmware not found"
            echo "  Searched:"
            for path in "${OVMF_SEARCH_PATHS[@]}"; do
                echo "    $path"
            done
            echo ""
            echo "  Install:"
            echo "    macOS:   brew install qemu   (includes OVMF)"
            echo "    Ubuntu:  sudo apt install ovmf"
            echo "    Fedora:  sudo dnf install edk2-ovmf"
            echo "    Manual:  Download from https://github.com/tianocore/edk2/releases"
            echo "             and place as: $PROJECT_ROOT/tools/OVMF.fd"
            echo ""
            echo "  Or specify path: $0 --ovmf /path/to/OVMF.fd"
            missing=1
        fi
    fi

    # Check V compiler
    if [[ ! -x "$V_COMPILER" ]]; then
        echo -e "${RED}Error:${NC} V compiler not found at $V_COMPILER"
        missing=1
    fi

    # Check Erlang
    if ! command -v erl &>/dev/null; then
        echo -e "${RED}Error:${NC} Erlang/OTP not found"
        missing=1
    fi

    if [[ $missing -ne 0 ]]; then
        exit 2
    fi
}

# Compile V source to PE UEFI binary
compile_uefi() {
    local v_file="$1"
    local work_dir="$2"
    local test_name
    test_name=$(basename "$v_file" .v)
    local v_dir
    v_dir=$(dirname "$v_file")
    local beam_out_dir="$v_dir/${test_name}.beam"

    # Clean any previous output
    rm -rf "$beam_out_dir"

    # Step 1: Compile V to native IR (x86_64, PE format)
    log_verbose "  V -> Native IR (target=x86_64, format=pe)..."
    local v_output
    v_output=$(VBEAM_TARGET=x86_64 VBEAM_FORMAT=pe "$V_COMPILER" -b beam "$v_file" 2>&1)
    local v_exit=$?

    if [[ $v_exit -ne 0 ]]; then
        echo "$v_output"
        rm -rf "$beam_out_dir"
        return 1
    fi

    # Check that IR file was generated
    local ir_file="$beam_out_dir/.vbeam_native.ir"
    if [[ ! -f "$ir_file" ]]; then
        echo "V compiler did not generate native IR file: $ir_file"
        echo "V output: $v_output"
        rm -rf "$beam_out_dir"
        return 2
    fi

    # Step 2: Compile IR to PE binary using vbeam_native
    log_verbose "  Native IR -> PE binary..."
    local out_file="$work_dir/BOOTX64.EFI"

    # Build BEAM modules path
    local ebin_dir="$VBEAM_RT_DIR/_build/default/lib/vbeam_rt/ebin"
    if [[ ! -d "$ebin_dir" ]]; then
        ebin_dir="$VBEAM_RT_DIR/ebin"
    fi

    local compile_output
    compile_output=$(erl -noshell \
        -pa "$ebin_dir" \
        -eval "vbeam_native:main([\"$ir_file\", \"-o\", \"$out_file\", \"-format\", \"pe\"]), init:stop()." \
        2>&1)
    local compile_exit=$?

    # Clean up V compiler output
    rm -rf "$beam_out_dir"

    if [[ $compile_exit -ne 0 ]]; then
        echo "PE compilation failed: $compile_output"
        return 3
    fi

    if [[ ! -f "$out_file" ]]; then
        echo "PE compiler did not produce output: $out_file"
        echo "Compiler output: $compile_output"
        return 3
    fi

    log_verbose "  PE binary produced: $(wc -c < "$out_file") bytes"
    return 0
}

# Create EFI boot structure for QEMU
create_efi_structure() {
    local pe_file="$1"
    local efi_dir="$2"

    mkdir -p "$efi_dir/EFI/BOOT"
    cp "$pe_file" "$efi_dir/EFI/BOOT/BOOTX64.EFI"
}

# Run UEFI binary in QEMU and capture serial output
run_uefi_qemu() {
    local efi_dir="$1"
    local timeout_sec="${2:-$TIMEOUT_SEC}"

    # Create a FAT disk image from the EFI directory
    local disk_img="$efi_dir/disk.img"

    # Calculate image size (at least 2880 sectors = 1.44MB floppy, or bigger)
    local efi_size
    efi_size=$(du -sk "$efi_dir/EFI" 2>/dev/null | cut -f1)
    local img_size_kb=$(( (efi_size + 512) * 2 ))
    if [[ $img_size_kb -lt 2880 ]]; then
        img_size_kb=2880
    fi

    # Method 1: Use mformat (mtools) to create FAT image
    if command -v mformat &>/dev/null; then
        dd if=/dev/zero of="$disk_img" bs=1024 count=$img_size_kb 2>/dev/null
        mformat -i "$disk_img" -F ::
        mmd -i "$disk_img" ::/EFI
        mmd -i "$disk_img" ::/EFI/BOOT
        mcopy -i "$disk_img" "$efi_dir/EFI/BOOT/BOOTX64.EFI" ::/EFI/BOOT/BOOTX64.EFI
    else
        # Method 2: Use QEMU's built-in FAT directory support
        # This is simpler and doesn't require mtools
        disk_img="fat:rw:$efi_dir"
    fi

    # Run QEMU with OVMF, capturing serial output
    local serial_output
    serial_output=$(timeout "$timeout_sec" \
        qemu-system-x86_64 \
            -bios "$OVMF_PATH" \
            -drive "format=raw,file=$disk_img" \
            -serial stdio \
            -nographic \
            -no-reboot \
            -monitor none \
            -m 256M \
            2>/dev/null)
    local qemu_exit=$?

    # Clean up disk image if we created one
    if [[ -f "$efi_dir/disk.img" ]]; then
        rm -f "$efi_dir/disk.img"
    fi

    echo "$serial_output"
    return $qemu_exit
}

# Filter UEFI/QEMU noise from serial output
# UEFI firmware may emit extra text before our program's output
filter_serial_output() {
    local raw_output="$1"

    # UEFI firmware often outputs boot messages, shell prompts, etc.
    # Our programs write to serial via UEFI simple text output protocol.
    # Strategy: take everything after the last UEFI boot prompt,
    # or if no prompt, take the whole output and strip common noise patterns.

    # Strip common UEFI firmware noise:
    # - "UEFI Interactive Shell" header
    # - "Shell>" prompts
    # - "map:" device listings
    # - "FS0:" filesystem entries
    # - "BdsDxe:" boot messages
    # - ANSI escape sequences
    echo "$raw_output" | \
        sed 's/\x1b\[[0-9;]*[a-zA-Z]//g' | \
        grep -v '^Shell>' | \
        grep -v '^UEFI ' | \
        grep -v '^map:' | \
        grep -v '^FS[0-9]:' | \
        grep -v '^BdsDxe:' | \
        grep -v '^$' | \
        sed 's/[[:space:]]*$//'
}

# Compare actual output to expected
compare_output() {
    local actual="$1"
    local expected="$2"

    # Normalize: strip trailing whitespace from each line and trailing newlines
    local norm_actual norm_expected
    norm_actual=$(echo "$actual" | sed 's/[[:space:]]*$//')
    norm_expected=$(echo "$expected" | sed 's/[[:space:]]*$//')

    [[ "$norm_actual" == "$norm_expected" ]]
}

# Run a single test
run_test() {
    local v_file="$1"
    local test_name
    test_name=$(basename "$v_file" .v)
    local test_dir
    test_dir=$(dirname "$v_file")

    ((TOTAL++))

    # Create work directory for this test
    local work_path="$WORK_DIR/$test_name"
    rm -rf "$work_path"
    mkdir -p "$work_path"

    log_verbose "Testing: $v_file"

    # Step 1: Compile V to PE UEFI binary
    local compile_output
    compile_output=$(compile_uefi "$v_file" "$work_path" 2>&1)
    local compile_exit=$?

    if [[ $compile_exit -eq 1 ]]; then
        log_fail "$test_name (V compilation failed)"
        if [[ $VERBOSE -eq 1 ]]; then
            echo -e "  ${RED}Error:${NC}"
            echo "$compile_output" | head -10 | sed 's/^/    /'
        fi
        ((COMPILE_ERRORS++))
        FAILED_TESTS+=("$test_name: V compilation error")
        return 1
    elif [[ $compile_exit -eq 2 ]]; then
        log_fail "$test_name (No native IR generated)"
        if [[ $VERBOSE -eq 1 ]]; then
            echo -e "  ${RED}Error:${NC}"
            echo "$compile_output" | head -10 | sed 's/^/    /'
        fi
        ((IR_ERRORS++))
        FAILED_TESTS+=("$test_name: no native IR")
        return 1
    elif [[ $compile_exit -eq 3 ]]; then
        log_fail "$test_name (PE link/assemble failed)"
        if [[ $VERBOSE -eq 1 ]]; then
            echo -e "  ${RED}Error:${NC}"
            echo "$compile_output" | head -10 | sed 's/^/    /'
        fi
        ((LINK_ERRORS++))
        FAILED_TESTS+=("$test_name: PE link error")
        return 1
    elif [[ $compile_exit -ne 0 ]]; then
        log_fail "$test_name (Unknown compile error: exit $compile_exit)"
        if [[ $VERBOSE -eq 1 ]]; then
            echo "$compile_output" | head -10 | sed 's/^/    /'
        fi
        ((COMPILE_ERRORS++))
        FAILED_TESTS+=("$test_name: compile error ($compile_exit)")
        return 1
    fi

    # Step 2: Create EFI boot structure
    log_verbose "  Creating EFI boot structure..."
    local efi_dir="$work_path/efi_root"
    create_efi_structure "$work_path/BOOTX64.EFI" "$efi_dir"

    # Step 3: Run in QEMU with OVMF
    log_verbose "  Booting in QEMU (timeout ${TIMEOUT_SEC}s)..."
    local raw_output
    raw_output=$(run_uefi_qemu "$efi_dir" "$TIMEOUT_SEC" 2>&1)
    local qemu_exit=$?

    # Filter UEFI firmware noise
    local actual_output
    actual_output=$(filter_serial_output "$raw_output")

    if [[ $qemu_exit -eq 124 ]]; then
        log_fail "$test_name (QEMU timeout after ${TIMEOUT_SEC}s)"
        if [[ $VERBOSE -eq 1 ]]; then
            echo -e "  ${YELLOW}Raw serial output:${NC}"
            echo "$raw_output" | head -20 | sed 's/^/    /'
        fi
        ((RUNTIME_ERRORS++))
        FAILED_TESTS+=("$test_name: QEMU timeout")
        return 1
    fi

    # Step 4: Compare output (if expected file exists)
    local expected_file="$test_dir/${test_name}.expected"
    if [[ -f "$expected_file" ]]; then
        local expected_output
        expected_output=$(cat "$expected_file")

        if compare_output "$actual_output" "$expected_output"; then
            log_pass "$test_name"
            ((PASSED++))
        else
            log_fail "$test_name (Output mismatch)"
            if [[ $VERBOSE -eq 1 ]]; then
                echo -e "  ${YELLOW}Expected:${NC}"
                echo "$expected_output" | sed 's/^/    /'
                echo -e "  ${YELLOW}Actual (filtered):${NC}"
                echo "$actual_output" | sed 's/^/    /'
                echo -e "  ${YELLOW}Raw serial:${NC}"
                echo "$raw_output" | head -20 | sed 's/^/    /'
            fi
            ((FAILED++))
            FAILED_TESTS+=("$test_name: output mismatch")
            return 1
        fi
    else
        # No expected file — check QEMU exited without error
        if [[ $qemu_exit -eq 0 || $qemu_exit -eq 124 ]]; then
            log_pass "$test_name (no .expected file, QEMU ran)"
            if [[ $VERBOSE -eq 1 ]]; then
                echo -e "  ${CYAN}Serial output:${NC}"
                echo "$actual_output" | head -10 | sed 's/^/    /'
            fi
            ((PASSED++))
        else
            log_fail "$test_name (QEMU error: exit $qemu_exit)"
            ((RUNTIME_ERRORS++))
            FAILED_TESTS+=("$test_name: QEMU error (exit $qemu_exit)")
            return 1
        fi
    fi

    # Cleanup unless keeping artifacts
    if [[ $KEEP_ARTIFACTS -eq 0 ]]; then
        rm -rf "$work_path"
    else
        log_verbose "  Artifacts kept in: $work_path"
    fi

    return 0
}

# === Main ===

# Parse options
TEST_PATHS=()
while [[ $# -gt 0 ]]; do
    case "$1" in
        -v|--verbose)
            VERBOSE=1
            shift
            ;;
        -k|--keep)
            KEEP_ARTIFACTS=1
            shift
            ;;
        --ovmf)
            OVMF_PATH="$2"
            shift 2
            ;;
        --timeout)
            TIMEOUT_SEC="$2"
            shift 2
            ;;
        -h|--help)
            usage
            ;;
        -*)
            echo "Unknown option: $1"
            usage
            ;;
        *)
            TEST_PATHS+=("$1")
            shift
            ;;
    esac
done

# Default test path
if [[ ${#TEST_PATHS[@]} -eq 0 ]]; then
    TEST_PATHS=("$DEFAULT_TEST_DIR")
fi

# Check all prerequisites
check_prerequisites

# Prepare work directory
mkdir -p "$WORK_DIR"

# Print header
echo "============================================"
echo "   V-to-Native x86_64 UEFI Test Harness"
echo "============================================"
echo ""
echo "V Compiler:  $V_COMPILER"
echo "QEMU:        $(command -v qemu-system-x86_64)"
echo "OVMF:        $OVMF_PATH"
echo "Timeout:     ${TIMEOUT_SEC}s per test"
echo ""

# Collect all test files
TEST_FILES=()
for path in "${TEST_PATHS[@]}"; do
    if [[ -f "$path" ]]; then
        if [[ "$path" == *.v ]]; then
            TEST_FILES+=("$path")
        fi
    elif [[ -d "$path" ]]; then
        while IFS= read -r -d '' f; do
            TEST_FILES+=("$f")
        done < <(find "$path" -name "*.v" -type f -print0 | sort -z)
    else
        echo -e "${YELLOW}Warning:${NC} Path not found: $path"
    fi
done

if [[ ${#TEST_FILES[@]} -eq 0 ]]; then
    echo -e "${RED}Error:${NC} No .v test files found"
    exit 2
fi

echo "Found ${#TEST_FILES[@]} test file(s)"
echo ""

# Run all tests
for test_file in "${TEST_FILES[@]}"; do
    run_test "$test_file"
done

# Print summary
TOTAL_FAILED=$((FAILED + COMPILE_ERRORS + IR_ERRORS + LINK_ERRORS + RUNTIME_ERRORS))

echo ""
echo "============================================"
echo "                Summary"
echo "============================================"
echo ""
echo -e "Total:          $TOTAL"
echo -e "${GREEN}Passed:         $PASSED${NC}"
if [[ $TOTAL_FAILED -gt 0 ]]; then
    echo -e "${RED}Failed:         $TOTAL_FAILED${NC}"
    if [[ $COMPILE_ERRORS -gt 0 ]]; then
        echo -e "  ${YELLOW}- V compile:    $COMPILE_ERRORS${NC}"
    fi
    if [[ $IR_ERRORS -gt 0 ]]; then
        echo -e "  ${YELLOW}- IR gen:       $IR_ERRORS${NC}"
    fi
    if [[ $LINK_ERRORS -gt 0 ]]; then
        echo -e "  ${YELLOW}- PE link:      $LINK_ERRORS${NC}"
    fi
    if [[ $RUNTIME_ERRORS -gt 0 ]]; then
        echo -e "  ${YELLOW}- QEMU runtime: $RUNTIME_ERRORS${NC}"
    fi
    if [[ $FAILED -gt 0 ]]; then
        echo -e "  ${YELLOW}- Mismatch:     $FAILED${NC}"
    fi
fi
echo ""

# Calculate pass rate
if [[ $TOTAL -gt 0 ]]; then
    PASS_RATE=$((PASSED * 100 / TOTAL))
    echo "Pass rate: ${PASS_RATE}%"
else
    echo "No tests run"
fi

# List failed tests if any
if [[ ${#FAILED_TESTS[@]} -gt 0 ]]; then
    echo ""
    echo "Failed tests:"
    for t in "${FAILED_TESTS[@]}"; do
        echo "  - $t"
    done
fi

# Cleanup work directory if empty and not keeping
if [[ $KEEP_ARTIFACTS -eq 0 ]]; then
    rmdir "$WORK_DIR" 2>/dev/null || true
fi

echo ""
echo "============================================"

# Exit with failure if any tests failed
if [[ $TOTAL_FAILED -gt 0 ]]; then
    exit 1
fi
exit 0
