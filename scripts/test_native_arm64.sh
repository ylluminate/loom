#!/bin/bash
# V-to-Native ARM64 Test Harness
# Tests V programs through the native ARM64 pipeline: V -> IR -> ARM64 Mach-O/ELF -> Run
#
# Usage:
#   ./scripts/test_native_arm64.sh                    # Run all tests in tests/native/
#   ./scripts/test_native_arm64.sh tests/native/      # Run tests in specific directory
#   ./scripts/test_native_arm64.sh hello.v             # Run specific test file
#   ./scripts/test_native_arm64.sh --verbose           # Verbose output
#
# Platform detection:
#   - ARM64 Mac:   Mach-O executables run directly
#   - x86_64 Linux: Uses qemu-aarch64 user-mode emulation (if available)
#
# Test case format:
#   foo.v          - V source file
#   foo.expected   - Expected stdout output
#
# Exit codes:
#   0 - All tests passed
#   1 - Some tests failed
#   2 - Configuration error

set -o pipefail

# === Configuration ===
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
V_COMPILER="/Users/u/tank/ops/tools/dev/vlang/v"
DEFAULT_TEST_DIR="$PROJECT_ROOT/tests/native"
WORK_DIR="$PROJECT_ROOT/beam_output/native_arm64_tests"
VBEAM_RT_DIR="$PROJECT_ROOT/vbeam_rt"
TIMEOUT_SEC=10

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
declare -a COMPILE_FAILED

# Platform detection
PLATFORM=""
ARCH=""
RUN_CMD=""

# === Functions ===

usage() {
    echo "Usage: $0 [OPTIONS] [test_path...]"
    echo ""
    echo "Options:"
    echo "  -v, --verbose      Show detailed output"
    echo "  -k, --keep         Keep intermediate artifacts"
    echo "  -h, --help         Show this help"
    echo ""
    echo "Arguments:"
    echo "  test_path          Directory of tests or specific .v file(s)"
    echo "                     Default: $DEFAULT_TEST_DIR"
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

detect_platform() {
    local os_name arch_name
    os_name=$(uname -s)
    arch_name=$(uname -m)

    if [[ "$os_name" == "Darwin" && "$arch_name" == "arm64" ]]; then
        PLATFORM="arm64-macos"
        ARCH="arm64"
        RUN_CMD="direct"
        log_info "Platform: ARM64 macOS (native execution)"
    elif [[ "$os_name" == "Darwin" && "$arch_name" == "x86_64" ]]; then
        # x86_64 Mac — check for Rosetta 2 (can run ARM64 Mach-O via translation)
        if sysctl -n sysctl.proc_translated 2>/dev/null | grep -q 1; then
            PLATFORM="arm64-macos-rosetta"
            ARCH="arm64"
            RUN_CMD="direct"
            log_info "Platform: x86_64 macOS under Rosetta 2 (native execution via translation)"
        else
            echo -e "${RED}Error:${NC} x86_64 macOS without Rosetta 2 cannot run ARM64 binaries"
            echo "  Install Rosetta 2: softwareupdate --install-rosetta"
            exit 2
        fi
    elif [[ "$os_name" == "Linux" && "$arch_name" == "aarch64" ]]; then
        PLATFORM="arm64-linux"
        ARCH="arm64"
        # On ARM64 Linux, we'd need to build ELF not Mach-O
        # For now this script targets Mach-O; ELF ARM64 Linux is future work
        echo -e "${YELLOW}Warning:${NC} ARM64 Linux detected. This script builds Mach-O (macOS) binaries."
        echo "  ARM64 ELF Linux native tests are planned for a future phase."
        exit 2
    elif [[ "$os_name" == "Linux" && "$arch_name" == "x86_64" ]]; then
        PLATFORM="x86_64-linux"
        ARCH="x86_64"
        # Check for qemu-aarch64 user-mode emulation
        if command -v qemu-aarch64 &>/dev/null; then
            RUN_CMD="qemu-aarch64"
            log_info "Platform: x86_64 Linux (ARM64 via qemu-aarch64 user-mode emulation)"
            log_info "Note: qemu-aarch64 can run ELF ARM64 binaries, not Mach-O."
            log_info "      Building as ELF ARM64 for QEMU compatibility."
        else
            echo -e "${RED}Error:${NC} x86_64 Linux requires qemu-aarch64 for ARM64 emulation"
            echo "  Install: sudo apt install qemu-user qemu-user-static"
            echo "           or: sudo dnf install qemu-user-static"
            exit 2
        fi
    else
        echo -e "${RED}Error:${NC} Unsupported platform: $os_name $arch_name"
        exit 2
    fi
}

# Determine the output format based on platform
get_format() {
    case "$PLATFORM" in
        arm64-macos|arm64-macos-rosetta)
            echo "macho"
            ;;
        x86_64-linux)
            # QEMU user-mode needs ELF, not Mach-O
            echo "elf64"
            ;;
        *)
            echo "macho"
            ;;
    esac
}

# Compile V source to native IR, then to native binary
compile_native() {
    local v_file="$1"
    local work_dir="$2"
    local test_name
    test_name=$(basename "$v_file" .v)
    local v_dir
    v_dir=$(dirname "$v_file")
    local beam_out_dir="$v_dir/${test_name}.beam"

    # Clean any previous output
    rm -rf "$beam_out_dir"

    local format
    format=$(get_format)

    # Step 1: Compile V to native IR
    log_verbose "  V -> Native IR (target=arm64, format=$format)..."
    local v_output
    v_output=$(VBEAM_TARGET=arm64 VBEAM_FORMAT="$format" "$V_COMPILER" -b beam "$v_file" 2>&1)
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

    # Step 2: Compile IR to native binary using vbeam_native
    log_verbose "  Native IR -> Binary..."
    local out_file="$work_dir/a.out"
    local compile_output

    # Build the BEAM modules if not already done
    if [[ ! -f "$VBEAM_RT_DIR/ebin/vbeam_native.beam" ]]; then
        log_verbose "  Building vbeam_rt modules..."
        (cd "$VBEAM_RT_DIR" && rebar3 compile 2>&1) || true
    fi

    # Use erl to invoke vbeam_native:main/1
    local ebin_dir="$VBEAM_RT_DIR/_build/default/lib/vbeam_rt/ebin"
    if [[ ! -d "$ebin_dir" ]]; then
        ebin_dir="$VBEAM_RT_DIR/ebin"
    fi

    compile_output=$(erl -noshell \
        -pa "$ebin_dir" \
        -eval "vbeam_native:main([\"$ir_file\", \"-o\", \"$out_file\"]), init:stop()." \
        2>&1)
    local compile_exit=$?

    # Clean up V compiler output
    rm -rf "$beam_out_dir"

    if [[ $compile_exit -ne 0 ]]; then
        echo "Native compilation failed: $compile_output"
        return 3
    fi

    if [[ ! -f "$out_file" ]]; then
        echo "Native compiler did not produce output: $out_file"
        echo "Compiler output: $compile_output"
        return 3
    fi

    log_verbose "  Binary produced: $(wc -c < "$out_file") bytes"
    return 0
}

# Run the native binary and capture output
run_native() {
    local binary="$1"
    local timeout_sec="${2:-$TIMEOUT_SEC}"

    case "$RUN_CMD" in
        direct)
            timeout "$timeout_sec" "$binary" 2>&1
            ;;
        qemu-aarch64)
            timeout "$timeout_sec" qemu-aarch64 "$binary" 2>&1
            ;;
        *)
            echo "Unknown run command: $RUN_CMD"
            return 1
            ;;
    esac
    return $?
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

    # Step 1: Compile V to native binary
    local compile_output
    compile_output=$(compile_native "$v_file" "$work_path" 2>&1)
    local compile_exit=$?

    if [[ $compile_exit -eq 1 ]]; then
        log_fail "$test_name (V compilation failed)"
        if [[ $VERBOSE -eq 1 ]]; then
            echo -e "  ${RED}Error:${NC}"
            echo "$compile_output" | head -10 | sed 's/^/    /'
        fi
        ((COMPILE_ERRORS++))
        COMPILE_FAILED+=("$test_name")
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
        log_fail "$test_name (Native link/assemble failed)"
        if [[ $VERBOSE -eq 1 ]]; then
            echo -e "  ${RED}Error:${NC}"
            echo "$compile_output" | head -10 | sed 's/^/    /'
        fi
        ((LINK_ERRORS++))
        FAILED_TESTS+=("$test_name: native link error")
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

    # Step 2: Run the native binary
    log_verbose "  Running native binary..."
    local actual_output
    actual_output=$(run_native "$work_path/a.out" "$TIMEOUT_SEC" 2>&1)
    local run_exit=$?

    if [[ $run_exit -eq 124 ]]; then
        log_fail "$test_name (Timeout after ${TIMEOUT_SEC}s)"
        ((RUNTIME_ERRORS++))
        FAILED_TESTS+=("$test_name: timeout")
        return 1
    fi

    if [[ $run_exit -ne 0 ]]; then
        log_fail "$test_name (Runtime error: exit code $run_exit)"
        if [[ $VERBOSE -eq 1 ]]; then
            echo -e "  ${RED}Output:${NC}"
            echo "$actual_output" | head -10 | sed 's/^/    /'
        fi
        ((RUNTIME_ERRORS++))
        FAILED_TESTS+=("$test_name: runtime error (exit $run_exit)")
        return 1
    fi

    # Step 3: Compare output (if expected file exists)
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
                echo -e "  ${YELLOW}Actual:${NC}"
                echo "$actual_output" | sed 's/^/    /'
            fi
            ((FAILED++))
            FAILED_TESTS+=("$test_name: output mismatch")
            return 1
        fi
    else
        # No expected file — just check it runs without crashing
        log_pass "$test_name (no .expected file, ran without error)"
        if [[ $VERBOSE -eq 1 ]]; then
            echo -e "  ${CYAN}Output:${NC}"
            echo "$actual_output" | sed 's/^/    /'
        fi
        ((PASSED++))
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

# Validate V compiler
if [[ ! -x "$V_COMPILER" ]]; then
    echo -e "${RED}Error:${NC} V compiler not found at $V_COMPILER"
    exit 2
fi

# Validate Erlang is available (needed for vbeam_native)
if ! command -v erl &>/dev/null; then
    echo -e "${RED}Error:${NC} Erlang/OTP not found. Install Erlang to use the native compiler."
    exit 2
fi

# Detect platform and execution method
detect_platform

# Prepare work directory
mkdir -p "$WORK_DIR"

# Print header
echo "============================================"
echo "     V-to-Native ARM64 Test Harness"
echo "============================================"
echo ""
echo "V Compiler:  $V_COMPILER"
echo "Platform:    $PLATFORM"
echo "Execution:   $RUN_CMD"
echo "Format:      $(get_format)"
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
        echo -e "  ${YELLOW}- Link/assemble: $LINK_ERRORS${NC}"
    fi
    if [[ $RUNTIME_ERRORS -gt 0 ]]; then
        echo -e "  ${YELLOW}- Runtime:      $RUNTIME_ERRORS${NC}"
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
