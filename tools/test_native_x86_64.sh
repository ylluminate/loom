#!/bin/bash
# V-to-Native x86_64 ELF Test Harness
# Tests V programs through the native x86_64 ELF pipeline: V -> IR -> x86_64 ELF -> Run
#
# Usage:
#   ./scripts/test_native_x86_64.sh                    # Run all tests in tests/native/
#   ./scripts/test_native_x86_64.sh tests/native/      # Run tests in specific directory
#   ./scripts/test_native_x86_64.sh hello.v             # Run specific test file
#   ./scripts/test_native_x86_64.sh --verbose           # Verbose output
#
# Execution methods (in order of preference):
#   1. Docker with --platform linux/amd64 (macOS + OrbStack/Docker Desktop)
#   2. qemu-x86_64 user-mode emulation (Linux ARM64/other)
#   3. Direct execution (native x86_64 Linux)
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
WORK_DIR="$PROJECT_ROOT/beam_output/native_x86_64_tests"
VBEAM_RT_DIR="$PROJECT_ROOT/vbeam_rt"
TIMEOUT_SEC=10

# Docker image for x86_64 execution (must be pre-pulled or locally available)
DOCKER_IMAGE="${DOCKER_IMAGE:-thevlang/vlang:alpine}"

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

# Execution method
RUN_METHOD=""

# === Functions ===

usage() {
    echo "Usage: $0 [OPTIONS] [test_path...]"
    echo ""
    echo "Options:"
    echo "  -v, --verbose      Show detailed output"
    echo "  -k, --keep         Keep intermediate artifacts"
    echo "  --docker IMAGE     Docker image for x86_64 execution (default: $DOCKER_IMAGE)"
    echo "  --timeout SECONDS  Per-test timeout (default: $TIMEOUT_SEC)"
    echo "  -h, --help         Show this help"
    echo ""
    echo "Arguments:"
    echo "  test_path          Directory of tests or specific .v file(s)"
    echo "                     Default: $DEFAULT_TEST_DIR"
    echo ""
    echo "Execution methods (auto-detected):"
    echo "  Docker:    macOS + OrbStack/Docker Desktop (--platform linux/amd64)"
    echo "  QEMU:      qemu-x86_64 user-mode (Linux ARM64)"
    echo "  Direct:    native x86_64 Linux"
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

detect_run_method() {
    local os_name arch_name
    os_name=$(uname -s)
    arch_name=$(uname -m)

    if [[ "$os_name" == "Linux" && "$arch_name" == "x86_64" ]]; then
        RUN_METHOD="direct"
        log_info "Execution: direct (native x86_64 Linux)"
        return 0
    fi

    # Check for qemu-x86_64 user-mode (Linux only)
    if [[ "$os_name" == "Linux" ]] && command -v qemu-x86_64 &>/dev/null; then
        RUN_METHOD="qemu"
        log_info "Execution: qemu-x86_64 user-mode"
        return 0
    fi

    # Check for Docker (works on macOS with OrbStack/Docker Desktop)
    if command -v docker &>/dev/null; then
        # Verify Docker is running
        if docker info &>/dev/null 2>&1; then
            # Verify the image exists locally
            if docker image inspect "$DOCKER_IMAGE" &>/dev/null 2>&1; then
                RUN_METHOD="docker"
                log_info "Execution: Docker --platform linux/amd64 ($DOCKER_IMAGE)"
                return 0
            else
                echo -e "${YELLOW}Warning:${NC} Docker image '$DOCKER_IMAGE' not found locally."
                echo "  Pull it:  docker pull --platform linux/amd64 $DOCKER_IMAGE"
                echo "  Or use:   DOCKER_IMAGE=<image> $0"
            fi
        else
            echo -e "${YELLOW}Warning:${NC} Docker daemon not running"
            echo "  Start OrbStack: orbctl start"
            echo "  Or start Docker Desktop"
        fi
    fi

    echo -e "${RED}Error:${NC} No execution method available for x86_64 ELF binaries."
    echo ""
    echo "Options:"
    echo "  1. Install Docker + OrbStack:  brew install orbstack"
    echo "  2. Install QEMU user-mode (Linux only):  apt install qemu-user"
    echo "  3. Run on native x86_64 Linux"
    exit 2
}

# Run an x86_64 ELF binary
run_x86_64() {
    local binary="$1"
    local timeout_sec="${2:-$TIMEOUT_SEC}"

    case "$RUN_METHOD" in
        direct)
            timeout "$timeout_sec" "$binary" 2>&1
            ;;
        qemu)
            timeout "$timeout_sec" qemu-x86_64 "$binary" 2>&1
            ;;
        docker)
            # Mount the directory containing the binary into the container
            local binary_dir
            binary_dir=$(dirname "$binary")
            local binary_name
            binary_name=$(basename "$binary")
            timeout "$timeout_sec" \
                docker run --rm --platform linux/amd64 \
                    -v "$binary_dir:/work:ro" \
                    "$DOCKER_IMAGE" "/work/$binary_name" 2>&1
            ;;
        *)
            echo "Unknown run method: $RUN_METHOD"
            return 1
            ;;
    esac
    return $?
}

# Compile V source to x86_64 ELF native binary
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

    # Step 1: Compile V to native IR (x86_64, elf64 format)
    log_verbose "  V -> Native IR (target=x86_64, format=elf64)..."
    local v_output
    v_output=$(VBEAM_TARGET=x86_64 VBEAM_FORMAT=elf64 "$V_COMPILER" -b beam "$v_file" 2>&1)
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
    log_verbose "  Native IR -> x86_64 ELF binary..."
    local out_file="$work_dir/a.out"

    # Use erl to invoke vbeam_native:main/1
    local ebin_dir="$VBEAM_RT_DIR/_build/default/lib/vbeam_rt/ebin"
    if [[ ! -d "$ebin_dir" ]]; then
        ebin_dir="$VBEAM_RT_DIR/src"
    fi

    local compile_output
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

    # Verify it's an ELF binary
    local file_type
    file_type=$(file "$out_file" 2>/dev/null)
    if [[ "$file_type" != *"ELF 64-bit"* ]]; then
        echo "Output is not an ELF 64-bit binary: $file_type"
        return 3
    fi

    log_verbose "  Binary produced: $(wc -c < "$out_file") bytes"
    return 0
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
    log_verbose "  Running x86_64 binary..."
    local actual_output
    actual_output=$(run_x86_64 "$work_path/a.out" "$TIMEOUT_SEC" 2>&1)
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
        # No expected file -- just check it runs without crashing
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
        --docker)
            DOCKER_IMAGE="$2"
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

# Detect execution method
detect_run_method

# Prepare work directory
mkdir -p "$WORK_DIR"

# Print header
echo "============================================"
echo "   V-to-Native x86_64 ELF Test Harness"
echo "============================================"
echo ""
echo "V Compiler:  $V_COMPILER"
echo "Execution:   $RUN_METHOD"
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
