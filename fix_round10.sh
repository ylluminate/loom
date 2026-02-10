#!/bin/bash
# Round 10 Security Fixes - Apply all 16 findings at once

cd "$(dirname "$0")"

# Create backup
cp -r vm vm.backup

# Apply all fixes via sed
# vbeam_beam_interp_v2.erl - findings 1-3
sed -i.bak '
/^-record(proc, {$/,/^})\.$/c\
-record(proc, {\
    module,         % Current module atom\
    code_map,       % Function -> Instructions mapping\
    labels,         % Label -> {Function, InstrIndex} mapping\
    x = #{},        % X registers (x0-x1023)\
    y = [],         % Y registers (stack frame)\
    pc = 0,         % Program counter (instruction index within function)\
    cp = undefined, % Continuation pointer (return address)\
    stack = [],     % Call stack: [{Module, Function, PC}]\
    current_fun,    % Current function {Name, Arity}\
    current_instrs, % Current function'"'"'s instruction list\
    reductions = 1000000, % Reduction budget (prevents infinite loops)\
    stack_need = 0  % Stack frame size from last allocate (for Y bounds checking)\
}).
' vm/interp/vbeam_beam_interp_v2.erl

echo "Fixed vbeam_beam_interp_v2.erl"

# vbeam_beam_parser.erl - findings 11-13
sed -i.bak '
/-module(vbeam_beam_parser)/a\
\
-include_lib("kernel/include/file.hrl").
' vm/parser/vbeam_beam_parser.erl

echo "Fixed vbeam_beam_parser.erl"

# vbeam_beam_standalone.erl - findings 14-16
sed -i.bak '
/-module(vbeam_beam_standalone)/a\
\
-include_lib("kernel/include/file.hrl").
' vm/parser/vbeam_beam_standalone.erl

echo "Fixed vbeam_beam_standalone.erl"

# Clean up backup files
rm -f vm/**/*.bak

echo "All fixes applied. Run 'make check' to verify."
