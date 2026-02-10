%% @doc Linux x86_64 syscall dispatch table for V-on-BEAM kernel
%% Routes Linux syscalls to appropriate BEAM services
-module(vbeam_linux_syscall).
-export([dispatch/2, syscall_name/1, init/0]).

%% Error codes (Linux errno values)
-define(ENOSYS, 38).  % Function not implemented
-define(EINVAL, 22).  % Invalid argument
-define(EBADF, 9).    % Bad file descriptor
-define(ENOMEM, 12).  % Out of memory
-define(EFAULT, 14).  % Bad address
-define(EACCES, 13).  % Permission denied
-define(ENOENT, 2).   % No such file or directory

%% Syscall state tracking via ETS (thread-safe)
-define(TRACKING_TABLE, vbeam_syscall_tracking).

%% @doc Initialize syscall dispatch system
%% Creates ETS table for tracking unimplemented syscalls
%% BUG 10 FIX: Make init idempotent with try/catch
init() ->
    try
        case ets:whereis(?TRACKING_TABLE) of
            undefined ->
                ets:new(?TRACKING_TABLE, [named_table, public, set]),
                ok;
            _ ->
                ok
        end
    catch
        error:badarg ->
            %% Table already exists (race condition)
            ok
    end.

%% @doc Dispatch a syscall to the appropriate handler
%% Returns {ok, Result} | {error, Errno}
dispatch(SyscallNr, Args) when is_integer(SyscallNr), is_list(Args) ->
    try
        dispatch_impl(SyscallNr, Args)
    catch
        _:_ ->
            %% Catch all errors to isolate handler failures
            {error, ?EINVAL}
    end.

%% @private Internal dispatch implementation
dispatch_impl(SyscallNr, Args) ->
    case SyscallNr of
        %% ============================================================
        %% IMPLEMENTED SYSCALLS - Critical for BusyBox
        %% ============================================================

        %% File I/O
        0 -> sys_read(Args);           % read(fd, buf, count)
        1 -> sys_write(Args);          % write(fd, buf, count)
        2 -> sys_open(Args);           % open(path, flags, mode)
        3 -> sys_close(Args);          % close(fd)
        8 -> sys_lseek(Args);          % lseek(fd, offset, whence)
        257 -> sys_openat(Args);       % openat(dirfd, path, flags, mode)

        %% Memory management
        9 -> sys_mmap(Args);           % mmap(addr, len, prot, flags, fd, off)
        10 -> sys_mprotect(Args);      % mprotect(addr, len, prot)
        11 -> sys_munmap(Args);        % munmap(addr, len)
        12 -> sys_brk(Args);           % brk(addr)

        %% Process management
        56 -> sys_clone(Args);         % clone(flags, stack, ptid, ctid, regs)
        57 -> sys_fork(Args);          % fork()
        59 -> sys_execve(Args);        % execve(path, argv, envp)
        60 -> sys_exit(Args);          % exit(status)
        61 -> sys_wait4(Args);         % wait4(pid, status, options, rusage)
        231 -> sys_exit_group(Args);   % exit_group(status)

        %% Process identity
        39 -> sys_getpid(Args);        % getpid()
        110 -> sys_getppid(Args);      % getppid()
        102 -> sys_getuid(Args);       % getuid()
        104 -> sys_getgid(Args);       % getgid()
        107 -> sys_geteuid(Args);      % geteuid()
        108 -> sys_getegid(Args);      % getegid()

        %% Signals (minimal implementation)
        13 -> sys_rt_sigaction(Args);  % rt_sigaction(sig, act, oldact, sigsetsize)
        14 -> sys_rt_sigprocmask(Args);% rt_sigprocmask(how, set, oldset, sigsetsize)

        %% Filesystem metadata
        4 -> sys_stat(Args);           % stat(path, statbuf)
        5 -> sys_fstat(Args);          % fstat(fd, statbuf)
        6 -> sys_lstat(Args);          % lstat(path, statbuf)
        78 -> sys_getdents(Args);      % getdents(fd, dirp, count)
        217 -> sys_getdents64(Args);   % getdents64(fd, dirp, count)
        262 -> sys_newfstatat(Args);   % newfstatat(dirfd, path, statbuf, flags)

        %% Architecture-specific
        158 -> sys_arch_prctl(Args);   % arch_prctl(code, addr)

        %% Time
        35 -> sys_nanosleep(Args);     % nanosleep(req, rem)
        228 -> sys_clock_gettime(Args);% clock_gettime(clk_id, tp)

        %% Pipes and duplication
        22 -> sys_pipe(Args);          % pipe(pipefd)
        293 -> sys_pipe2(Args);        % pipe2(pipefd, flags)
        32 -> sys_dup(Args);           % dup(oldfd)
        33 -> sys_dup2(Args);          % dup2(oldfd, newfd)

        %% I/O control
        16 -> sys_ioctl(Args);         % ioctl(fd, cmd, arg)

        %% ============================================================
        %% STUBBED SYSCALLS - Return -ENOSYS with logging
        %% ============================================================
        _ -> stub_syscall(SyscallNr, Args)
    end.

%% ============================================================
%% IMPLEMENTED SYSCALLS
%% ============================================================

%% File I/O operations
sys_read([Fd, Buf, Count]) ->
    vbeam_vfs:read(Fd, Buf, Count).

sys_write([Fd, Buf, Count]) ->
    vbeam_vfs:write(Fd, Buf, Count).

sys_open([Path, Flags, Mode]) ->
    vbeam_vfs:open(Path, Flags, Mode).

sys_openat([DirFd, Path, Flags, Mode]) ->
    vbeam_vfs:openat(DirFd, Path, Flags, Mode).

sys_close([Fd]) ->
    vbeam_vfs:close(Fd).

sys_lseek([Fd, Offset, Whence]) ->
    vbeam_vfs:lseek(Fd, Offset, Whence).

%% Memory management
sys_mmap([Addr, Len, Prot, Flags, Fd, Offset]) ->
    vbeam_vmm:mmap(Addr, Len, Prot, Flags, Fd, Offset).

sys_mprotect([Addr, Len, Prot]) ->
    vbeam_vmm:mprotect(Addr, Len, Prot).

sys_munmap([Addr, Len]) ->
    vbeam_vmm:munmap(Addr, Len).

sys_brk([Addr]) ->
    vbeam_vmm:brk(Addr).

%% Process management
sys_clone([Flags, Stack, Ptid, Ctid, Regs]) ->
    vbeam_proc_table:clone(Flags, Stack, Ptid, Ctid, Regs).

sys_fork(_Args) ->
    vbeam_proc_table:fork().

sys_execve([Path, Argv, Envp]) ->
    vbeam_proc_table:execve(Path, Argv, Envp).

sys_exit([Status]) ->
    vbeam_proc_table:exit(Status).

sys_wait4([Pid, Status, Options, Rusage]) ->
    vbeam_proc_table:wait4(Pid, Status, Options, Rusage).

sys_exit_group([Status]) ->
    vbeam_proc_table:exit_group(Status).

%% Process identity
sys_getpid(_Args) ->
    {ok, vbeam_proc_table:getpid()}.

sys_getppid(_Args) ->
    {ok, vbeam_proc_table:getppid()}.

sys_getuid(_Args) ->
    {ok, 1000}.  % Default UID

sys_getgid(_Args) ->
    {ok, 1000}.  % Default GID

sys_geteuid(_Args) ->
    {ok, 1000}.

sys_getegid(_Args) ->
    {ok, 1000}.

%% Signals (minimal)
sys_rt_sigaction([Sig, Act, OldAct, SigSetSize]) ->
    vbeam_signal:rt_sigaction(Sig, Act, OldAct, SigSetSize).

sys_rt_sigprocmask([How, Set, OldSet, SigSetSize]) ->
    vbeam_signal:rt_sigprocmask(How, Set, OldSet, SigSetSize).

%% Filesystem metadata
sys_stat([Path, StatBuf]) ->
    vbeam_vfs:stat(Path, StatBuf).

sys_fstat([Fd, StatBuf]) ->
    vbeam_vfs:fstat(Fd, StatBuf).

sys_lstat([Path, StatBuf]) ->
    vbeam_vfs:lstat(Path, StatBuf).

sys_getdents([Fd, DirP, Count]) ->
    vbeam_vfs:getdents(Fd, DirP, Count).

sys_getdents64([Fd, DirP, Count]) ->
    vbeam_vfs:getdents64(Fd, DirP, Count).

sys_newfstatat([DirFd, Path, StatBuf, Flags]) ->
    vbeam_vfs:newfstatat(DirFd, Path, StatBuf, Flags).

%% Architecture-specific
%% BUG 3 FIX: Track FS/GS base addresses using process dictionary
sys_arch_prctl([Code, Addr]) ->
    case Code of
        16#1001 -> % ARCH_SET_GS
            put(vbeam_gs_base, Addr),
            {ok, 0};
        16#1002 -> % ARCH_SET_FS
            put(vbeam_fs_base, Addr),
            {ok, 0};
        16#1003 -> % ARCH_GET_FS
            %% SECURITY FIX: Validate Addr is page-aligned and within reasonable range
            case is_integer(Addr) andalso (Addr band 16#FFF) =:= 0 andalso Addr < 16#7FFFFFFFFFFF of
                true ->
                    %% SECURITY: Cap user_mem entries at 64 to prevent unbounded growth
                    CurrentCount = length([K || K <- get_keys(),
                                               is_tuple(K),
                                               tuple_size(K) =:= 2,
                                               element(1, K) =:= vbeam_user_mem]),
                    case CurrentCount >= 64 of
                        true ->
                            {error, ?ENOMEM};  %% Out of memory
                        false ->
                            %% Linux writes FS base to *(unsigned long *)Addr
                            %% Simulate by storing to process dictionary key
                            FSBase = case get(vbeam_fs_base) of
                                undefined -> 0;
                                Val -> Val
                            end,
                            put({vbeam_user_mem, Addr}, FSBase),
                            {ok, 0} %% Return 0 (success), not the base address
                    end;
                false ->
                    {error, ?EFAULT} %% Bad address
            end;
        16#1004 -> % ARCH_GET_GS
            %% SECURITY FIX: Validate Addr is page-aligned and within reasonable range
            case is_integer(Addr) andalso (Addr band 16#FFF) =:= 0 andalso Addr < 16#7FFFFFFFFFFF of
                true ->
                    %% SECURITY: Cap user_mem entries at 64 to prevent unbounded growth
                    CurrentCount = length([K || K <- get_keys(),
                                               is_tuple(K),
                                               tuple_size(K) =:= 2,
                                               element(1, K) =:= vbeam_user_mem]),
                    case CurrentCount >= 64 of
                        true ->
                            {error, ?ENOMEM};  %% Out of memory
                        false ->
                            %% Linux writes GS base to *(unsigned long *)Addr
                            GSBase = case get(vbeam_gs_base) of
                                undefined -> 0;
                                Val -> Val
                            end,
                            put({vbeam_user_mem, Addr}, GSBase),
                            {ok, 0} %% Return 0 (success), not the base address
                    end;
                false ->
                    {error, ?EFAULT} %% Bad address
            end;
        _ -> {error, ?EINVAL}
    end.

%% Time
sys_nanosleep([Req, Rem]) ->
    vbeam_time:nanosleep(Req, Rem).

sys_clock_gettime([ClkId, Tp]) ->
    vbeam_time:clock_gettime(ClkId, Tp).

%% Pipes
sys_pipe([PipeFd]) ->
    vbeam_vfs:pipe(PipeFd).

sys_pipe2([PipeFd, Flags]) ->
    vbeam_vfs:pipe2(PipeFd, Flags).

sys_dup([OldFd]) ->
    vbeam_vfs:dup(OldFd).

sys_dup2([OldFd, NewFd]) ->
    vbeam_vfs:dup2(OldFd, NewFd).

%% I/O control
sys_ioctl([Fd, Cmd, Arg]) ->
    vbeam_vfs:ioctl(Fd, Cmd, Arg).

%% ============================================================
%% STUBBED SYSCALLS
%% ============================================================

stub_syscall(SyscallNr, Args) ->
    %% Log first occurrence using ETS (thread-safe, lazy init)
    try
        case ets:lookup(?TRACKING_TABLE, SyscallNr) of
            [] ->
                %% BUG 5 FIX: Add cardinality check to prevent unbounded ETS growth
                MaxUnknownSyscalls = 10000,
                case ets:info(?TRACKING_TABLE, size) of
                    Size when Size < MaxUnknownSyscalls ->
                        %% BUG 7 FIX: Truncate args to first 3 elements, limit representation
                        Name = syscall_name(SyscallNr),
                        TruncatedArgs = lists:sublist(Args, 3),
                        SafeArgs = [truncate_arg(Arg) || Arg <- TruncatedArgs],
                        io:format("[vbeam] UNIMPLEMENTED SYSCALL: ~s (~p) args=~p~n",
                                 [Name, SyscallNr, SafeArgs]),
                        ets:insert(?TRACKING_TABLE, {SyscallNr, true});
                    _ ->
                        %% Table full - don't insert
                        ok
                end;
            _ ->
                %% Already seen
                ok
        end
    catch
        error:badarg ->
            %% Table doesn't exist - initialize once, don't retry recursively
            %% Just return error to prevent stack exhaustion on protected ETS failure
            init()
    end,
    {error, ?ENOSYS}.

%% BUG 7 FIX: Truncate arg representation to 200 chars max
truncate_arg(Arg) ->
    Str = lists:flatten(io_lib:format("~P", [Arg, 10])),
    case length(Str) of
        Len when Len > 200 ->
            lists:sublist(Str, 200) ++ "...";
        _ ->
            Str
    end.

%% @doc Get syscall name from number
syscall_name(Nr) ->
    case Nr of
        %% Complete x86_64 syscall table
        0 -> "read";
        1 -> "write";
        2 -> "open";
        3 -> "close";
        4 -> "stat";
        5 -> "fstat";
        6 -> "lstat";
        7 -> "poll";
        8 -> "lseek";
        9 -> "mmap";
        10 -> "mprotect";
        11 -> "munmap";
        12 -> "brk";
        13 -> "rt_sigaction";
        14 -> "rt_sigprocmask";
        15 -> "rt_sigreturn";
        16 -> "ioctl";
        17 -> "pread64";
        18 -> "pwrite64";
        19 -> "readv";
        20 -> "writev";
        21 -> "access";
        22 -> "pipe";
        23 -> "select";
        24 -> "sched_yield";
        25 -> "mremap";
        26 -> "msync";
        27 -> "mincore";
        28 -> "madvise";
        29 -> "shmget";
        30 -> "shmat";
        31 -> "shmctl";
        32 -> "dup";
        33 -> "dup2";
        34 -> "pause";
        35 -> "nanosleep";
        36 -> "getitimer";
        37 -> "alarm";
        38 -> "setitimer";
        39 -> "getpid";
        40 -> "sendfile";
        41 -> "socket";
        42 -> "connect";
        43 -> "accept";
        44 -> "sendto";
        45 -> "recvfrom";
        46 -> "sendmsg";
        47 -> "recvmsg";
        48 -> "shutdown";
        49 -> "bind";
        50 -> "listen";
        51 -> "getsockname";
        52 -> "getpeername";
        53 -> "socketpair";
        54 -> "setsockopt";
        55 -> "getsockopt";
        56 -> "clone";
        57 -> "fork";
        58 -> "vfork";
        59 -> "execve";
        60 -> "exit";
        61 -> "wait4";
        62 -> "kill";
        63 -> "uname";
        64 -> "semget";
        65 -> "semop";
        66 -> "semctl";
        67 -> "shmdt";
        68 -> "msgget";
        69 -> "msgsnd";
        70 -> "msgrcv";
        71 -> "msgctl";
        72 -> "fcntl";
        73 -> "flock";
        74 -> "fsync";
        75 -> "fdatasync";
        76 -> "truncate";
        77 -> "ftruncate";
        78 -> "getdents";
        79 -> "getcwd";
        80 -> "chdir";
        81 -> "fchdir";
        82 -> "rename";
        83 -> "mkdir";
        84 -> "rmdir";
        85 -> "creat";
        86 -> "link";
        87 -> "unlink";
        88 -> "symlink";
        89 -> "readlink";
        90 -> "chmod";
        91 -> "fchmod";
        92 -> "chown";
        93 -> "fchown";
        94 -> "lchown";
        95 -> "umask";
        96 -> "gettimeofday";
        97 -> "getrlimit";
        98 -> "getrusage";
        99 -> "sysinfo";
        100 -> "times";
        101 -> "ptrace";
        102 -> "getuid";
        103 -> "syslog";
        104 -> "getgid";
        105 -> "setuid";
        106 -> "setgid";
        107 -> "geteuid";
        108 -> "getegid";
        109 -> "setpgid";
        110 -> "getppid";
        111 -> "getpgrp";
        112 -> "setsid";
        113 -> "setreuid";
        114 -> "setregid";
        115 -> "getgroups";
        116 -> "setgroups";
        117 -> "setresuid";
        118 -> "getresuid";
        119 -> "setresgid";
        120 -> "getresgid";
        121 -> "getpgid";
        122 -> "setfsuid";
        123 -> "setfsgid";
        124 -> "getsid";
        125 -> "capget";
        126 -> "capset";
        127 -> "rt_sigpending";
        128 -> "rt_sigtimedwait";
        129 -> "rt_sigqueueinfo";
        130 -> "rt_sigsuspend";
        131 -> "sigaltstack";
        132 -> "utime";
        133 -> "mknod";
        134 -> "uselib";
        135 -> "personality";
        136 -> "ustat";
        137 -> "statfs";
        138 -> "fstatfs";
        139 -> "sysfs";
        140 -> "getpriority";
        141 -> "setpriority";
        142 -> "sched_setparam";
        143 -> "sched_getparam";
        144 -> "sched_setscheduler";
        145 -> "sched_getscheduler";
        146 -> "sched_get_priority_max";
        147 -> "sched_get_priority_min";
        148 -> "sched_rr_get_interval";
        149 -> "mlock";
        150 -> "munlock";
        151 -> "mlockall";
        152 -> "munlockall";
        153 -> "vhangup";
        154 -> "modify_ldt";
        155 -> "pivot_root";
        156 -> "_sysctl";
        157 -> "prctl";
        158 -> "arch_prctl";
        159 -> "adjtimex";
        160 -> "setrlimit";
        161 -> "chroot";
        162 -> "sync";
        163 -> "acct";
        164 -> "settimeofday";
        165 -> "mount";
        166 -> "umount2";
        167 -> "swapon";
        168 -> "swapoff";
        169 -> "reboot";
        170 -> "sethostname";
        171 -> "setdomainname";
        172 -> "iopl";
        173 -> "ioperm";
        174 -> "create_module";
        175 -> "init_module";
        176 -> "delete_module";
        177 -> "get_kernel_syms";
        178 -> "query_module";
        179 -> "quotactl";
        180 -> "nfsservctl";
        181 -> "getpmsg";
        182 -> "putpmsg";
        183 -> "afs_syscall";
        184 -> "tuxcall";
        185 -> "security";
        186 -> "gettid";
        187 -> "readahead";
        188 -> "setxattr";
        189 -> "lsetxattr";
        190 -> "fsetxattr";
        191 -> "getxattr";
        192 -> "lgetxattr";
        193 -> "fgetxattr";
        194 -> "listxattr";
        195 -> "llistxattr";
        196 -> "flistxattr";
        197 -> "removexattr";
        198 -> "lremovexattr";
        199 -> "fremovexattr";
        200 -> "tkill";
        201 -> "time";
        202 -> "futex";
        203 -> "sched_setaffinity";
        204 -> "sched_getaffinity";
        205 -> "set_thread_area";
        206 -> "io_setup";
        207 -> "io_destroy";
        208 -> "io_getevents";
        209 -> "io_submit";
        210 -> "io_cancel";
        211 -> "get_thread_area";
        212 -> "lookup_dcookie";
        213 -> "epoll_create";
        214 -> "epoll_ctl_old";
        215 -> "epoll_wait_old";
        216 -> "remap_file_pages";
        217 -> "getdents64";
        218 -> "set_tid_address";
        219 -> "restart_syscall";
        220 -> "semtimedop";
        221 -> "fadvise64";
        222 -> "timer_create";
        223 -> "timer_settime";
        224 -> "timer_gettime";
        225 -> "timer_getoverrun";
        226 -> "timer_delete";
        227 -> "clock_settime";
        228 -> "clock_gettime";
        229 -> "clock_getres";
        230 -> "clock_nanosleep";
        231 -> "exit_group";
        232 -> "epoll_wait";
        233 -> "epoll_ctl";
        234 -> "tgkill";
        235 -> "utimes";
        236 -> "vserver";
        237 -> "mbind";
        238 -> "set_mempolicy";
        239 -> "get_mempolicy";
        240 -> "mq_open";
        241 -> "mq_unlink";
        242 -> "mq_timedsend";
        243 -> "mq_timedreceive";
        244 -> "mq_notify";
        245 -> "mq_getsetattr";
        246 -> "kexec_load";
        247 -> "waitid";
        248 -> "add_key";
        249 -> "request_key";
        250 -> "keyctl";
        251 -> "ioprio_set";
        252 -> "ioprio_get";
        253 -> "inotify_init";
        254 -> "inotify_add_watch";
        255 -> "inotify_rm_watch";
        256 -> "migrate_pages";
        257 -> "openat";
        258 -> "mkdirat";
        259 -> "mknodat";
        260 -> "fchownat";
        261 -> "futimesat";
        262 -> "newfstatat";
        263 -> "unlinkat";
        264 -> "renameat";
        265 -> "linkat";
        266 -> "symlinkat";
        267 -> "readlinkat";
        268 -> "fchmodat";
        269 -> "faccessat";
        270 -> "pselect6";
        271 -> "ppoll";
        272 -> "unshare";
        273 -> "set_robust_list";
        274 -> "get_robust_list";
        275 -> "splice";
        276 -> "tee";
        277 -> "sync_file_range";
        278 -> "vmsplice";
        279 -> "move_pages";
        280 -> "utimensat";
        281 -> "epoll_pwait";
        282 -> "signalfd";
        283 -> "timerfd_create";
        284 -> "eventfd";
        285 -> "fallocate";
        286 -> "timerfd_settime";
        287 -> "timerfd_gettime";
        288 -> "accept4";
        289 -> "signalfd4";
        290 -> "eventfd2";
        291 -> "epoll_create1";
        292 -> "dup3";
        293 -> "pipe2";
        294 -> "inotify_init1";
        295 -> "preadv";
        296 -> "pwritev";
        297 -> "rt_tgsigqueueinfo";
        298 -> "perf_event_open";
        299 -> "recvmmsg";
        300 -> "fanotify_init";
        301 -> "fanotify_mark";
        302 -> "prlimit64";
        303 -> "name_to_handle_at";
        304 -> "open_by_handle_at";
        305 -> "clock_adjtime";
        306 -> "syncfs";
        307 -> "sendmmsg";
        308 -> "setns";
        309 -> "getcpu";
        310 -> "process_vm_readv";
        311 -> "process_vm_writev";
        312 -> "kcmp";
        313 -> "finit_module";
        314 -> "sched_setattr";
        315 -> "sched_getattr";
        316 -> "renameat2";
        317 -> "seccomp";
        318 -> "getrandom";
        319 -> "memfd_create";
        320 -> "kexec_file_load";
        321 -> "bpf";
        322 -> "execveat";
        323 -> "userfaultfd";
        324 -> "membarrier";
        325 -> "mlock2";
        326 -> "copy_file_range";
        327 -> "preadv2";
        328 -> "pwritev2";
        329 -> "pkey_mprotect";
        330 -> "pkey_alloc";
        331 -> "pkey_free";
        332 -> "statx";
        333 -> "io_pgetevents";
        334 -> "rseq";
        424 -> "pidfd_send_signal";
        425 -> "io_uring_setup";
        426 -> "io_uring_enter";
        427 -> "io_uring_register";
        428 -> "open_tree";
        429 -> "move_mount";
        430 -> "fsopen";
        431 -> "fsconfig";
        432 -> "fsmount";
        433 -> "fspick";
        434 -> "pidfd_open";
        435 -> "clone3";
        436 -> "close_range";
        437 -> "openat2";
        438 -> "pidfd_getfd";
        439 -> "faccessat2";
        440 -> "process_madvise";
        441 -> "epoll_pwait2";
        442 -> "mount_setattr";
        443 -> "quotactl_fd";
        444 -> "landlock_create_ruleset";
        445 -> "landlock_add_rule";
        446 -> "landlock_restrict_self";
        447 -> "memfd_secret";
        448 -> "process_mrelease";
        449 -> "futex_waitv";
        450 -> "set_mempolicy_home_node";
        _ -> io_lib:format("unknown_~p", [Nr])
    end.
