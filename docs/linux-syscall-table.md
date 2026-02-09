# Linux x86_64 Syscall Table

Complete reference for the V-on-BEAM Linux personality layer.

## Implementation Status Legend

- **DONE**: Fully implemented and routed to BEAM services
- **STUB**: Returns -ENOSYS with first-call logging
- **TODO**: Planned for implementation

## Critical Syscalls (Implemented)

These are the ~50 syscalls essential for BusyBox and basic Linux userspace:

### File I/O (vbeam_vfs)
| Nr  | Name      | Args | Status | Notes |
|-----|-----------|------|--------|-------|
| 0   | read      | fd, buf, count | DONE | Read from file descriptor |
| 1   | write     | fd, buf, count | DONE | Write to file descriptor |
| 2   | open      | path, flags, mode | DONE | Open file (legacy) |
| 3   | close     | fd | DONE | Close file descriptor |
| 8   | lseek     | fd, offset, whence | DONE | Reposition file offset |
| 257 | openat    | dirfd, path, flags, mode | DONE | Open file (modern) |

### Memory Management (vbeam_vmm)
| Nr  | Name      | Args | Status | Notes |
|-----|-----------|------|--------|-------|
| 9   | mmap      | addr, len, prot, flags, fd, off | DONE | Map memory |
| 10  | mprotect  | addr, len, prot | DONE | Set memory protection |
| 11  | munmap    | addr, len | DONE | Unmap memory |
| 12  | brk       | addr | DONE | Change data segment size |

### Process Management (vbeam_proc_table)
| Nr  | Name       | Args | Status | Notes |
|-----|------------|------|--------|-------|
| 56  | clone      | flags, stack, ptid, ctid, regs | DONE | Create child process |
| 57  | fork       | - | DONE | Create child (legacy) |
| 59  | execve     | path, argv, envp | DONE | Execute program |
| 60  | exit       | status | DONE | Exit current process |
| 61  | wait4      | pid, status, options, rusage | DONE | Wait for process |
| 231 | exit_group | status | DONE | Exit all threads |

### Process Identity
| Nr  | Name    | Args | Status | Notes |
|-----|---------|------|--------|-------|
| 39  | getpid  | - | DONE | Get process ID |
| 110 | getppid | - | DONE | Get parent process ID |
| 102 | getuid  | - | DONE | Get user ID |
| 104 | getgid  | - | DONE | Get group ID |
| 107 | geteuid | - | DONE | Get effective user ID |
| 108 | getegid | - | DONE | Get effective group ID |

### Signals (vbeam_signal)
| Nr  | Name           | Args | Status | Notes |
|-----|----------------|------|--------|-------|
| 13  | rt_sigaction   | sig, act, oldact, sigsetsize | DONE | Set signal handler |
| 14  | rt_sigprocmask | how, set, oldset, sigsetsize | DONE | Set signal mask |

### Filesystem Metadata (vbeam_vfs)
| Nr  | Name        | Args | Status | Notes |
|-----|-------------|------|--------|-------|
| 4   | stat        | path, statbuf | DONE | Get file status |
| 5   | fstat       | fd, statbuf | DONE | Get file status by fd |
| 6   | lstat       | path, statbuf | DONE | Get file status (no follow) |
| 78  | getdents    | fd, dirp, count | DONE | Get directory entries |
| 217 | getdents64  | fd, dirp, count | DONE | Get directory entries (64-bit) |
| 262 | newfstatat  | dirfd, path, statbuf, flags | DONE | Get file status (modern) |

### Architecture-Specific
| Nr  | Name       | Args | Status | Notes |
|-----|------------|------|--------|-------|
| 158 | arch_prctl | code, addr | DONE | Set TLS (FS/GS registers) |

### Time (vbeam_time)
| Nr  | Name           | Args | Status | Notes |
|-----|----------------|------|--------|-------|
| 35  | nanosleep      | req, rem | DONE | Sleep with nanosecond precision |
| 228 | clock_gettime  | clk_id, tp | DONE | Get time from clock |

### Pipes and Duplication (vbeam_vfs)
| Nr  | Name  | Args | Status | Notes |
|-----|-------|------|--------|-------|
| 22  | pipe  | pipefd | DONE | Create pipe |
| 293 | pipe2 | pipefd, flags | DONE | Create pipe with flags |
| 32  | dup   | oldfd | DONE | Duplicate fd |
| 33  | dup2  | oldfd, newfd | DONE | Duplicate fd to specific number |

### I/O Control (vbeam_vfs)
| Nr  | Name  | Args | Status | Notes |
|-----|-------|------|--------|-------|
| 16  | ioctl | fd, cmd, arg | DONE | I/O control operations |

---

## Complete x86_64 Syscall Table (450+ syscalls)

### 0-99: Core System Calls

| Nr | Name | Args | Status | Notes |
|----|------|------|--------|-------|
| 0 | read | fd, buf, count | DONE | |
| 1 | write | fd, buf, count | DONE | |
| 2 | open | path, flags, mode | DONE | Obsolete, use openat |
| 3 | close | fd | DONE | |
| 4 | stat | path, statbuf | DONE | |
| 5 | fstat | fd, statbuf | DONE | |
| 6 | lstat | path, statbuf | DONE | |
| 7 | poll | fds, nfds, timeout | STUB | |
| 8 | lseek | fd, offset, whence | DONE | |
| 9 | mmap | addr, len, prot, flags, fd, off | DONE | |
| 10 | mprotect | addr, len, prot | DONE | |
| 11 | munmap | addr, len | DONE | |
| 12 | brk | addr | DONE | |
| 13 | rt_sigaction | sig, act, oldact, sigsetsize | DONE | |
| 14 | rt_sigprocmask | how, set, oldset, sigsetsize | DONE | |
| 15 | rt_sigreturn | - | STUB | |
| 16 | ioctl | fd, cmd, arg | DONE | |
| 17 | pread64 | fd, buf, count, pos | STUB | |
| 18 | pwrite64 | fd, buf, count, pos | STUB | |
| 19 | readv | fd, iov, iovcnt | STUB | |
| 20 | writev | fd, iov, iovcnt | STUB | |
| 21 | access | path, mode | STUB | |
| 22 | pipe | pipefd | DONE | |
| 23 | select | nfds, readfds, writefds, exceptfds, timeout | STUB | |
| 24 | sched_yield | - | STUB | |
| 25 | mremap | old_addr, old_size, new_size, flags, new_addr | STUB | |
| 26 | msync | addr, len, flags | STUB | |
| 27 | mincore | addr, len, vec | STUB | |
| 28 | madvise | addr, len, advice | STUB | |
| 29 | shmget | key, size, shmflg | STUB | |
| 30 | shmat | shmid, shmaddr, shmflg | STUB | |
| 31 | shmctl | shmid, cmd, buf | STUB | |
| 32 | dup | oldfd | DONE | |
| 33 | dup2 | oldfd, newfd | DONE | |
| 34 | pause | - | STUB | |
| 35 | nanosleep | req, rem | DONE | |
| 36 | getitimer | which, curr_value | STUB | |
| 37 | alarm | seconds | STUB | |
| 38 | setitimer | which, new_value, old_value | STUB | |
| 39 | getpid | - | DONE | |
| 40 | sendfile | out_fd, in_fd, offset, count | STUB | |
| 41 | socket | family, type, protocol | STUB | TODO: networking |
| 42 | connect | sockfd, addr, addrlen | STUB | TODO: networking |
| 43 | accept | sockfd, addr, addrlen | STUB | TODO: networking |
| 44 | sendto | sockfd, buf, len, flags, dest_addr, addrlen | STUB | TODO: networking |
| 45 | recvfrom | sockfd, buf, len, flags, src_addr, addrlen | STUB | TODO: networking |
| 46 | sendmsg | sockfd, msg, flags | STUB | TODO: networking |
| 47 | recvmsg | sockfd, msg, flags | STUB | TODO: networking |
| 48 | shutdown | sockfd, how | STUB | TODO: networking |
| 49 | bind | sockfd, addr, addrlen | STUB | TODO: networking |
| 50 | listen | sockfd, backlog | STUB | TODO: networking |
| 51 | getsockname | sockfd, addr, addrlen | STUB | TODO: networking |
| 52 | getpeername | sockfd, addr, addrlen | STUB | TODO: networking |
| 53 | socketpair | family, type, protocol, sv | STUB | TODO: networking |
| 54 | setsockopt | sockfd, level, optname, optval, optlen | STUB | TODO: networking |
| 55 | getsockopt | sockfd, level, optname, optval, optlen | STUB | TODO: networking |
| 56 | clone | flags, stack, ptid, ctid, regs | DONE | |
| 57 | fork | - | DONE | |
| 58 | vfork | - | STUB | Use clone |
| 59 | execve | path, argv, envp | DONE | |
| 60 | exit | status | DONE | |
| 61 | wait4 | pid, status, options, rusage | DONE | |
| 62 | kill | pid, sig | STUB | TODO: signals |
| 63 | uname | buf | STUB | TODO: system info |
| 64 | semget | key, nsems, semflg | STUB | |
| 65 | semop | semid, sops, nsops | STUB | |
| 66 | semctl | semid, semnum, cmd, arg | STUB | |
| 67 | shmdt | shmaddr | STUB | |
| 68 | msgget | key, msgflg | STUB | |
| 69 | msgsnd | msqid, msgp, msgsz, msgflg | STUB | |
| 70 | msgrcv | msqid, msgp, msgsz, msgtyp, msgflg | STUB | |
| 71 | msgctl | msqid, cmd, buf | STUB | |
| 72 | fcntl | fd, cmd, arg | STUB | TODO: critical for dup |
| 73 | flock | fd, operation | STUB | |
| 74 | fsync | fd | STUB | |
| 75 | fdatasync | fd | STUB | |
| 76 | truncate | path, length | STUB | |
| 77 | ftruncate | fd, length | STUB | |
| 78 | getdents | fd, dirp, count | DONE | |
| 79 | getcwd | buf, size | STUB | TODO: needed |
| 80 | chdir | path | STUB | TODO: needed |
| 81 | fchdir | fd | STUB | |
| 82 | rename | oldpath, newpath | STUB | |
| 83 | mkdir | path, mode | STUB | TODO: needed |
| 84 | rmdir | path | STUB | |
| 85 | creat | path, mode | STUB | Use openat |
| 86 | link | oldpath, newpath | STUB | |
| 87 | unlink | path | STUB | TODO: needed |
| 88 | symlink | target, linkpath | STUB | |
| 89 | readlink | path, buf, bufsiz | STUB | |
| 90 | chmod | path, mode | STUB | |
| 91 | fchmod | fd, mode | STUB | |
| 92 | chown | path, owner, group | STUB | |
| 93 | fchown | fd, owner, group | STUB | |
| 94 | lchown | path, owner, group | STUB | |
| 95 | umask | mask | STUB | |
| 96 | gettimeofday | tv, tz | STUB | Use clock_gettime |
| 97 | getrlimit | resource, rlim | STUB | TODO: needed |
| 98 | getrusage | who, usage | STUB | |
| 99 | sysinfo | info | STUB | |

### 100-199: Extended System Calls

| Nr | Name | Args | Status | Notes |
|----|------|------|--------|-------|
| 100 | times | buf | STUB | |
| 101 | ptrace | request, pid, addr, data | STUB | |
| 102 | getuid | - | DONE | |
| 103 | syslog | type, bufp, len | STUB | |
| 104 | getgid | - | DONE | |
| 105 | setuid | uid | STUB | |
| 106 | setgid | gid | STUB | |
| 107 | geteuid | - | DONE | |
| 108 | getegid | - | DONE | |
| 109 | setpgid | pid, pgid | STUB | |
| 110 | getppid | - | DONE | |
| 111 | getpgrp | - | STUB | |
| 112 | setsid | - | STUB | |
| 113 | setreuid | ruid, euid | STUB | |
| 114 | setregid | rgid, egid | STUB | |
| 115 | getgroups | size, list | STUB | |
| 116 | setgroups | size, list | STUB | |
| 117 | setresuid | ruid, euid, suid | STUB | |
| 118 | getresuid | ruid, euid, suid | STUB | |
| 119 | setresgid | rgid, egid, sgid | STUB | |
| 120 | getresgid | rgid, egid, sgid | STUB | |
| 121 | getpgid | pid | STUB | |
| 122 | setfsuid | fsuid | STUB | |
| 123 | setfsgid | fsgid | STUB | |
| 124 | getsid | pid | STUB | |
| 125 | capget | hdrp, datap | STUB | |
| 126 | capset | hdrp, datap | STUB | |
| 127 | rt_sigpending | set, sigsetsize | STUB | |
| 128 | rt_sigtimedwait | set, info, timeout, sigsetsize | STUB | |
| 129 | rt_sigqueueinfo | pid, sig, info | STUB | |
| 130 | rt_sigsuspend | mask, sigsetsize | STUB | |
| 131 | sigaltstack | ss, old_ss | STUB | |
| 132 | utime | filename, times | STUB | |
| 133 | mknod | path, mode, dev | STUB | |
| 134 | uselib | library | STUB | Obsolete |
| 135 | personality | persona | STUB | |
| 136 | ustat | dev, ubuf | STUB | Obsolete |
| 137 | statfs | path, buf | STUB | |
| 138 | fstatfs | fd, buf | STUB | |
| 139 | sysfs | option, arg1, arg2 | STUB | Obsolete |
| 140 | getpriority | which, who | STUB | |
| 141 | setpriority | which, who, prio | STUB | |
| 142 | sched_setparam | pid, param | STUB | |
| 143 | sched_getparam | pid, param | STUB | |
| 144 | sched_setscheduler | pid, policy, param | STUB | |
| 145 | sched_getscheduler | pid | STUB | |
| 146 | sched_get_priority_max | policy | STUB | |
| 147 | sched_get_priority_min | policy | STUB | |
| 148 | sched_rr_get_interval | pid, interval | STUB | |
| 149 | mlock | addr, len | STUB | |
| 150 | munlock | addr, len | STUB | |
| 151 | mlockall | flags | STUB | |
| 152 | munlockall | - | STUB | |
| 153 | vhangup | - | STUB | |
| 154 | modify_ldt | func, ptr, bytecount | STUB | |
| 155 | pivot_root | new_root, put_old | STUB | |
| 156 | _sysctl | args | STUB | Obsolete |
| 157 | prctl | option, arg2, arg3, arg4, arg5 | STUB | TODO: needed |
| 158 | arch_prctl | code, addr | DONE | |
| 159 | adjtimex | buf | STUB | |
| 160 | setrlimit | resource, rlim | STUB | |
| 161 | chroot | path | STUB | |
| 162 | sync | - | STUB | |
| 163 | acct | filename | STUB | |
| 164 | settimeofday | tv, tz | STUB | |
| 165 | mount | source, target, fstype, flags, data | STUB | TODO: critical |
| 166 | umount2 | target, flags | STUB | |
| 167 | swapon | path, swapflags | STUB | |
| 168 | swapoff | path | STUB | |
| 169 | reboot | magic, magic2, cmd, arg | STUB | |
| 170 | sethostname | name, len | STUB | |
| 171 | setdomainname | name, len | STUB | |
| 172 | iopl | level | STUB | |
| 173 | ioperm | from, num, turn_on | STUB | |
| 174 | create_module | name, size | STUB | Obsolete |
| 175 | init_module | module_image, len, param_values | STUB | |
| 176 | delete_module | name, flags | STUB | |
| 177 | get_kernel_syms | table | STUB | Obsolete |
| 178 | query_module | name, which, buf, bufsize, ret | STUB | Obsolete |
| 179 | quotactl | cmd, special, id, addr | STUB | |
| 180 | nfsservctl | cmd, argp, resp | STUB | Obsolete |
| 181 | getpmsg | fd, ctlptr, dataptr, bandp, flagsp | STUB | Unimplemented |
| 182 | putpmsg | fd, ctlptr, dataptr, band, flags | STUB | Unimplemented |
| 183 | afs_syscall | - | STUB | Unimplemented |
| 184 | tuxcall | - | STUB | Unimplemented |
| 185 | security | - | STUB | Unimplemented |
| 186 | gettid | - | STUB | TODO: threading |
| 187 | readahead | fd, offset, count | STUB | |
| 188 | setxattr | path, name, value, size, flags | STUB | |
| 189 | lsetxattr | path, name, value, size, flags | STUB | |
| 190 | fsetxattr | fd, name, value, size, flags | STUB | |
| 191 | getxattr | path, name, value, size | STUB | |
| 192 | lgetxattr | path, name, value, size | STUB | |
| 193 | fgetxattr | fd, name, value, size | STUB | |
| 194 | listxattr | path, list, size | STUB | |
| 195 | llistxattr | path, list, size | STUB | |
| 196 | flistxattr | fd, list, size | STUB | |
| 197 | removexattr | path, name | STUB | |
| 198 | lremovexattr | path, name | STUB | |
| 199 | fremovexattr | fd, name | STUB | |

### 200-299: Modern Extensions

| Nr | Name | Args | Status | Notes |
|----|------|------|--------|-------|
| 200 | tkill | tid, sig | STUB | |
| 201 | time | tloc | STUB | Use clock_gettime |
| 202 | futex | uaddr, op, val, timeout, uaddr2, val3 | STUB | TODO: threading critical |
| 203 | sched_setaffinity | pid, len, mask | STUB | |
| 204 | sched_getaffinity | pid, len, mask | STUB | |
| 205 | set_thread_area | u_info | STUB | |
| 206 | io_setup | nr_events, ctxp | STUB | |
| 207 | io_destroy | ctx | STUB | |
| 208 | io_getevents | ctx, min_nr, nr, events, timeout | STUB | |
| 209 | io_submit | ctx, nr, iocbpp | STUB | |
| 210 | io_cancel | ctx, iocb, result | STUB | |
| 211 | get_thread_area | u_info | STUB | |
| 212 | lookup_dcookie | cookie, buffer, len | STUB | |
| 213 | epoll_create | size | STUB | TODO: event loops |
| 214 | epoll_ctl_old | - | STUB | Obsolete |
| 215 | epoll_wait_old | - | STUB | Obsolete |
| 216 | remap_file_pages | addr, size, prot, pgoff, flags | STUB | |
| 217 | getdents64 | fd, dirp, count | DONE | |
| 218 | set_tid_address | tidptr | STUB | TODO: threading |
| 219 | restart_syscall | - | STUB | |
| 220 | semtimedop | semid, sops, nsops, timeout | STUB | |
| 221 | fadvise64 | fd, offset, len, advice | STUB | |
| 222 | timer_create | clockid, sevp, timerid | STUB | |
| 223 | timer_settime | timerid, flags, new_value, old_value | STUB | |
| 224 | timer_gettime | timerid, curr_value | STUB | |
| 225 | timer_getoverrun | timerid | STUB | |
| 226 | timer_delete | timerid | STUB | |
| 227 | clock_settime | clockid, tp | STUB | |
| 228 | clock_gettime | clockid, tp | DONE | |
| 229 | clock_getres | clockid, res | STUB | |
| 230 | clock_nanosleep | clockid, flags, request, remain | STUB | |
| 231 | exit_group | status | DONE | |
| 232 | epoll_wait | epfd, events, maxevents, timeout | STUB | TODO: event loops |
| 233 | epoll_ctl | epfd, op, fd, event | STUB | TODO: event loops |
| 234 | tgkill | tgid, tid, sig | STUB | |
| 235 | utimes | filename, times | STUB | |
| 236 | vserver | - | STUB | Unimplemented |
| 237 | mbind | addr, len, mode, nodemask, maxnode, flags | STUB | |
| 238 | set_mempolicy | mode, nodemask, maxnode | STUB | |
| 239 | get_mempolicy | mode, nodemask, maxnode, addr, flags | STUB | |
| 240 | mq_open | name, oflag, mode, attr | STUB | |
| 241 | mq_unlink | name | STUB | |
| 242 | mq_timedsend | mqdes, msg_ptr, msg_len, msg_prio, abs_timeout | STUB | |
| 243 | mq_timedreceive | mqdes, msg_ptr, msg_len, msg_prio, abs_timeout | STUB | |
| 244 | mq_notify | mqdes, notification | STUB | |
| 245 | mq_getsetattr | mqdes, newattr, oldattr | STUB | |
| 246 | kexec_load | entry, nr_segments, segments, flags | STUB | |
| 247 | waitid | idtype, id, infop, options, rusage | STUB | |
| 248 | add_key | type, description, payload, plen, keyring | STUB | |
| 249 | request_key | type, description, callout_info, keyring | STUB | |
| 250 | keyctl | operation, arg2, arg3, arg4, arg5 | STUB | |
| 251 | ioprio_set | which, who, ioprio | STUB | |
| 252 | ioprio_get | which, who | STUB | |
| 253 | inotify_init | - | STUB | |
| 254 | inotify_add_watch | fd, pathname, mask | STUB | |
| 255 | inotify_rm_watch | fd, wd | STUB | |
| 256 | migrate_pages | pid, maxnode, old_nodes, new_nodes | STUB | |
| 257 | openat | dirfd, pathname, flags, mode | DONE | |
| 258 | mkdirat | dirfd, pathname, mode | STUB | |
| 259 | mknodat | dirfd, pathname, mode, dev | STUB | |
| 260 | fchownat | dirfd, pathname, owner, group, flags | STUB | |
| 261 | futimesat | dirfd, pathname, times | STUB | |
| 262 | newfstatat | dirfd, pathname, statbuf, flags | DONE | |
| 263 | unlinkat | dirfd, pathname, flags | STUB | |
| 264 | renameat | olddirfd, oldpath, newdirfd, newpath | STUB | |
| 265 | linkat | olddirfd, oldpath, newdirfd, newpath, flags | STUB | |
| 266 | symlinkat | target, newdirfd, linkpath | STUB | |
| 267 | readlinkat | dirfd, pathname, buf, bufsiz | STUB | |
| 268 | fchmodat | dirfd, pathname, mode, flags | STUB | |
| 269 | faccessat | dirfd, pathname, mode, flags | STUB | |
| 270 | pselect6 | nfds, readfds, writefds, exceptfds, timeout, sigmask | STUB | |
| 271 | ppoll | fds, nfds, timeout_ts, sigmask, sigsetsize | STUB | |
| 272 | unshare | flags | STUB | |
| 273 | set_robust_list | head, len | STUB | |
| 274 | get_robust_list | pid, head_ptr, len_ptr | STUB | |
| 275 | splice | fd_in, off_in, fd_out, off_out, len, flags | STUB | |
| 276 | tee | fd_in, fd_out, len, flags | STUB | |
| 277 | sync_file_range | fd, offset, nbytes, flags | STUB | |
| 278 | vmsplice | fd, iov, nr_segs, flags | STUB | |
| 279 | move_pages | pid, nr_pages, pages, nodes, status, flags | STUB | |
| 280 | utimensat | dirfd, pathname, times, flags | STUB | |
| 281 | epoll_pwait | epfd, events, maxevents, timeout, sigmask, sigsetsize | STUB | |
| 282 | signalfd | ufd, mask, flags | STUB | |
| 283 | timerfd_create | clockid, flags | STUB | |
| 284 | eventfd | initval | STUB | |
| 285 | fallocate | fd, mode, offset, len | STUB | |
| 286 | timerfd_settime | fd, flags, new_value, old_value | STUB | |
| 287 | timerfd_gettime | fd, curr_value | STUB | |
| 288 | accept4 | sockfd, addr, addrlen, flags | STUB | |
| 289 | signalfd4 | ufd, mask, flags | STUB | |
| 290 | eventfd2 | initval, flags | STUB | |
| 291 | epoll_create1 | flags | STUB | |
| 292 | dup3 | oldfd, newfd, flags | STUB | |
| 293 | pipe2 | pipefd, flags | DONE | |
| 294 | inotify_init1 | flags | STUB | |
| 295 | preadv | fd, iov, iovcnt, pos_l, pos_h | STUB | |
| 296 | pwritev | fd, iov, iovcnt, pos_l, pos_h | STUB | |
| 297 | rt_tgsigqueueinfo | tgid, tid, sig, uinfo | STUB | |
| 298 | perf_event_open | attr, pid, cpu, group_fd, flags | STUB | |
| 299 | recvmmsg | sockfd, msgvec, vlen, flags, timeout | STUB | |

### 300-450: Recent Extensions

| Nr | Name | Args | Status | Notes |
|----|------|------|--------|-------|
| 300 | fanotify_init | flags, event_f_flags | STUB | |
| 301 | fanotify_mark | fanotify_fd, flags, mask, dirfd, pathname | STUB | |
| 302 | prlimit64 | pid, resource, new_limit, old_limit | STUB | |
| 303 | name_to_handle_at | dirfd, pathname, handle, mount_id, flags | STUB | |
| 304 | open_by_handle_at | mount_fd, handle, flags | STUB | |
| 305 | clock_adjtime | clockid, buf | STUB | |
| 306 | syncfs | fd | STUB | |
| 307 | sendmmsg | sockfd, msgvec, vlen, flags | STUB | |
| 308 | setns | fd, nstype | STUB | |
| 309 | getcpu | cpu, node, tcache | STUB | |
| 310 | process_vm_readv | pid, local_iov, liovcnt, remote_iov, riovcnt, flags | STUB | |
| 311 | process_vm_writev | pid, local_iov, liovcnt, remote_iov, riovcnt, flags | STUB | |
| 312 | kcmp | pid1, pid2, type, idx1, idx2 | STUB | |
| 313 | finit_module | fd, param_values, flags | STUB | |
| 314 | sched_setattr | pid, attr, flags | STUB | |
| 315 | sched_getattr | pid, attr, size, flags | STUB | |
| 316 | renameat2 | olddirfd, oldpath, newdirfd, newpath, flags | STUB | |
| 317 | seccomp | operation, flags, args | STUB | |
| 318 | getrandom | buf, buflen, flags | STUB | TODO: needed |
| 319 | memfd_create | name, flags | STUB | |
| 320 | kexec_file_load | kernel_fd, initrd_fd, cmdline_len, cmdline, flags | STUB | |
| 321 | bpf | cmd, attr, size | STUB | |
| 322 | execveat | dirfd, pathname, argv, envp, flags | STUB | |
| 323 | userfaultfd | flags | STUB | |
| 324 | membarrier | cmd, flags, cpu_id | STUB | |
| 325 | mlock2 | addr, len, flags | STUB | |
| 326 | copy_file_range | fd_in, off_in, fd_out, off_out, len, flags | STUB | |
| 327 | preadv2 | fd, iov, iovcnt, pos_l, pos_h, flags | STUB | |
| 328 | pwritev2 | fd, iov, iovcnt, pos_l, pos_h, flags | STUB | |
| 329 | pkey_mprotect | addr, len, prot, pkey | STUB | |
| 330 | pkey_alloc | flags, access_rights | STUB | |
| 331 | pkey_free | pkey | STUB | |
| 332 | statx | dirfd, pathname, flags, mask, statxbuf | STUB | |
| 333 | io_pgetevents | ctx_id, min_nr, nr, events, timeout, sig | STUB | |
| 334 | rseq | rseq, rseq_len, flags, sig | STUB | |
| 424 | pidfd_send_signal | pidfd, sig, info, flags | STUB | |
| 425 | io_uring_setup | entries, params | STUB | |
| 426 | io_uring_enter | fd, to_submit, min_complete, flags, sig, sigsz | STUB | |
| 427 | io_uring_register | fd, opcode, arg, nr_args | STUB | |
| 428 | open_tree | dirfd, pathname, flags | STUB | |
| 429 | move_mount | from_dirfd, from_pathname, to_dirfd, to_pathname, flags | STUB | |
| 430 | fsopen | fsname, flags | STUB | |
| 431 | fsconfig | fd, cmd, key, value, aux | STUB | |
| 432 | fsmount | fd, flags, attr_flags | STUB | |
| 433 | fspick | dirfd, pathname, flags | STUB | |
| 434 | pidfd_open | pid, flags | STUB | |
| 435 | clone3 | cl_args, size | STUB | |
| 436 | close_range | first, last, flags | STUB | |
| 437 | openat2 | dirfd, pathname, how, size | STUB | |
| 438 | pidfd_getfd | pidfd, targetfd, flags | STUB | |
| 439 | faccessat2 | dirfd, pathname, mode, flags | STUB | |
| 440 | process_madvise | pidfd, iovec, vlen, advice, flags | STUB | |
| 441 | epoll_pwait2 | epfd, events, maxevents, timeout, sigmask, sigsetsize | STUB | |
| 442 | mount_setattr | dirfd, pathname, flags, attr, size | STUB | |
| 443 | quotactl_fd | fd, cmd, id, addr | STUB | |
| 444 | landlock_create_ruleset | attr, size, flags | STUB | |
| 445 | landlock_add_rule | ruleset_fd, rule_type, rule_attr, flags | STUB | |
| 446 | landlock_restrict_self | ruleset_fd, flags | STUB | |
| 447 | memfd_secret | flags | STUB | |
| 448 | process_mrelease | pidfd, flags | STUB | |
| 449 | futex_waitv | waiters, nr_futexes, flags, timeout, clockid | STUB | |
| 450 | set_mempolicy_home_node | addr, len, home_node, flags | STUB | |

---

## Implementation Priority Queue

### Phase 1: BusyBox Minimum (DONE)
- ✅ File I/O (read, write, open, close, lseek)
- ✅ Memory (mmap, munmap, mprotect, brk)
- ✅ Process (clone, fork, execve, exit, wait4)
- ✅ Identity (getpid, getuid, etc.)
- ✅ Signals (rt_sigaction, rt_sigprocmask - minimal)
- ✅ Filesystem metadata (stat, fstat, getdents64)
- ✅ Architecture (arch_prctl for TLS)
- ✅ Time (nanosleep, clock_gettime)
- ✅ Pipes (pipe, pipe2, dup, dup2)

### Phase 2: Shell and Basic Tools
- [ ] 72: fcntl (critical for file descriptor flags)
- [ ] 79: getcwd (current working directory)
- [ ] 80: chdir (change directory)
- [ ] 83: mkdir (create directories)
- [ ] 87: unlink (delete files)
- [ ] 63: uname (system information)
- [ ] 97: getrlimit (resource limits)
- [ ] 318: getrandom (random numbers)

### Phase 3: Networking
- [ ] 41-55: socket operations (socket, bind, listen, connect, accept, send, recv)

### Phase 4: Threading
- [ ] 202: futex (critical for threading)
- [ ] 186: gettid (thread ID)
- [ ] 218: set_tid_address (thread setup)

### Phase 5: Event Loops
- [ ] 213: epoll_create
- [ ] 233: epoll_ctl
- [ ] 232: epoll_wait

---

## BEAM Service Routing

Syscalls are routed to these BEAM services:

| Service | Syscalls Handled | Module |
|---------|-----------------|--------|
| vbeam_vfs | File I/O, pipes, ioctl | File system operations |
| vbeam_vmm | mmap, munmap, mprotect, brk | Virtual memory manager |
| vbeam_proc_table | clone, fork, execve, exit, wait4 | Process table |
| vbeam_signal | rt_sigaction, rt_sigprocmask | Signal handling |
| vbeam_time | nanosleep, clock_gettime | Time services |

---

## Error Codes (errno)

Common Linux error codes returned:

| Code | Name | Value | Meaning |
|------|------|-------|---------|
| ENOSYS | Function not implemented | 38 | Syscall not implemented |
| EINVAL | Invalid argument | 22 | Invalid argument |
| EBADF | Bad file descriptor | 9 | Invalid fd |
| ENOMEM | Out of memory | 12 | Memory allocation failed |
| EFAULT | Bad address | 14 | Invalid memory address |
| EACCES | Permission denied | 13 | Access denied |
| ENOENT | No such file or directory | 2 | File not found |

---

## Testing

To trace which syscalls are actually called:

```erlang
1> vbeam_linux_syscall:init().
ok
2> vbeam_linux_syscall:dispatch(0, [3, <<>>, 1024]).
[vbeam] UNIMPLEMENTED SYSCALL: read (0) args=[3,<<>>,1024]
{error,38}
```

The first call to each unimplemented syscall will log. Subsequent calls are silent (to avoid spam).

---

## References

- [Linux Syscall Table (x86_64)](https://filippo.io/linux-syscall-table/)
- [syscalls(2) man page](https://man7.org/linux/man-pages/man2/syscalls.2.html)
- [Chromium OS syscall docs](https://chromium.googlesource.com/chromiumos/docs/+/master/constants/syscalls.md)
