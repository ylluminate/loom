# Loom OS Hackathon Submission Checklist

## Pre-Submission (Before Hackathon Opens)

### Tests & Demos
- [ ] `./scripts/test_runtime.sh` — 46/46 pass
- [ ] `VBEAM_DIRECT=1 ./scripts/test_runtime.sh` — 46/46 pass (Tier 4)
- [ ] `./demos/hackathon_showcase.sh` — runs clean, no errors
- [ ] `./demos/fault_tolerance/run_demo.sh` — crash/restart visible
- [ ] `./demos/hot_reload/run_demo.sh` — code swap visible
- [ ] `./demos/pipeline/run_demo.sh` — V compile + execute works
- [ ] `make nucleus` — builds nucleus.efi
- [ ] `make qemu-test` — nucleus boots in QEMU (if QEMU available)

### Repo Hygiene
- [ ] No credentials or secrets in tracked files
- [ ] No personal paths hardcoded in public-facing files
- [ ] `.gitignore` covers build artifacts, loom/ staging
- [ ] README.md (root) is clean and points to loom/README.md or replaces it
- [ ] License file present (MIT)
- [ ] No broken symlinks

### Documentation
- [ ] loom/README.md — public-facing pitch (DONE, needs review)
- [ ] Architecture diagram or section in README
- [ ] Build instructions tested on clean checkout
- [ ] Demo instructions clear for someone who's never seen the project

### Submission Materials
- [ ] Hackathon submission form filled out
- [ ] Video recorded (if required)
- [ ] Screenshots captured (boot splash, demo output, test results)
- [ ] Project description (1-2 paragraphs) ready for form

## During Hackathon (Feb 10-16)

### Showcase Moments to Build/Polish
- [ ] V hello world on bare metal (READY — just needs clean recording)
- [ ] Fault tolerance demo (READY)
- [ ] Hot code reload demo (READY)
- [ ] Full pipeline V -> BEAM -> execute (READY)
- [ ] Boot splash screenshot/GIF (need to capture from QEMU)

### Stretch Goals (if time permits)
- [ ] Animation on boot splash (color cycling, loading bar)
- [ ] Real UEFI hardware test (not just QEMU)
- [ ] Additional V program running on bare metal beyond hello world
- [ ] Short narrated demo video

## Post-Submission
- [ ] Verify submission went through
- [ ] Save submission confirmation
- [ ] Note what we'd do differently next time
