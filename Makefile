root :=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
tests :=$(root)/tests
src := $(root)/src
tmp := $(root)/tmp

nrepl:
	guix shell \
	guix \
	guile-next \
	guile-ares-rs \
	-- guile \
	-l nrepl.scm

check:
	if [ -d $(tmp) ]; then rm -rf $(tmp); fi
	guile --no-auto-compile -L $(tests) -L $(src) $(tests)/runner.scm

guix-shell/check:
	if [ -d $(tmp) ]; then rm -rf $(tmp); fi
	guix shell guix guile-next guile-ares-rs -- \
	  guile --no-auto-compile -L $(tests) -L $(src) $(tests)/runner.scm

system/check:
	guix build -L $(src) -L $(tests) --no-grafts \
	  -e '(@ (x-files tests services datomic) %test-datomic-dev)'
	guix build -L $(src) -L $(tests) --no-grafts \
	  -e '(@ (x-files tests services datomic) %test-datomic-postgres)'

system-test-datomic-dev:
	guix build -L $(src) -L $(tests) --no-grafts \
	  -e '(@ (x-files tests services datomic) %test-datomic-dev)'

system-test-datomic-postgres:
	guix build -L $(src) -L $(tests) --no-grafts \
	  -e '(@ (x-files tests services datomic) %test-datomic-postgres)'

system-test-datomic-backup:
	guix build -L $(src) -L $(tests) --no-grafts \
	  -e '(@ (x-files tests services datomic-backup) %test-datomic-backup)'

system-test-datomic-backup-names:
	guix build -L $(src) -L $(tests) --no-grafts \
	  -e '(@ (x-files tests services datomic-backup) %test-datomic-backup-names)'

system-test-clickhouse:
	guix build -L $(src) -L $(tests) --no-grafts \
	  -e '(@ (x-files tests services clickhouse) %test-clickhouse)'

system-test-remark42:
	guix build -L $(src) -L $(tests) --no-grafts --no-offload \
	  -e '(@ (x-files tests services remark42) %test-remark42)'

system-test-stalwart:
	guix build -L $(src) -L $(tests) --no-grafts --no-offload \
	  -e '(@ (x-files tests services stalwart) %test-stalwart)'

# --- OnePlus 6T (fajita) Phosh VM ----------------------------------------
# The VM cannot emulate the Snapdragon 845; it exercises the transferable
# software stack (greetd autologin -> Phosh/phoc, logind, mobile services).
# aarch64 matches the device CPU but runs under TCG (no KVM) and is slow;
# build/run x86_64 instead by overriding ARCH (e.g. make ARCH=x86_64-linux ...).
ARCH        := aarch64-linux
oneplus6t-os := '(@ (x-files systems oneplus-6t-vm) oneplus-6t-vm-os)'

image-oneplus-6t:
	guix system image -L $(src) --image-type=efi-raw --system=$(ARCH) \
	  -e $(oneplus6t-os)

run-oneplus-6t:
	@img=$$(guix system image -L $(src) --image-type=efi-raw --system=$(ARCH) -e $(oneplus6t-os)); \
	qemu=$$(guix build qemu | grep -vE '(-doc|-static)$$' | grep 'qemu-[0-9]'); \
	code=$$qemu/share/qemu/edk2-aarch64-code.fd; \
	disk=$(tmp)/oneplus-6t.qcow2; vars=$(tmp)/oneplus-6t-vars.fd; \
	mkdir -p $(tmp); \
	qemu-img convert -O qcow2 $$img $$disk; qemu-img resize $$disk +8G; \
	[ -f $$vars ] || truncate -s 64M $$vars; \
	echo "SSH: ssh -p 5555 phone@localhost  (password: 1234)"; \
	$$qemu/bin/qemu-system-aarch64 -M virt -cpu max -smp 4 -m 4096 \
	  -drive if=pflash,format=raw,unit=0,file=$$code,readonly=on \
	  -drive if=pflash,format=raw,unit=1,file=$$vars \
	  -drive file=$$disk,format=qcow2,if=virtio \
	  -device virtio-gpu-pci,xres=720,yres=1440 \
	  -device virtio-keyboard-pci -device virtio-tablet-pci \
	  -device virtio-net-pci,netdev=n0 -netdev user,id=n0,hostfwd=tcp::5555-:22 \
	  -display gtk
