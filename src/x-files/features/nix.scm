(define-module (x-files features nix)
  #:use-module ((guix gexp) #:select (local-file))
  #:use-module ((rde lib file) #:select (find-file-in-load-path))
  #:use-module ((rde features) #:select (feature))
  #:use-module ((rde features emacs) #:select (rde-elisp-configuration-service))
  #:use-module ((gnu services) #:select (service))
  #:use-module ((gnu services base) #:select (udev-rule
                                              udev-rules-service))
  #:use-module ((gnu services nix) #:select (nix-service-type
                                              nix-configuration))
  #:use-module ((gnu packages package-management) #:select (nix))
  #:use-module ((gnu packages emacs-xyz) #:select (emacs-envrc
                                                    emacs-nix-mode))
  #:use-module ((x-files packages emacs nix-lsp) #:select (emacs-nix-lsp))

  #:export (feature-nix-dev))

;; Throwaway/dev Nix daemon, in the spirit of feature-clickhouse-dev — lets
;; nix-build / nixos-rebuild build-vm / nixos-generators build and boot
;; NixOS VM images for dayjob testing, without installing NixOS anywhere.
;; `nix-command` + `flakes` are enabled by default since most current
;; NixOS-VM recipes (nixos-generators, flake-based configs) assume them.
;;
;; `system-features = kvm`: without this, nixosTest/nix-build VM
;; derivations (which declare `requiredSystemFeatures = ["kvm"]`) get
;; silently sandboxed away from /dev/kvm even though the nixbld* build
;; users ARE in the kvm group -- nix-daemon only bind-mounts /dev/kvm
;; into the build sandbox when "kvm" is advertised here; without it,
;; qemu falls back to software TCG emulation with zero error surfaced
;; to the user beyond a build-log line ("failed to initialize kvm:
;; Permission denied ... falling back to tcg") -- confirmed live on
;; 2026-08-17: every atlas-node devenv/nixos VM test that night ran
;; under TCG, 10-50x slower than real KVM, which was the dominant cause
;; of hours of "why is this so slow" before this was found.
;;
;; Even with `system-features = kvm` and /dev/kvm bind-mounted into the
;; sandbox, qemu STILL got "Permission denied" -- confirmed live: Nix's
;; Linux sandbox uses an unprivileged user namespace that maps only the
;; nixbld* build user's own uid/gid, not its supplementary groups, so
;; being in the `kvm` group on the host doesn't carry into the sandbox.
;; /dev/kvm's default `crw-rw---- root:kvm` mode is therefore
;; unreachable from inside any sandboxed build no matter what nix.conf
;; says. `chmod 666 /dev/kvm` (imperative, live-tested) fixed it
;; instantly (2min VM test run vs 26-60+ min under TCG) but doesn't
;; survive reboots or reconfigures re-triggering udev. This udev rule
;; is the declarative, persistent version of that same fix -- MODE
;; "0666" so any build sandbox can open it, sidestepping group
;; membership entirely (the standard fix for this exact class of
;; Nix+KVM sandboxing limitation, same idea as
;; system/services/falcon-qemu.scm's udev rule for /dev/falcon-usb).
(define %nix-kvm-udev-rules
  (udev-rules-service 'nix-kvm-access
    (udev-rule "99-nix-kvm.rules"
      "KERNEL==\"kvm\", GROUP=\"kvm\", MODE=\"0666\"")))

;; nixd (evaluation-backed Nix LSP, reflecting this same feature's live
;; nix-daemon) wired into both LSP clients this config runs -- lsp-mode and
;; eglot -- lives in its own package, (x-files packages emacs nix-lsp),
;; built from packages/aux/nix-lsp/nix-lsp.el, with the nixd store path
;; baked in at build time via `emacs-substitute-variables' rather than
;; spliced in as a gexp'd string literal here.  This service just requires
;; that file; kept as a top-level define, outside `feature-nix-dev's body,
;; since it has nothing else feature-local to close over.
(define (nix-lsp-service config)
  (rde-elisp-configuration-service
   'nix-lsp config
   '((require 'nix-lsp))
   #:elisp-packages (list emacs-nix-lsp)))

;; envrc.el: per-directory environment (direnv, e.g. `use nix'/`use flake')
;; loaded into Emacs buffers -- so LSP/compile/M-x shell-command run under
;; the SAME project-local Nix environment a terminal `cd'd there would see,
;; not just whatever this feature's own nix-daemon happens to be. Global
;; mode is envrc.el's own intended usage (it's a silent no-op in any
;; directory without a `.envrc'); its `envrc-direnv-executable' already
;; comes pre-patched to an absolute store path by Guix's own emacs-envrc
;; package, so no substitute-variables phase is needed here.
(define (nix-envrc-service config)
  (rde-elisp-configuration-service
   'nix-envrc config
   '((require 'envrc)
     (envrc-global-mode))
   #:elisp-packages (list emacs-envrc)))

;; Interactive Nix REPL (comint-based `nix-repl', bundled with nix-mode) --
;; the Nix analogue of a Lisp inferior-process REPL. `nix-repl' alone only
;; gives you the raw process + TAB-completion; nix-repl-config.el (loaded
;; below, see packages/aux/nix-repl/) adds the eval-region/eval-buffer
;; convention every other REPL integration in this config uses, and turns on
;; `nix-prettify-global-mode' (collapses /nix/store/<hash>-foo hashes to
;; /nix/store/…-foo everywhere, including the REPL's own output). No
;; keybindings are assigned; bind the commands yourself if wanted.
(define (nix-repl-service config)
  (rde-elisp-configuration-service
   'nix-repl config
   `((load-file
      ,(local-file
        (find-file-in-load-path
         "x-files/packages/aux/nix-repl/nix-repl-config.el"))))
   #:elisp-packages (list emacs-nix-mode)))

(define* (feature-nix-dev
          #:key
          (package nix)
          (sandbox? #t)
          (extra-config (list "experimental-features = nix-command flakes"
                               "system-features = kvm nixos-test benchmark big-parallel")))
  (define f-name 'nix-dev)

  (define (get-system-services config)
    (list
     (service nix-service-type
              (nix-configuration
               (package package)
               (sandbox sandbox?)
               (extra-config extra-config)))
     %nix-kvm-udev-rules))

  (define (get-home-services config)
    (list (nix-lsp-service config)
          (nix-envrc-service config)
          (nix-repl-service config)))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (system-services-getter get-system-services)
   (home-services-getter   get-home-services)))
