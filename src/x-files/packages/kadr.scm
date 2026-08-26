(define-module (x-files packages kadr)
  #:use-module ((x-files packages electron)   #:select (electron-42))
  #:use-module ((guix licenses)                #:prefix license:)
  #:use-module ((guix packages)                #:select (package
                                                         origin
                                                         base32
                                                         this-package-input))
  #:use-module ((guix git-download)            #:select (git-fetch
                                                         git-reference
                                                         git-file-name))
  #:use-module ((guix build-system gnu)        #:select (gnu-build-system))
  #:use-module ((gnu packages)                 #:select (specification->package))
  #:use-module ((gnu packages node)            #:select (node))
  #:use-module ((gnu packages guile-xyz)       #:select (guile-ini
                                                        guile-smc
                                                        guile-lib))
  #:use-module (guix gexp)
  #:export (make-kadr))

;; guile-ini's (ini) module pulls in (ini fsm), which needs guile-smc and
;; guile-lib; all three must be on the builder's load path -- same set as
;; (x-files packages postman)/dconf.
(define %kadr-guile-ini-extensions (list guile-ini guile-smc guile-lib))

(define %kadr-version "0.4.4")

(define kadr-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/HelpFreedom/kadr")
          (commit (string-append "v" %kadr-version))))
    (file-name (git-file-name "kadr" %kadr-version))
    (sha256
     (base32 "0nhmcp6fddxvdp3fyfdg065jasaslkd1nfqpzkmhgzcv41zlqj2q"))))

;; kadr ships no prebuilt release -- npm/Vite/Electron source only -- and
;; its own "postinstall": "electron-rebuild -f -w node-pty" recompiles
;; node-pty's native addon against Electron's ABI.  Both npm's dependency
;; resolution and @electron/rebuild's header/binary downloads need network,
;; which normal Guix build derivations don't get.  So, same shape as
;; (x-files packages ntfyr)'s cargo vendor: do the full "npm ci" (which
;; runs node-pty's own install script and then kadr's postinstall in the
;; same lifecycle) once, in a network-enabled fixed-output derivation whose
;; hash pins the exact resulting node_modules tree; the actual package
;; build below then only ever touches that tree offline.
(define kadr-npm-vendor
  (computed-file
   "kadr-npm-vendor"
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (setenv "PATH"
                 (string-append #$node "/bin:"
                                #$(specification->package "python") "/bin:"
                                #$(specification->package "gcc-toolchain") "/bin:"
                                #$(specification->package "coreutils") "/bin:"
                                #$(specification->package "sed") "/bin:"
                                #$(specification->package "binutils") "/bin:"
                                #$(specification->package "make") "/bin:"
                                #$(specification->package "bash-minimal") "/bin:"
                                #$(specification->package "git-minimal") "/bin"))
         ;; node-pty's native addon does low-level pty/termios work that
         ;; needs kernel uapi headers (linux/types.h &c.), which glibc does
         ;; not bundle; a normal gnu-build-system package would get this
         ;; via its native-search-paths automation, but this raw gexp
         ;; builder has to set CPATH itself.
         (setenv "CPATH"
                 (string-append #$(specification->package "linux-libre-headers")
                                "/include"))
         (setenv "HOME" "/tmp")
         (setenv "npm_config_cache" "/tmp/npm-cache")
         ;; The "electron" devDependency's own postinstall would otherwise
         ;; download its ~120MB prebuilt zip too -- it is never used, since
         ;; the packaged app runs against this channel's own electron-42;
         ;; only its package.json (which @electron/rebuild reads to detect
         ;; the target ABI version) is needed, and that is written
         ;; regardless of this flag.
         (setenv "ELECTRON_SKIP_BINARY_DOWNLOAD" "1")
         (copy-recursively #$kadr-source "source")
         (for-each make-file-writable (find-files "source"))
         (chdir "source")
         ;; Guix's build sandbox has no /usr/bin/env, so npm packages'
         ;; "#!/usr/bin/env node" launchers (node-gyp, electron-rebuild,
         ;; ...) can't be exec'd directly.  --ignore-scripts skips running
         ;; any lifecycle script during install -- node-pty's own native
         ;; build is irrelevant anyway, since @electron/rebuild rebuilds it
         ;; against Electron's ABI regardless of whatever was there before
         ;; -- then every node_modules/**/.bin launcher's shebang is
         ;; rewritten to an absolute store path before invoking
         ;; electron-rebuild directly (which is exactly kadr's own
         ;; "postinstall" script).
         (invoke "npm" "ci" "--ignore-scripts" "--no-audit" "--no-fund")
         ;; node_modules/**/.bin entries are symlinks into each package's
         ;; real directory; patching the symlink path itself would replace
         ;; it with a standalone copy sitting in .bin/, breaking any
         ;; sibling-relative import the script makes from its real home
         ;; (hit exactly this with electron-rebuild's "./search-module.js").
         ;; Patch the canonicalized target instead, leaving the symlink and
         ;; the script's real location untouched.
         (for-each (lambda (file)
                     (patch-shebang (canonicalize-path file)
                                    (list (string-append #$node "/bin"))))
                   (find-files "node_modules"
                               (lambda (file stat)
                                 (string-contains file "/.bin/"))))
         (invoke "node_modules/.bin/electron-rebuild" "-f" "-w" "node-pty")
         ;; node-gyp's intermediate build/ tree (Makefiles, config.gypi,
         ;; .o/.d files) embeds this derivation's absolute build directory
         ;; and isn't needed past this point -- keep only the final
         ;; compiled addon, stripped, so this FOD's output hash doesn't
         ;; depend on non-reproducible intermediate build state.
         (let ((addon "node_modules/node-pty/build/Release/pty.node"))
           (invoke "strip" "--strip-unneeded" addon)
           (copy-file addon "/tmp/pty.node")
           (delete-file-recursively "node_modules/node-pty/build")
           (mkdir-p "node_modules/node-pty/build/Release")
           (copy-file "/tmp/pty.node" addon))
         ;; node-gyp's bundled Python (gyp) writes __pycache__/*.pyc while
         ;; running, whose header embeds the source .py's mtime -- a real
         ;; content difference between otherwise-identical runs, not just
         ;; metadata nar-hashing already ignores.  Not needed afterwards.
         (for-each delete-file-recursively
                   (find-files "node_modules"
                               (lambda (file stat)
                                 (string=? (basename file) "__pycache__"))
                               #:directories? #t))
         (copy-recursively "node_modules" #$output)))
   #:options (list #:hash-algo 'sha256
                   #:hash (base32 "0nkpqqns51pwnkadr9fjks49avbb7hln1pjm125zyah07iijjb5v")
                   #:recursive? #t)))

;; Chromium's own sandbox setup fails under Guix (no setuid chrome-sandbox
;; helper in the store), and Wayland doesn't export DISPLAY/XAUTHORITY to
;; terminals -- same fixes as (x-files packages postman)/spotify.  ffmpeg
;; is on PATH for import/audio-mix/export; faster-whisper (python3, pip
;; install) remains an optional external tool the user installs separately,
;; exactly as upstream documents it.  The embedded Claude Code panel spawns
;; a plain "claude" off PATH -- make-kadr's #:claude lets a caller supply
;; that package (e.g. claude-code from the ai-cloud channel) so its bin/
;; is on the wrapper's PATH too; omit it and the panel just needs "claude"
;; findable some other way (already on the user's profile PATH, etc).
(define %kadr-wrapper-script
  "#!/bin/sh
if [ -z \"$DISPLAY\" ]; then
  DISPLAY=:0
fi
if [ -z \"$XAUTHORITY\" ]; then
  XAUTHORITY=$(ls /run/user/$(id -u)/.mutter-Xwaylandauth.* 2>/dev/null | head -1)
fi
exec env \\
  DISPLAY=\"$DISPLAY\" \\
  XAUTHORITY=\"$XAUTHORITY\" \\
  PATH=\"~a:$PATH\" \\
  ~a --no-sandbox --disable-gpu-sandbox --disable-dev-shm-usage ~a \"$@\"
")

;; Desktop entry as guile-ini data: (("Section" ("Key" . "Value") ...)).
;; scm->ini serialises it to the freedesktop "[Section]\nKey=Value" form.
(define %kadr-desktop-entry
  '(("Desktop Entry"
     ("Type"         . "Application")
     ("Name"         . "Kadr")
     ("GenericName"  . "Video Editor")
     ("Comment"      . "GPU-accelerated multi-track video editor with AI integration")
     ("TryExec"      . "kadr")
     ("Exec"         . "kadr %U")
     ("Terminal"     . "false")
     ("Categories"   . "AudioVideo;Video;"))))

(define* (make-kadr #:key (claude #f))
  (package
    (name "kadr")
    (version %kadr-version)
    (source kadr-source)
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      (with-extensions %kadr-guile-ini-extensions
       #~(modify-phases %standard-phases
          (delete 'bootstrap)
          (delete 'configure)
          (delete 'check)
          (add-after 'unpack 'patch-hardcoded-bin-bash
            (lambda _
              ;; The embedded Claude session pty.spawn's '/bin/bash' --
              ;; Guix System has no /bin/bash (only /bin/sh -> bash), so
              ;; that ENOENTs before it ever gets to resolving "claude"
              ;; on PATH.  Point it at this package's own bash directly.
              (substitute* "electron/claude.ts"
                (("'/bin/bash'")
                 (string-append "'" #$(file-append (specification->package "bash-minimal")
                                                   "/bin/bash")
                                "'")))))
          (add-after 'unpack 'use-vendored-node-modules
            (lambda _
              (copy-recursively #$kadr-npm-vendor "node_modules")
              (for-each make-file-writable (find-files "node_modules"))))
          (replace 'build
            (lambda _
              (setenv "PATH" (string-append #$node "/bin:" (getenv "PATH")))
              (invoke "node_modules/.bin/electron-vite" "build")))
          (replace 'install
            (lambda _
              (use-modules (ini))
              (let* ((share (string-append #$output "/share/kadr"))
                     (app   (string-append share "/app"))
                     (bin   (string-append #$output "/bin"))
                     (apps  (string-append #$output "/share/applications")))
                (mkdir-p app)
                (copy-recursively "out" (string-append app "/out"))
                (copy-recursively "electron" (string-append app "/electron"))
                (copy-recursively "scripts" (string-append app "/scripts"))
                (mkdir-p (string-append app "/node_modules"))
                (copy-recursively "node_modules/node-pty"
                                  (string-append app "/node_modules/node-pty"))
                (copy-file "package.json" (string-append app "/package.json"))

                (mkdir-p bin)
                (call-with-output-file (string-append bin "/kadr")
                  (lambda (port)
                    (format port #$%kadr-wrapper-script
                            (string-append #$(this-package-input "ffmpeg") "/bin"
                                          #$@(if claude
                                                 (list ":" (file-append claude "/bin"))
                                                 '()))
                            #$(file-append electron-42 "/bin/electron")
                            app)))
                (chmod (string-append bin "/kadr") #o755)

                (mkdir-p apps)
                (call-with-output-file (string-append apps "/kadr.desktop")
                  (lambda (port)
                    (scm->ini '#$%kadr-desktop-entry #:port port))))))
          ;; The npm-vendored node_modules/node-pty native addon is already
          ;; linked against this build environment's own glibc/gcc-toolchain
          ;; with correct RPATHs; nothing here needs patchelf.
          (delete 'validate-runpath)))))
    (native-inputs (list node))
    (inputs (append (list electron-42 (specification->package "ffmpeg"))
                    (or (and claude (list claude)) '())))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/HelpFreedom/kadr")
    (synopsis "GPU-accelerated multi-track video editor with AI integration")
    (description
     "Kadr is a multi-track video editor (Electron, React, and TypeScript)
featuring GPU (WebGL2) compositing, keyframes, masks, transitions, and local
speech-to-text.  It embeds an interactive @command{claude} session wired to
the live project over MCP, so an AI agent can edit clips, transcribe audio,
and create @url{https://www.remotion.dev/,Remotion} compositions on the same
timeline the user is watching.  This package builds Kadr from its npm/Vite
source tree; see @code{make-kadr}'s @code{#:claude} argument to make a
@command{claude} package available to the embedded session.")
    (license license:gpl3)))

(define-public kadr (make-kadr))
