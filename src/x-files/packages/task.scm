(define-module (x-files packages task)
  #:use-module (gnu packages gcc)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) #:prefix license:))

(define target->bin-name
  '(("x86_64-linux"  . "task_linux_amd64")
    ("i686-linux"    . "task_linux_386")
    ("armhf-linux"   . "task_linux_arm")
    ("riscv64-linux" . "task_linux_riscv64")
    ("aarch64-linux" . "task_linux_arm64")))

(define targets (map car target->bin-name))

(define target->hash
  '(("x86_64-linux"  . "10pvjz3jka3nxjmh35j1fal089p3kr1x464daw3v9g5w9ahfnrs3")
    ("i686-linux"    . "1axflmc8469m6mfy5kfb725yd62k9iq3yzwhhgh1kbna2a2g9pca")
    ("armhf-linux"   . "1c6sd3hnwf6mf3cwhxn77z9nwhp81nw470zm29ysfkrfnhb1c8jw")
    ("riscv64-linux" . "0riwglp87mwyfny47cm9g31jb689as7fdmr55lncbxw6x8az87sb" )
    ("aarch64-linux" . "006fyj1s4a3jlc6klwv06iwlwam5i97g2h76g4q2iwhsm3bp7mv1")))

(define-public task
  (let* [(target    (or (%current-target-system) (%current-system)))
         (task.bin  (assoc-ref target->bin-name target))
         (hash      (assoc-ref target->hash target))
         (version   "3.45.4")
         (uri       (string-append
                     "https://github.com/go-task/"
                     "task/releases/download/"
                     "v" version
                     "/" task.bin ".tar.gz"))]
    (package
      (name "task")
      (version version)
      (source (origin
                (method url-fetch)
                (uri uri)
                (sha256 (base32 hash))))
      (build-system binary-build-system)
      (arguments
       (list
        #:strip-binaries? #f
        #:validate-runpath? #f
        #:install-plan #~'(("task" "/bin/"))
        #:phases
        #~(modify-phases %standard-phases
            (replace 'unpack
              (lambda* (#:key source #:allow-other-keys)
                (invoke "tar" "-xvf" source)))
            (add-after 'install 'install-completions
              (lambda _
                (let* [(share (string-append #$output "/share"))
                       (bash  (string-append #$output "/etc/bash_completion.d"))
                       (zsh   (string-append share    "/zsh/site-functions"))
                       (fish  (string-append share    "/fish/vendor_completions.d"))]
                  (mkdir-p bash) (mkdir-p fish) (mkdir-p zsh)
                  (copy-file "completion/bash/task.bash"
                             (string-append bash "/task"))
                  (copy-file "completion/zsh/_task"
                             (string-append zsh "/task"))
                  (copy-file "completion/fish/task.fish"
                             (string-append fish "/task"))))))))
      (supported-systems targets)
      (home-page "https://taskfile.dev")
      (synopsis "")
      (description "")
      (license license:expat))))
