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
  '(("x86_64-linux"  . "1yqn8h902b1hsyndwrw5mx4hhi95118qz6jd5p3sk0fqnkdmj238")
    ("i686-linux"    . "07bdv3f2bg32i01q20dpgn49bbb3qb9iipg6zk8f641p9fa0hy76")
    ("armhf-linux"   . "026jpipk3xd9akc5fa8jcr70bdiagk9h9x41p5sd9qwcygv6qxpr")
    ("riscv64-linux" . "0vnwg22hqqshh0y05jjd4m01bymac9862ccxgl6k1lz1g8h47cxy")
    ("aarch64-linux" . "0nyzw6a44bi47gr0id4ics2x9b17idycsahvkvimhq2akm1rs66p")))

(define-public task
  (let* [(target    (or (%current-target-system) (%current-system)))
         (task.bin  (assoc-ref target->bin-name target))
         (hash      (assoc-ref target->hash target))
         (version   "3.54.0")
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
      (synopsis "Task is a task runner / build tool that aims to be simpler and easier to use than, for example, GNU Make")
      ;#"I've only has to pack it because it's used at work, so I could experiment with it a little. This software is just a piece of shit"
      (description "A fast, cross-platform build tool inspired by Make, designed for modern workflows")
      (license license:expat))))
