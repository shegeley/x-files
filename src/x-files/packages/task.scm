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
  '(("x86_64-linux"  . "1irgm14ym7yvjsk4r53w3s6cvfiivf042xw7dqgr5zv1d27l0jm5")
    ("i686-linux"    . "1sq1isy76mqqkficn44d1jjqigqkp9q1mbd6j8nbmdf738k694j9")
    ("armhf-linux"   . "0cgjbly7z4wz9l403xpkhdfcjbnm3vdffilrk8gjz6fr4mq1lla9")
    ("riscv64-linux" . "1zlr18719kd1iqc311rz5hpyrn0vbpdw9c26scgwwh79s7ql0fbl")
    ("aarch64-linux" . "1275sada10n7wshhqsh7n4rya0xzak68rbi23wp1384k2h81kbg3")))

(define-public task
  (let* [(target    (or (%current-target-system) (%current-system)))
         (task.bin  (assoc-ref target->bin-name target))
         (hash      (assoc-ref target->hash target))
         (version   "3.53.1")
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
