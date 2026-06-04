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
  '(("x86_64-linux"  . "1da3na4z6zyz79kniacvn3bhhapxs7cbmv6gwymg47lnzzq94zns")
    ("i686-linux"    . "05bk7iaayrmzc284hbsjpwjg7na442vq2l6msqp4vrjyj2mja12q")
    ("armhf-linux"   . "0ckpn6wv7flzi394p0c0mzhxa7md8p5rj8g491xrkn6v4khyas4x")
    ("riscv64-linux" . "074f6zl9sh4lral3dfplydbrds2kszshbkqcj2qi0p4wjq041p55")
    ("aarch64-linux" . "1y0023pdl9hihgc6kinm0kff09649xlkwfrzanjlj97z1sq8pia9")))

(define-public task
  (let* [(target    (or (%current-target-system) (%current-system)))
         (task.bin  (assoc-ref target->bin-name target))
         (hash      (assoc-ref target->hash target))
         (version   "3.51.1")
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
