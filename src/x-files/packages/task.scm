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
  '(("x86_64-linux"  . "1gcmksn8f74pghwlha999s4ia5kk665xfis8h28sgp2kmvzpkih2")
    ("i686-linux"    . "16zccy5acgydlqw51683cv9f41g4by9l1xzbs9670h90wnxh71ag")
    ("armhf-linux"   . "077fwr1hphk7yd5jy9zd81p1llnw9mnd6qi50rljliwwbfj5j0bs")
    ("riscv64-linux" . "0ni78kcdc85h5ll49105dwdhslfh43c9fkclcmj1zldbay4xba6v")
    ("aarch64-linux" . "1gjf9v867p0y39iynvr7vzk86z1v9rb8kckp8m9w1kihi084803y")))

(define-public task
  (let* [(target    (or (%current-target-system) (%current-system)))
         (task.bin  (assoc-ref target->bin-name target))
         (hash      (assoc-ref target->hash target))
         (version   "3.52.0")
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
