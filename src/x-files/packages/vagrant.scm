(define-module (x-files packages vagrant)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((gnu packages virtualization) #:prefix upstream:)
  #:use-module ((gnu packages ruby-xyz) #:prefix ruby-xyz:))

;; Guix's `vagrant' and a few of its transitive Ruby dependencies pin exact
;; gem versions in their installed .gemspec files that have since drifted
;; from what the channel actually provides -- so the real `vagrant` binary
;; fails at runtime with Gem::MissingSpecVersionError before it even gets to
;; parse argv. Each fix below relaxes exactly the pin that broke, the same
;; way upstream's own `relax-requirements' phase already does for
;; rgl/vagrant_cloud/rexml.

;; google-protobuf's own installed gemspec has a *runtime* dependency on
;; rake "~> 13.3"; the channel provides rake 13.1.0. Propagated by both
;; googleapis-common-protos-types and ruby-grpc, below.
(define-public ruby-google-protobuf
  (package
    (inherit ruby-xyz:ruby-google-protobuf)
    (arguments
     (substitute-keyword-arguments
         (package-arguments ruby-xyz:ruby-google-protobuf)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'relax-rake-requirement
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (specs (string-append
                               out "/lib/ruby/vendor_ruby/specifications")))
                  (for-each
                   (lambda (f)
                     (substitute* f (("~> 13\\.3") ">= 13.1")))
                   (find-files specs "\\.gemspec$")))))))))))

;; googleapis-common-protos-types pins google-protobuf to "~> 3.18" in its
;; installed gemspec; the channel provides google-protobuf 4.35.1. Propagated
;; by both `vagrant' directly and by `ruby-grpc'.
(define-public ruby-googleapis-common-protos-types
  (package
    (inherit ruby-xyz:ruby-googleapis-common-protos-types)
    (arguments
     (substitute-keyword-arguments
         (package-arguments ruby-xyz:ruby-googleapis-common-protos-types)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'relax-protobuf-requirement
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (specs (string-append
                               out "/lib/ruby/vendor_ruby/specifications")))
                  (for-each
                   (lambda (f)
                     (substitute* f (("~> 3\\.18") ">= 3.18")))
                   (find-files specs "\\.gemspec$")))))))))
    (propagated-inputs
     (modify-inputs
         (package-propagated-inputs ruby-xyz:ruby-googleapis-common-protos-types)
       (replace "ruby-google-protobuf" ruby-google-protobuf)))))

;; ruby-grpc itself is fine (its own protobuf pin is ">= 3.25, < 5.0"), but
;; it propagates the broken google-protobuf/googleapis-common-protos-types
;; above.
(define-public ruby-grpc
  (package
    (inherit ruby-xyz:ruby-grpc)
    (propagated-inputs
     (modify-inputs (package-propagated-inputs ruby-xyz:ruby-grpc)
       (replace "ruby-google-protobuf" ruby-google-protobuf)
       (replace "ruby-googleapis-common-protos-types"
         ruby-googleapis-common-protos-types)))))

;; vagrant pins childprocess to "~> 4.1.0" in vagrant.gemspec (the channel
;; has 5.1.0), and separately propagates the same broken
;; googleapis-common-protos-types/grpc as above.
(define-public vagrant
  (package
    (inherit upstream:vagrant)
    (arguments
     (substitute-keyword-arguments (package-arguments upstream:vagrant)
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'relax-requirements
              (lambda _
                (substitute* "vagrant.gemspec"
                  (("s\\.required_ruby_version ") "# s.required_ruby_version ")
                  (("dependency \"rgl\", \"~> 0.5.10\"")
                   "dependency \"rgl\"")
                  (("dependency \"vagrant_cloud\", \"~> 3.0.5\"")
                   "dependency \"vagrant_cloud\"")
                  (("dependency \"rexml\", .*")
                   "dependency \"rexml\"\n")
                  (("dependency \"childprocess\", \"~> 4.1.0\"")
                   "dependency \"childprocess\"")
                  ((".*dependency \"(wdm|winrm(|-elevated|-fs))\".*") "")
                  ((".*dependency \"rb-kqueue\".*") "")
                  (("^  gitignore_path = " line)
                   (string-append
                    "all_files.reject! { |file| file.match?(\"vagrant-.*\\.gem\") }\n"
                    line)))))))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs upstream:vagrant)
       (replace "ruby-googleapis-common-protos-types"
         ruby-googleapis-common-protos-types)
       (replace "ruby-grpc" ruby-grpc)))))

;; vagrant-libvirt depends on `vagrant' as a plain input; rewire it to the
;; fixed package above instead of upstream's broken one.
(define-public vagrant-libvirt
  (package
    (inherit upstream:vagrant-libvirt)
    (inputs
     (modify-inputs (package-inputs upstream:vagrant-libvirt)
       (replace "vagrant" vagrant)))))
