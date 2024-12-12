(define-module (x-files kernels sm7150)
  #:use-module (nongnu packages linux)
  #:use-module (guix download)

  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages linux)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system trivial)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (nonguix licenses)
  #:use-module (gnu packages linux))

(define sm7150-mainline
 (let [(url    "https://github.com/sm7150-mainline/linux")
       (commit "9dddfbbbb0cbe49944d87722f3c9403947602d05")
       (hash   "009qlljjwdkhzh1g71jpy7nbdrrz86ar54g7x1r9liy136a2z6h6")]
  (customize-linux
   #:name "sm7150-mainline"
   #:linux linux-arm64-generic
   #:source (origin
             (method git-fetch)
             (uri (git-reference (url url) (commit commit) (recursive? #t)))
             (sha256 (base32 hash))))))
