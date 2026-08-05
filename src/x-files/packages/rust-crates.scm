(define-module (x-files packages rust-crates)
  #:use-module (guix build-system cargo)
  #:export (lookup-cargo-inputs))

;;;
;;; Flat Cargo input table for tramp-rpc-server, vendored from upstream Guix
;;; (branch packages/emacs-tramp-rpc, commit 2d15c5a33c3).  The individual
;;; rust-*-* crate-source bindings are module-private in
;;; (gnu packages rust-crates) and thus cannot be imported, so they are
;;; re-declared here (self-contained origins keyed by their own hashes).
;;;
;;; Consumed via (cargo-inputs 'tramp-rpc-server
;;;                            #:module '(x-files packages rust-crates)).
;;;

(define rust-base64-0.22.1
  (crate-source "base64" "0.22.1"
                "1imqzgh7bxcikp5vx3shqvw9j09g9ly0xr0jma0q66i52r7jbcvj"))
(define rust-bitflags-2.13.1
  (crate-source "bitflags" "2.13.1"
                "1nl76mpykmwmb8rq1l5vw1azdh1wvxdrnsk4sy3rdrzx01nvg25m"))
(define rust-bytes-1.12.1
  (crate-source "bytes" "1.12.1"
                "017z19dpg4f942h051m7bpnzcgng042hhcpd7bmg7bjjqd42lrgw"))
(define rust-cfg-aliases-0.2.2
  (crate-source "cfg_aliases" "0.2.2"
                "09rm3dv28gbsal7w6q76lg2nfyn8wp789ska9b8vr1w750xfhygh"))
(define rust-cfg-if-1.0.4
  (crate-source "cfg-if" "1.0.4"
                "008q28ajc546z5p2hcwdnckmg0hia7rnx52fni04bwqkzyrghc4k"))
(define rust-errno-0.3.14
  (crate-source "errno" "0.3.14"
                "1szgccmh8vgryqyadg8xd58mnwwicf39zmin3bsn63df2wbbgjir"))
(define rust-futures-0.3.33
  (crate-source "futures" "0.3.33"
                "066j5aqz8an05xh4hn5ljdnjn80z3g335v4grx4gaifr57wg3358"))
(define rust-futures-channel-0.3.33
  (crate-source "futures-channel" "0.3.33"
                "1bn5hlhfkl1sgypmiachaqcgwmr6wmjal7dyhfyb1zkazvs90996"))
(define rust-futures-core-0.3.33
  (crate-source "futures-core" "0.3.33"
                "1iqdbvcdlplfr2g43h7xrfkv2sg5p1a26x8acz1xgxl07i3hrm9c"))
(define rust-futures-executor-0.3.33
  (crate-source "futures-executor" "0.3.33"
                "0n3lpkmcfrsnh40i4armn040gnqbpd257hz5qs46zipjr6f8fm37"))
(define rust-futures-io-0.3.33
  (crate-source "futures-io" "0.3.33"
                "0yjx13qdm9b2p4w00ddw85k6yccnnmqrlrrz8yfmi5jg7jmfqxs5"))
(define rust-futures-macro-0.3.33
  (crate-source "futures-macro" "0.3.33"
                "02xiyd5y1nk9b805aympj4wq2czgvxnhcml9w9xkc665d3g3qv9d"))
(define rust-futures-sink-0.3.33
  (crate-source "futures-sink" "0.3.33"
                "01z38z344hpryw84b6r0rbwcb669d8pyvl2szg10aqwx96n1hi73"))
(define rust-futures-task-0.3.33
  (crate-source "futures-task" "0.3.33"
                "02f1y1yvjg1cv998zkgl1706pi9y4fyc9045l1hlmyqyhclfscdj"))
(define rust-futures-util-0.3.33
  (crate-source "futures-util" "0.3.33"
                "1anyg40j5www5l22r2jbn1birsafz4q1w9qmcjk4vqzwasi90ym7"))
(define rust-itoa-1.0.18
  (crate-source "itoa" "1.0.18"
                "10jnd1vpfkb8kj38rlkn2a6k02afvj3qmw054dfpzagrpl6achlg"))
(define rust-libc-0.2.189
  (crate-source "libc" "0.2.189"
                "1whjfs375vlng2q6yrbzs73cvp5lm3w1n2gfqajb2vgf7zg3xbry"))
(define rust-memchr-2.8.3
  (crate-source "memchr" "2.8.3"
                "161xa63ipfanf8v3nb82xd5hqgydv55nzw59wyngqbz6alfaz2yg"))
(define rust-mio-1.2.2
  (crate-source "mio" "1.2.2"
                "09y4b7gc42ymgssshh8sz6gs3y5r8bbigqaw2c4snh6fy5qmrmih"))
(define rust-nix-0.31.3
  (crate-source "nix" "0.31.3"
                "0gbwnjfny9rq9hl5bz4ry520n9rnfknna4bg88n66f7zx3yx486g"
                #:snippet '(delete-file-recursively "test")))
(define rust-pin-project-lite-0.2.17
  (crate-source "pin-project-lite" "0.2.17"
                "1kfmwvs271si96zay4mm8887v5khw0c27jc9srw1a75ykvgj54x8"))
(define rust-pin-utils-0.1.0
  (crate-source "pin-utils" "0.1.0"
                "117ir7vslsl2z1a7qzhws4pd01cg2d3338c47swjyvqv2n60v1wb"))
(define rust-proc-macro2-1.0.107
  (crate-source "proc-macro2" "1.0.107"
                "1nb6ly8kp65f724kj73ippc7lvydss24sm2vagk6qpklpg4pwplq"))
(define rust-quote-1.0.47
  (crate-source "quote" "1.0.47"
                "00ch0yyzvv6s671ik0kcsbw8nigdaj2g3fr61kcahwx48aqlvgqz"))
(define rust-serde-1.0.229
  (crate-source "serde" "1.0.229"
                "1fp04fq4a79bpm61xz1zy0pbz4kpc7d771zii1k3inmszq55jj21"))
(define rust-serde-core-1.0.229
  (crate-source "serde_core" "1.0.229"
                "0j1ajiha76h3nmd976il9li6975k121xa7jb39ws8n0yqp4s5p37"))
(define rust-serde-derive-1.0.229
  (crate-source "serde_derive" "1.0.229"
                "0j4k63i7h1bikxwz2c89ig0hrwbnl9mz1czn85xx99x5cc9dg9g7"))
(define rust-serde-json-1.0.151
  (crate-source "serde_json" "1.0.151"
                "051zww7lvpw147vvwss1ng6w587qyrkzg75fvj08q2dfrmgbahf8"))
(define rust-signal-hook-registry-1.4.8
  (crate-source "signal-hook-registry" "1.4.8"
                "06vc7pmnki6lmxar3z31gkyg9cw7py5x9g7px70gy2hil75nkny4"))
(define rust-slab-0.4.12
  (crate-source "slab" "0.4.12"
                "1xcwik6s6zbd3lf51kkrcicdq2j4c1fw0yjdai2apy9467i0sy8c"))
(define rust-socket2-0.6.5
  (crate-source "socket2" "0.6.5"
                "1m7diygswpvlpvrxd6ap169nxgax014jr8220nqlr3bzyb3y5lf3"))
(define rust-syn-2.0.114
  (crate-source "syn" "2.0.114"
                "0akw62dizhyrkf3ym1jsys0gy1nphzgv0y8qkgpi6c1s4vghglfl"))
(define rust-thiserror-1.0.69
  (crate-source "thiserror" "1.0.69"
                "0lizjay08agcr5hs9yfzzj6axs53a2rgx070a1dsi3jpkcrzbamn"))
(define rust-thiserror-impl-1.0.69
  (crate-source "thiserror-impl" "1.0.69"
                "1h84fmn2nai41cxbhk6pqf46bxqq1b344v8yz089w1chzi76rvjg"))
(define rust-tokio-1.53.1
  (crate-source "tokio" "1.53.1"
                "1v8b3b45pkpbibls75yniqbvx5dlks2708141ljni5mnf6lawb10"))
(define rust-tokio-macros-2.7.2
  (crate-source "tokio-macros" "2.7.2"
                "03kvy2r5gr4zccm4vdx8vvv3q69kbjc1b006rs11aibz74m3lxvq"))
(define rust-unicode-ident-1.0.24
  (crate-source "unicode-ident" "1.0.24"
                "0xfs8y1g7syl2iykji8zk5hgfi5jw819f5zsrbaxmlzwsly33r76"
                #:snippet '(delete-file-recursively "tests")))
(define rust-wasi-0.14.7+wasi-0.2.4
  (crate-source "wasi" "0.14.7+wasi-0.2.4"
                "133fq3mq7h65mzrsphcm7bbbx1gsz7srrbwh01624zin43g7hd48"))
(define rust-windows-aarch64-gnullvm-0.53.1
  (crate-source "windows_aarch64_gnullvm" "0.53.1"
                "0lqvdm510mka9w26vmga7hbkmrw9glzc90l4gya5qbxlm1pl3n59"
                #:snippet '(delete-file-recursively "lib")))
(define rust-windows-aarch64-msvc-0.53.1
  (crate-source "windows_aarch64_msvc" "0.53.1"
                "01jh2adlwx043rji888b22whx4bm8alrk3khjpik5xn20kl85mxr"
                #:snippet '(delete-file-recursively "lib")))
(define rust-windows-i686-gnu-0.53.1
  (crate-source "windows_i686_gnu" "0.53.1"
                "18wkcm82ldyg4figcsidzwbg1pqd49jpm98crfz0j7nqd6h6s3ln"
                #:snippet '(delete-file-recursively "lib")))
(define rust-windows-i686-gnullvm-0.53.1
  (crate-source "windows_i686_gnullvm" "0.53.1"
                "030qaxqc4salz6l4immfb6sykc6gmhyir9wzn2w8mxj8038mjwzs"
                #:snippet '(delete-file-recursively "lib")))
(define rust-windows-i686-msvc-0.53.1
  (crate-source "windows_i686_msvc" "0.53.1"
                "1hi6scw3mn2pbdl30ji5i4y8vvspb9b66l98kkz350pig58wfyhy"
                #:snippet '(delete-file-recursively "lib")))
(define rust-windows-link-0.2.1
  (crate-source "windows-link" "0.2.1"
                "1rag186yfr3xx7piv5rg8b6im2dwcf8zldiflvb22xbzwli5507h"))
(define rust-windows-sys-0.60.2
  (crate-source "windows-sys" "0.60.2"
                "1jrbc615ihqnhjhxplr2kw7rasrskv9wj3lr80hgfd42sbj01xgj"))
(define rust-windows-sys-0.61.2
  (crate-source "windows-sys" "0.61.2"
                "1z7k3y9b6b5h52kid57lvmvm05362zv1v8w0gc7xyv5xphlp44xf"))
(define rust-windows-targets-0.53.5
  (crate-source "windows-targets" "0.53.5"
                "1wv9j2gv3l6wj3gkw5j1kr6ymb5q6dfc42yvydjhv3mqa7szjia9"))
(define rust-windows-x86-64-gnu-0.53.1
  (crate-source "windows_x86_64_gnu" "0.53.1"
                "16d4yiysmfdlsrghndr97y57gh3kljkwhfdbcs05m1jasz6l4f4w"
                #:snippet '(delete-file-recursively "lib")))
(define rust-windows-x86-64-gnullvm-0.53.1
  (crate-source "windows_x86_64_gnullvm" "0.53.1"
                "1qbspgv4g3q0vygkg8rnql5c6z3caqv38japiynyivh75ng1gyhg"
                #:snippet '(delete-file-recursively "lib")))
(define rust-windows-x86-64-msvc-0.53.1
  (crate-source "windows_x86_64_msvc" "0.53.1"
                "0l6npq76vlq4ksn4bwsncpr8508mk0gmznm6wnhjg95d19gzzfyn"
                #:snippet '(delete-file-recursively "lib")))
(define rust-zmij-1.0.23
  (crate-source "zmij" "1.0.23"
                "06zwri21nnrl34rwinmvbciap8yk1mrl8qfg9pff7lgspc56sri9"))

(define-cargo-inputs lookup-cargo-inputs
                     (tramp-rpc-server =>
                                       (list rust-base64-0.22.1
                                             rust-bitflags-2.13.1
                                             rust-bytes-1.12.1
                                             rust-cfg-if-1.0.4
                                             rust-cfg-aliases-0.2.2
                                             rust-errno-0.3.14
                                             rust-futures-0.3.33
                                             rust-futures-channel-0.3.33
                                             rust-futures-core-0.3.33
                                             rust-futures-executor-0.3.33
                                             rust-futures-io-0.3.33
                                             rust-futures-macro-0.3.33
                                             rust-futures-sink-0.3.33
                                             rust-futures-task-0.3.33
                                             rust-futures-util-0.3.33
                                             rust-itoa-1.0.18
                                             rust-libc-0.2.189
                                             rust-memchr-2.8.3
                                             rust-mio-1.2.2
                                             rust-nix-0.31.3
                                             rust-pin-project-lite-0.2.17
                                             rust-pin-utils-0.1.0
                                             rust-proc-macro2-1.0.107
                                             rust-quote-1.0.47
                                             rust-serde-1.0.229
                                             rust-serde-core-1.0.229
                                             rust-serde-derive-1.0.229
                                             rust-serde-json-1.0.151
                                             rust-signal-hook-registry-1.4.8
                                             rust-slab-0.4.12
                                             rust-socket2-0.6.5
                                             rust-syn-2.0.114
                                             rust-thiserror-1.0.69
                                             rust-thiserror-impl-1.0.69
                                             rust-tokio-1.53.1
                                             rust-tokio-macros-2.7.2
                                             rust-unicode-ident-1.0.24
                                             rust-wasi-0.14.7+wasi-0.2.4
                                             rust-windows-link-0.2.1
                                             rust-windows-sys-0.60.2
                                             rust-windows-sys-0.61.2
                                             rust-windows-targets-0.53.5
                                             rust-windows-aarch64-gnullvm-0.53.1
                                             rust-windows-aarch64-msvc-0.53.1
                                             rust-windows-i686-gnu-0.53.1
                                             rust-windows-i686-gnullvm-0.53.1
                                             rust-windows-i686-msvc-0.53.1
                                             rust-windows-x86-64-gnu-0.53.1
                                             rust-windows-x86-64-gnullvm-0.53.1
                                             rust-windows-x86-64-msvc-0.53.1
                                             rust-zmij-1.0.23)))
