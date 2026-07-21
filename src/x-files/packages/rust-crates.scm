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
(define rust-bitflags-2.10.0
  (crate-source "bitflags" "2.10.0"
                "1lqxwc3625lcjrjm5vygban9v8a6dlxisp1aqylibiaw52si4bl1"))
(define rust-bytes-1.11.0
  (crate-source "bytes" "1.11.0"
                "1cww1ybcvisyj8pbzl4m36bni2jaz0narhczp1348gqbvkxh8lmk"))
(define rust-cfg-aliases-0.2.1
  (crate-source "cfg_aliases" "0.2.1"
                "092pxdc1dbgjb6qvh83gk56rkic2n2ybm4yvy76cgynmzi3zwfk1"))
(define rust-cfg-if-1.0.4
  (crate-source "cfg-if" "1.0.4"
                "008q28ajc546z5p2hcwdnckmg0hia7rnx52fni04bwqkzyrghc4k"))
(define rust-errno-0.3.14
  (crate-source "errno" "0.3.14"
                "1szgccmh8vgryqyadg8xd58mnwwicf39zmin3bsn63df2wbbgjir"))
(define rust-futures-0.3.31
  (crate-source "futures" "0.3.31"
                "0xh8ddbkm9jy8kc5gbvjp9a4b6rqqxvc8471yb2qaz5wm2qhgg35"))
(define rust-futures-channel-0.3.31
  (crate-source "futures-channel" "0.3.31"
                "040vpqpqlbk099razq8lyn74m0f161zd0rp36hciqrwcg2zibzrd"))
(define rust-futures-core-0.3.31
  (crate-source "futures-core" "0.3.31"
                "0gk6yrxgi5ihfanm2y431jadrll00n5ifhnpx090c2f2q1cr1wh5"))
(define rust-futures-executor-0.3.31
  (crate-source "futures-executor" "0.3.31"
                "17vcci6mdfzx4gbk0wx64chr2f13wwwpvyf3xd5fb1gmjzcx2a0y"))
(define rust-futures-io-0.3.31
  (crate-source "futures-io" "0.3.31"
                "1ikmw1yfbgvsychmsihdkwa8a1knank2d9a8dk01mbjar9w1np4y"))
(define rust-futures-macro-0.3.31
  (crate-source "futures-macro" "0.3.31"
                "0l1n7kqzwwmgiznn0ywdc5i24z72zvh9q1dwps54mimppi7f6bhn"))
(define rust-futures-sink-0.3.31
  (crate-source "futures-sink" "0.3.31"
                "1xyly6naq6aqm52d5rh236snm08kw8zadydwqz8bip70s6vzlxg5"))
(define rust-futures-task-0.3.31
  (crate-source "futures-task" "0.3.31"
                "124rv4n90f5xwfsm9qw6y99755y021cmi5dhzh253s920z77s3zr"))
(define rust-futures-util-0.3.31
  (crate-source "futures-util" "0.3.31"
                "10aa1ar8bgkgbr4wzxlidkqkcxf77gffyj8j7768h831pcaq784z"))
(define rust-itoa-1.0.17
  (crate-source "itoa" "1.0.17"
                "1lh93xydrdn1g9x547bd05g0d3hra7pd1k4jfd2z1pl1h5hwdv4j"))
(define rust-libc-0.2.180
  (crate-source "libc" "0.2.180"
                "1z2n7hl10fnk1xnv19ahhqxwnb4qi9aclnl6gigim2aaahw5mhxw"))
(define rust-memchr-2.7.6
  (crate-source "memchr" "2.7.6"
                "0wy29kf6pb4fbhfksjbs05jy2f32r2f3r1ga6qkmpz31k79h0azm"))
(define rust-mio-1.1.1
  (crate-source "mio" "1.1.1"
                "1z2phpalqbdgihrcjp8y09l3kgq6309jnhnr6h11l9s7mnqcm6x6"))
(define rust-nix-0.29.0
  (crate-source "nix" "0.29.0"
                "0ikvn7s9r2lrfdm3mx1h7nbfjvcc6s9vxdzw7j5xfkd2qdnp9qki"
                #:snippet '(delete-file-recursively "test")))
(define rust-pin-project-lite-0.2.16
  (crate-source "pin-project-lite" "0.2.16"
                "16wzc7z7dfkf9bmjin22f5282783f6mdksnr0nv0j5ym5f9gyg1v"))
(define rust-pin-utils-0.1.0
  (crate-source "pin-utils" "0.1.0"
                "117ir7vslsl2z1a7qzhws4pd01cg2d3338c47swjyvqv2n60v1wb"))
(define rust-proc-macro2-1.0.105
  (crate-source "proc-macro2" "1.0.105"
                "1rvgs5qdznlrqrgicmv24nybnrnv8kyvk2vi7s52ddna1q71hpak"))
(define rust-quote-1.0.43
  (crate-source "quote" "1.0.43"
                "02n41mlr81qmczac7m5kjy51y8b7yrb8ym4ncmjycampjjjxjx6w"))
(define rust-serde-1.0.228
  (crate-source "serde" "1.0.228"
                "17mf4hhjxv5m90g42wmlbc61hdhlm6j9hwfkpcnd72rpgzm993ls"))
(define rust-serde-core-1.0.228
  (crate-source "serde_core" "1.0.228"
                "1bb7id2xwx8izq50098s5j2sqrrvk31jbbrjqygyan6ask3qbls1"))
(define rust-serde-derive-1.0.228
  (crate-source "serde_derive" "1.0.228"
                "0y8xm7fvmr2kjcd029g9fijpndh8csv5m20g4bd76w8qschg4h6m"))
(define rust-serde-json-1.0.149
  (crate-source "serde_json" "1.0.149"
                "11jdx4vilzrjjd1dpgy67x5lgzr0laplz30dhv75lnf5ffa07z43"))
(define rust-signal-hook-registry-1.4.8
  (crate-source "signal-hook-registry" "1.4.8"
                "06vc7pmnki6lmxar3z31gkyg9cw7py5x9g7px70gy2hil75nkny4"))
(define rust-slab-0.4.11
  (crate-source "slab" "0.4.11"
                "12bm4s88rblq02jjbi1dw31984w61y2ldn13ifk5gsqgy97f8aks"))
(define rust-socket2-0.6.1
  (crate-source "socket2" "0.6.1"
                "109qn0kjhqi5zds84qyqi5wn72g8azjhmf4b04fkgkrkd48rw4hp"))
(define rust-syn-2.0.114
  (crate-source "syn" "2.0.114"
                "0akw62dizhyrkf3ym1jsys0gy1nphzgv0y8qkgpi6c1s4vghglfl"))
(define rust-thiserror-1.0.69
  (crate-source "thiserror" "1.0.69"
                "0lizjay08agcr5hs9yfzzj6axs53a2rgx070a1dsi3jpkcrzbamn"))
(define rust-thiserror-impl-1.0.69
  (crate-source "thiserror-impl" "1.0.69"
                "1h84fmn2nai41cxbhk6pqf46bxqq1b344v8yz089w1chzi76rvjg"))
(define rust-tokio-1.49.0
  (crate-source "tokio" "1.49.0"
                "11ix3pl03s0bp71q3wddrbf8xr0cpn47d7fzr6m42r3kswy918kj"))
(define rust-tokio-macros-2.6.0
  (crate-source "tokio-macros" "2.6.0"
                "19czvgliginbzyhhfbmj77wazqn2y8g27y2nirfajdlm41bphh5g"))
(define rust-unicode-ident-1.0.22
  (crate-source "unicode-ident" "1.0.22"
                "1x8xrz17vqi6qmkkcqr8cyf0an76ig7390j9cnqnk47zyv2gf4lk"
                #:snippet '(delete-file-recursively "tests")))
(define rust-wasi-0.11.1+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.1+wasi-snapshot-preview1"
                "0jx49r7nbkbhyfrfyhz0bm4817yrnxgd3jiwwwfv0zl439jyrwyc"))
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
(define rust-zmij-1.0.14
  (crate-source "zmij" "1.0.14"
                "1siglzwmyqs0zc0crnm8r1mylnhv410qxdmchzw2ips8p183z3xx"))

(define-cargo-inputs lookup-cargo-inputs
                     (tramp-rpc-server =>
                                       (list rust-base64-0.22.1
                                             rust-bitflags-2.10.0
                                             rust-bytes-1.11.0
                                             rust-cfg-if-1.0.4
                                             rust-cfg-aliases-0.2.1
                                             rust-errno-0.3.14
                                             rust-futures-0.3.31
                                             rust-futures-channel-0.3.31
                                             rust-futures-core-0.3.31
                                             rust-futures-executor-0.3.31
                                             rust-futures-io-0.3.31
                                             rust-futures-macro-0.3.31
                                             rust-futures-sink-0.3.31
                                             rust-futures-task-0.3.31
                                             rust-futures-util-0.3.31
                                             rust-itoa-1.0.17
                                             rust-libc-0.2.180
                                             rust-memchr-2.7.6
                                             rust-mio-1.1.1
                                             rust-nix-0.29.0
                                             rust-pin-project-lite-0.2.16
                                             rust-pin-utils-0.1.0
                                             rust-proc-macro2-1.0.105
                                             rust-quote-1.0.43
                                             rust-serde-1.0.228
                                             rust-serde-core-1.0.228
                                             rust-serde-derive-1.0.228
                                             rust-serde-json-1.0.149
                                             rust-signal-hook-registry-1.4.8
                                             rust-slab-0.4.11
                                             rust-socket2-0.6.1
                                             rust-syn-2.0.114
                                             rust-thiserror-1.0.69
                                             rust-thiserror-impl-1.0.69
                                             rust-tokio-1.49.0
                                             rust-tokio-macros-2.6.0
                                             rust-unicode-ident-1.0.22
                                             rust-wasi-0.11.1+wasi-snapshot-preview1
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
                                             rust-zmij-1.0.14)))
