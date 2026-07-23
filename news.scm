(channel-news
 (version 0)
 (entry
  (commit "a1b1140")
  (title (en "Browser history manager + guile-fsnotify"))
  (body (en "Added @code{(x-files features browser-history-manager)}: a Guix
Home feature that periodically (shepherd timer) and in near-real-time (a native
@code{guile-fsnotify} inotify watcher on Fibers) deletes browser history and
other per-domain traces (history, autocomplete, cookies, downloads, plus a
best-effort cache scan) matching declarative patterns, across Firefox- and
Chromium-family profiles.  Deletion uses guile-sqlite3 with @code{PRAGMA
foreign_keys=ON} (so a real Firefox @code{places.sqlite} is left with no
dangling references), keeps bookmarks by default, and skips locked/running DBs;
a marionette system test seeds and asserts against both schemas.  Also added
@code{(x-files packages guile-fsnotify)} (pinned Linux inotify/fanotify FFI
bindings).")))
 (entry
  (commit "08924a2")
  (title (en "yandex-tracker-mcp 0.7.2"))
  (body (en "Packaged @code{yandex-tracker-mcp}, a Model Context Protocol
server exposing Yandex Tracker issues, queues, comments, worklogs and search to
AI assistants over stdio, streamable-http or SSE.  Full functionality — down to
the service-account (IAM-key) auth path — needs a current gRPC: the generated
Yandex stubs use the registered-method API and guard for @code{grpcio>=1.78},
but Guix ships 1.52.  So the change also carries @code{python-grpcio-next}
(1.78, built from the sdist with the vendored C-core at @code{-O1}),
@code{python-yandexcloud} on protobuf@tie{}6, a protobuf-6 rebuild of
@code{googleapis-common-protos} (1.70, with the legacy @file{google/}
@code{pkg_resources} namespace stub restored so @code{google.api} coexists with
@code{google.protobuf}), @code{python-aiocache}, and a completed
@code{python-mcp} dependency closure.")))
 (entry
  (commit "6b1e6dd")
  (title (en "emacs-ansible-vault"))
  (body (en "Packaged @code{emacs-ansible-vault} (ansible-vault-mode 0.6.1), an
Emacs minor mode that transparently decrypts Ansible Vault files on open and
re-encrypts them on save.  Its @code{ansible-vault-command} @code{defcustom} is
patched to the absolute @command{ansible-vault} path from @code{ansible-core}
(also a propagated input), so it works without any extra @env{PATH} setup.")))
 (entry
  (commit "0bed1c2")
  (title (en "slojka 0.2.1"))
  (body (en "Packaged @code{slojka} (Слойка), an Electron-based AI-first raster
graphics editor.  Upstream ships only a prebuilt AppImage, so — like
@code{spotify} — the package unpacks the appended squashfs image (offset
computed from the AppImage's ELF section-header table), patchelfs the Electron
binary and @code{chrome_crashpad_handler}, and wraps @command{slojka} with the
required @code{LD_LIBRARY_PATH} and @code{FONTCONFIG_FILE}.  The single 512x512
icon is rendered down to the full @code{hicolor} ladder with ImageMagick and the
@file{.desktop} entry is generated from guile-ini data.  Only the editor core is
packaged; the optional SAM@tie{}2.1 / polza.ai AI features self-provision into
@file{~/.local/share/slojka/venv} at runtime.")))
 (entry
  (commit "4aba6a4")
  (title (en "Spotify: desktop entry generated with guile-ini"))
  (body (en "The @code{spotify} @file{.desktop} entry is now serialised from
guile-ini data with @code{scm->ini} instead of a hand-written string, matching
the new @code{slojka} package and @code{(x-files services dconf)}.  No
user-visible change.")))
 (entry
  (commit "3fc3452")
  (title (en "Spotify: fixed missing libayatana tray libraries"))
  (body (en "@code{spotify} would not start with @code{libayatana-indicator3.so.7:
cannot open shared object file}.  The binary hard-links three Ayatana tray
libraries (@code{libayatana-ido3-0.4.so.0}, @code{libayatana-indicator3.so.7},
@code{libayatana-appindicator3.so.1}); Guix proper ships none of them and the
old package only faked the appindicator one with a symlink to classic
@code{libappindicator3}, leaving the other two unresolved.  Added the Ayatana
Indicators stack to the channel (@code{ayatana-ido} 0.9.3,
@code{libayatana-indicator} 0.9.5, @code{libayatana-appindicator} 0.6.0) and
wired the real libraries into Spotify's launcher, restoring startup and the
system-tray icon.")))
 (entry
  (commit "bcd7d1c")
  (title (en "Ntfyr: app icon shows up in GNOME"))
  (body (en "Fixed the missing @code{ntfyr} icon on the GNOME desktop.  The
@file{.desktop} file and @code{hicolor} icons installed fine, but meson's
@code{post_install} baked a per-package
@file{share/icons/hicolor/icon-theme.cache}.  With store mtimes reset to 1970,
GTK treated that cache as authoritative and stopped scanning the directory, so
the profile's @code{gtk-icon-themes} hook could not rebuild a merged cache over
it and GNOME never resolved the icon.  The cache is now deleted in a
post-install phase so the profile hook regenerates a correct one.")))
 (entry
  (commit "4e9885a")
  (title (en "kubectl 1.36.2"))
  (body (en "Bumped @code{kubectl} 1.35.4@tie{}->@tie{}1.36.2 (dl.k8s.io release
binary), sha256 recomputed with @command{guix download}.")))
 (entry
  (commit "728a926")
  (title (en "k0s 1.36.2"))
  (body (en "Bumped @code{k0s} 1.35.4@tie{}->@tie{}1.36.2 (@code{+k0s.0}
upstream binary), sha256 recomputed with @command{guix download}.")))
 (entry
  (commit "b2f8cc2")
  (title (en "vault 2.0.3"))
  (body (en "Bumped @code{vault} 2.0.1@tie{}->@tie{}2.0.3 (HashiCorp release
zip, x86_64 and aarch64), sha256 recomputed with @command{guix download}.")))
 (entry
  (commit "5f22d64")
  (title (en "stalwart 0.16.12"))
  (body (en "Bumped @code{stalwart} 0.16.11@tie{}->@tie{}0.16.12 (upstream
release binary, x86_64 and aarch64), sha256 recomputed with
@command{guix download}.")))
 (entry
  (commit "cf5f7a3")
  (title (en "grafana 13.1.0"))
  (body (en "Bumped @code{grafana-bin} 13.0.3@tie{}->@tie{}13.1.0 (dl.grafana.com
release tarball, x86_64 and aarch64), sha256 recomputed with
@command{guix download}.")))
 (entry
  (commit "1fbc40e")
  (title (en "etcd 3.7.0"))
  (body (en "Bumped @code{etcd} 3.6.12@tie{}->@tie{}3.7.0; the public symbol is
now @code{etcd-3.7.0} (with @code{etcd} aliased to it).  sha256 for x86_64 and
aarch64 recomputed with @command{guix download}.")))
 (entry
  (commit "e4f7125")
  (title (en "coder 2.34.5"))
  (body (en "Bumped @code{coder} 2.33.11@tie{}->@tie{}2.34.5 (upstream release
tarball, x86_64/aarch64/armv7), sha256 recomputed with
@command{guix download}.")))
 (entry
  (commit "71ad417")
  (title (en "deno 2.9.2"))
  (body (en "Bumped @code{deno} 2.8.2@tie{}->@tie{}2.9.2 (upstream release zip,
x86_64 and aarch64), sha256 recomputed with @command{guix download}.")))
 (entry
  (commit "ca3081f")
  (title (en "xray-checker: Xray/VLESS proxy quality monitor"))
  (body (en "Added @code{xray-checker} (kutovoys/xray-checker 1.3.1), a Prometheus
exporter that dials through each node in an Xray/VLESS/V2Ray/Sing-box/Clash
subscription and measures latency + connectivity, exposing @code{/metrics} and a
web status page.  Statically-linked linux amd64 Go binary packaged via
@code{nonguix} @code{binary-build-system}.  Intended for benchmarking VPN
providers (feed it multiple @option{--subscription-url}s).")))
 (entry
  (commit "b7d0b5b")
  (title (en "GitLab CLI (glab)"))
  (body (en "Added @code{glab}, the official GitLab command-line client
(gitlab-org/cli 1.107.0), a statically-linked linux amd64 binary packaged via
@code{nonguix} @code{binary-build-system}.  It works with merge requests,
issues, pipelines, releases, and the raw GitLab API against gitlab.com or a
self-hosted instance (selected via @env{GITLAB_HOST}).  Relocated from
@code{(g-files packages glab)} so it lives with the other shared packages.")))
 (entry
  (commit "bec1d12")
  (title (en "OfficeCLI: AI-agent Office suite"))
  (body (en "Added @code{officecli} (iOfficeAI/OfficeCLI 1.0.129), a
self-contained CLI for reading, editing, and automating Word, Excel, and
PowerPoint files.  It is a prebuilt .NET single-file binary: only its
interpreter is patched (rpath/strip would corrupt the appended bundle) and it
runs through a wrapper providing @code{glibc}, @code{gcc:lib}, and
@code{icu4c}.")))
 (entry
  (commit "103fa18")
  (title (en "ntfy clients: Ntfyr desktop app and emacs-ntfy"))
  (body (en "Added two clients for @url{https://ntfy.sh,ntfy} push
notifications.  @code{ntfyr} is a native GTK4/libadwaita desktop application
(tobagin/Ntfyr 0.7.1), built from its meson+cargo sources with the crate
dependencies vendored in a fixed-output derivation.  @code{emacs-ntfy}
(shombando/ntfy) sends notifications from Emacs, with @command{curl} pinned to
the store.")))
 (entry
  (commit "c4c1cdccf82a3870c52bcebca836e7f002132816")
  (title (en "Emacs 1s: feature-emacs-1s loads again"))
  (body (en "Fixed @code{feature-emacs-1s}.  A digit-leading token in the rde
elisp-config name made guix's @code{emacs-build-system} truncate the elpa name
(@code{rde-emacs-1s} was installed as @file{rde-emacs.el}), so the feature-loader's
@code{(require 'rde-emacs-1s)} failed with \"Cannot open load file: rde-emacs-1s\".
The config is now named @code{emacs-bsl} (after the @code{bsl-language-server} it
wraps); @code{feature-emacs-1s} is unchanged.  Also keeps the earlier fix for
@code{1s-mode} symbols being written as @code{#{1s-mode}#}.")))
 (entry
  (commit "23a9d3e")
  (title (en "GPaste 45.6"))
  (body (en "Updated @code{gpaste} from 45.3 to 45.6.")))
 (entry
  (commit "0c1022f")
  (title (en "ClickHouse 26.3.12.3-lts"))
  (body (en "Updated @code{clickhouse-bin} from 25.8.22.28-lts to 26.3.12.3-lts.")))
 (entry
  (commit "e61c6b0")
  (title (en "Package version bumps"))
  (body (en "Updated @code{deno} 2.8.2, @code{geckodriver} 0.37.0 (dropped i686),
@code{task} 3.51.1, @code{xray-core} 26.3.27 (+aarch64),
@code{coder} 2.33.6, @code{clojure-tools} 1.12.5.1654, @code{etcd} 3.6.12,
@code{zellij} 0.44.3, @code{remark42} 1.16.1 (+aarch64), @code{amneziawg-go} 0.2.18,
@code{wireproxy-awg} 1.0.15, @code{k0s} 1.35.4, @code{kubectl} 1.35.4,
@code{helm} 4.2.0, @code{vault} 2.0.1, @code{zapret2} 0.9.5.2,
@code{grafana} 13.0.2.")))
 (entry
  (commit "aaf3072")
  (title (en "Deno 2.8.0"))
  (body (en "Updated @code{deno} from 2.7.11 to 2.8.0.  Also fixed aarch64 binary URL typo.")))
 (entry
  (commit "dadd21d")
  (title (en "Emacs: prisma-ts-mode + tree-sitter-prisma"))
  (body (en "Added @code{tree-sitter-prisma} as a propagated input to @code{emacs-prisma-ts-mode}.")))
 (entry
  (commit "a9168e5")
  (title (en "Valentina: desktop integration"))
  (body (en "Fixed @code{valentina} install prefix, added desktop files and icons,
fixed RUNPATH for puzzle and tape binaries.")))
 (entry
  (commit "6fefafacb5825f4879a7d7ccedc17dc84be4f5a2")
  (title (en "Phosh: phone shell and compositor"))
  (body (en "Added @code{phosh} 0.55.0 and @code{phoc} 0.55.0 (Wayland compositor)
for mobile devices.  Includes bumped dependencies: @code{polkit-next} 124,
@code{gmobile-next} 0.7.1, @code{feedbackd-next} 0.8.9, @code{libmbim-next} 1.32,
@code{libqmi-next} 1.36, and @code{modem-manager-next} 1.24.2.
Based on the old Guix issue #44400, updated from 0.14 to current.")))
 (entry
  (commit "3b358d3a4cd87970f074bc4a2b35d2cc93ef2cdc")
  (title (en "Valentina: pattern-making software"))
  (body (en "Added @code{valentina} package for open-source pattern-making
for sewing.")))
 (entry
  (commit "40f882e1006b90949cd78a95558e2df3cec9bc54")
  (title (en "Qbs: Qt Build Suite"))
  (body (en "Added @code{qbs} package.")))
 (entry
  (commit "e2f7c7648bd054dc9553bd64cecf117d53a9d1c0")
  (title (en "ClickHouse: fix data directory permissions"))
  (body (en "The ClickHouse service now chowns all data-dir subdirectories.")))
 (entry
  (commit "52cecdfec45bece4528ff61b61d28cacb3e87afe")
  (title (en "ClickHouse: package, service, system test"))
  (body (en "Added @code{clickhouse-bin} package (version 25.8.22.28 LTS, x86_64 and aarch64)
using @code{binary-build-system} from nonguix.  Added @code{clickhouse-service-type}
with configurable ports, listen address, data/log directories, and log rotation.
Config is generated via Guile's @code{(sxml simple)}.  Includes a passing system test.")))
 (entry
  (commit "87077ee3a13656289caa66d172a3b63153ac4f4d")
  (title (en "Remark42: package, service, system test"))
  (body (en "Added @code{remark42} package and @code{remark42-service-type} for the
self-hosted comment engine.  Includes a passing system test.")))
 (entry
  (commit "aebbe148d6b32a374787bc41fccb84ff1fc2cfe1")
  (title (en "Grafana: package, service, feature"))
  (body (en "Added @code{grafana} package, @code{grafana-service-type}, and
@code{feature-grafana} for the observability dashboard.")))
 (entry
  (commit "e7c0e42a0e77086af6dd5dc75119de1b522e015a")
  (title (en "Spotify: package and service"))
  (body (en "Added @code{spotify} binary package and @code{spotify-service-type}.")))
 (entry
  (commit "76bccadc3c4ec1c362160ba1c3a120833b52005c")
  (title (en "Kubernetes via k0s: packages, service, feature"))
  (body (en "Added @code{k0s} package and @code{kuber-service-type} /
@code{feature-kuber} for lightweight Kubernetes cluster management.")))
 (entry
  (commit "9592864")
  (title (en "llama-cpp service"))
  (body (en "Added @code{llama-cpp-service-type} for running local LLM inference
via llama.cpp as a Shepherd service.")))
 (entry
  (commit "4d578cfddd66070e8c243077dd3bf4a82d8faa89")
  (title (en "Datomic: Postgres transactor + tests"))
  (body (en "Added @code{datomic-postgres-transactor-service-type} for running
Datomic against a PostgreSQL storage backend.  Both dev and postgres transactor
variants now have passing system tests.")))
 (entry
  (commit "ba4fe3072689b55a2de679ae75d50e7d03ff5e50")
  (title (en "dconf service: compose and merging"))
  (body (en "@code{dconf-service-type} now supports @code{compose} and config
merging, making it easier to extend dconf settings across multiple service
consumers.")))
 (entry
  (commit "c79e074ca8a07c8a18f76ed443809ebc24b23bdb")
  (title (en "MCP packages: filesystem and browser"))
  (body (en "Added @code{rust-mcp-filesystem} and @code{rust-mcp-browser}
packages for Model Context Protocol server tools.")))
 (entry
  (commit "da6ac8a945556f6b6210273fb7a173d5d5869144")
  (title (en "Docker: re-implementation + fresh packages"))
  (body (en "Tweaked the original @code{(@@ (gnu services docker) docker-service-type)} to support @code{/etc/docker/daemon.json} settings file")))
 (entry
  (commit "ea6f74f6e287774d0723748ab8a610ee4fe7a2c7")
  (title (en "Storage: first implementation"))
  (body (en "First attempt at unifying URI RFC 3986-based resource access")))
 (entry
  (commit "cab1244d30b8ec268e3bef6ef8d4d3a689b20318")
  (title (en "Breaking @code{news}!"))
  (body (en "Add news to this channel. TODO: automatize the management"))))
