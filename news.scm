(channel-news
 (version 0)
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
