import Config

config :falco, start_server: true

config :logger,
  level: :error,
  compile_time_purge_level: :error,
  sync_threshold: 10_000,
  truncate: 4096
