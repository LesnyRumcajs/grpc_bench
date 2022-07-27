import Config

config :grpc, start_server: true

config :logger,
  level: :debug,
  compile_time_purge_level: :debug,
  sync_threshold: 10_000,
  truncate: 4096
