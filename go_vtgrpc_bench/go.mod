module example

go 1.22
toolchain go1.24.1

require (
	github.com/planetscale/vtprotobuf v0.6.0
	go.uber.org/automaxprocs v1.5.3
	google.golang.org/grpc v1.63.2
)

require (
	golang.org/x/net v0.38.0 // indirect
	golang.org/x/sys v0.31.0 // indirect
	golang.org/x/text v0.23.0 // indirect
	google.golang.org/genproto/googleapis/rpc v0.0.0-20240415180920-8c6c420018be // indirect
	google.golang.org/protobuf v1.33.0 // indirect
)

replace local => ./
