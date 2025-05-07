module example

go 1.22
toolchain go1.24.1

require (
	connectrpc.com/connect v1.16.1
	go.uber.org/automaxprocs v1.5.3
	golang.org/x/net v0.38.0
)

require (
	golang.org/x/text v0.23.0 // indirect
	google.golang.org/protobuf v1.33.0 // indirect
)

replace local => ./
