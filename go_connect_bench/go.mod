module example

go 1.19

require (
	github.com/bufbuild/connect-go v1.0.0
	go.uber.org/automaxprocs v1.5.1
	golang.org/x/net v0.0.0-20220624214902-1bab6f366d9e
)

require golang.org/x/text v0.3.7 // indirect

replace local => ./
