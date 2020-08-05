#' Example gRPC service
#'
#' Reads a message with a name and returns a message greeting the name.
#' @references \url{https://github.com/grpc/grpc/tree/master/examples/cpp/helloworld}

library(grpc)

## reading the service definitions
spec <- '/app/proto/helloworld/helloworld.proto'

impl <- read_services(spec)

impl$SayHello$f <- function(request){
  newResponse(message = paste(request$name))
}

## actually running the service handlers
start_server(impl, "0.0.0.0:50051")

