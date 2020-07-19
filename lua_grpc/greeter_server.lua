--- Hello world greeter example server.
-- greeter_server.lua

-- Current work dir: grpc-lua/examples/helloworld
package.path = "grpc-lua/src/lua/?.lua;" .. package.path

local grpc = require("grpc_lua.grpc_lua")

local function main()
    local svr = grpc.server()
    local server_address = "0.0.0.0:50051"
    svr:add_listening_port(server_address)
    -- Service implementation is a table.
    local service = require("greeter_service")
    svr:register_service("helloworld.Greeter", service)
    print("Server listening on " .. server_address)
    svr:run()
end  -- main()

main()
