--- Hello world greeter example server side service.
-- greeter_service.lua

local M = {}

local grpc = require("grpc_lua.grpc_lua")
grpc.import_proto_file("helloworld.proto")

-------------------------------------------------------------------------------
--- Public functions.
-- @section public

function M.SayHello(request, replier)
    assert("table" == type(request))
    assert("table" == type(replier))
    -- replier:reply() can be called later after return.
    local response = { message = request.name }
    replier:reply(response);
end  -- SayHello()

return M
