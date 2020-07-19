module server;

import GreeterImpl;
import grpc;
import hunt.logging;
import std.stdio;



void main()
{
    string host = "0.0.0.0";
    ushort port = 50051;

    Server server = new Server();
    server.listen(host , port);
    server.register( new GreeterImpl());
    server.start();

    writeln("Server started on ", host, ":", port);

    getchar();

}
