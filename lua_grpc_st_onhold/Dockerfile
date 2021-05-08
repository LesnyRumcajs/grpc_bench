FROM python:3.9-rc

WORKDIR /app
RUN pip install conan
RUN apt update && apt install -y cmake

COPY lua_grpc_st_bench/conanfile.txt /app/conanfile.txt
COPY proto/helloworld/helloworld.proto /app/helloworld.proto

RUN conan remote add remote_bintray_conan-community https://api.bintray.com/conan/conan-community/conan
RUN conan remote add remote_bintray_bincrafters https://api.bintray.com/conan/bincrafters/public-conan
RUN conan remote add remote_bintray_inexorgame https://api.bintray.com/conan/inexorgame/inexor-conan
RUN conan remote add remote_bintray_conan https://api.bintray.com/conan/conan/conan-transit
RUN conan remote add remote_bintray_jinq0123 https://api.bintray.com/conan/jinq0123/test

RUN conan install lua-cpp/5.3.4@jinq0123/testing --build=missing
RUN conan install luapbintf/0.1@jinq0123/testing --build=missing
RUN conan install grpc-lua/0.1@jinq0123/testing --build=missing

COPY lua_grpc_st_bench /app

RUN cp $(find / -name 'lua-cpp' -executable -type f | head -n 1) .
RUN cp $(find / -name 'libluapbintf.so' -type f | head -n 1) /app/luapbintf.so
RUN cp $(find / -name 'libgrpc_lua.so' -type f | head -n 1) /app/grpc_lua.so
RUN cp $(find / -name 'liblua-cpp.so' -type f | head -n 1) .

RUN git clone https://github.com/jinq0123/grpc-lua.git

ENTRYPOINT [ "/app/lua-cpp", "greeter_server.lua" ]
