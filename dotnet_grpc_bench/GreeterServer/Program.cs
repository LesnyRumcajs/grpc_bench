#region Copyright notice and license

// Copyright 2019 The gRPC Authors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#endregion

using GreeterServer;
using GreeterServer.Services;
using Microsoft.AspNetCore.Server.Kestrel.Core;

var builder = WebApplication.CreateBuilder(args);

builder.WebHost.ConfigureKestrel(options =>
{
	options.AddServerHeader = false;
	options.ListenAnyIP(50051, listenOptions =>
	{
		listenOptions.Protocols = HttpProtocols.Http2;
	});
});

builder.Logging.ClearProviders();

builder.Services.AddGrpc(o => o.IgnoreUnknownServices = true);
builder.Services.Configure<RouteOptions>(c => c.SuppressCheckForUnhandledSecurityMetadata = true);
builder.Services.AddSingleton<GreeterService>();

var app = builder.Build();

app.Lifetime.ApplicationStarted.Register(() => Console.WriteLine("Application started."));
app.UseMiddleware<ServiceProvidersMiddleware>();
app.MapGrpcService<GreeterService>();

app.Run();