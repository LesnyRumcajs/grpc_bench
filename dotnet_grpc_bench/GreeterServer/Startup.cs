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

using System;
using GreeterServer.Services;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Routing;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;

namespace GreeterServer
{
    public class Startup
    {
        public void ConfigureServices(IServiceCollection services)
        {
            services.AddGrpc(o => o.IgnoreUnknownServices = true);
            services.Configure<RouteOptions>(c => c.SuppressCheckForUnhandledSecurityMetadata = true);
            services.AddSingleton<GreeterService>();
        }

        public void Configure(IApplicationBuilder app, IHostApplicationLifetime applicationLifetime)
        {
            applicationLifetime.ApplicationStarted.Register(() => Console.WriteLine("Application started."));

            // Configure ASP.NET Core to not use a per-request DI scope
            app.UseMiddleware<ServiceProvidersMiddleware>();

            app.UseRouting();

            app.UseEndpoints(endpoints =>
            {
                endpoints.MapGrpcService<GreeterService>();
            });
        }
    }
}
