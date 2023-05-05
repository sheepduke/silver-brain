namespace SilverBrain.Server

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Options
open Giraffe

open SilverBrain.Core
open SilverBrain.Store
open SilverBrain.Domain
open SilverBrain.Domain.ConceptMap

type ServerSettings =
    { RootDataDirectory: FilePath
      DefaultDatabaseName: DatabaseName }

module RestApi =
    let private createRequestContext (context: HttpContext) =
        let settings = context.GetService<IOptions<ServerSettings>>().Value

        let databaseName =
            match context.TryGetRequestHeader "X-SilverBrain-DatabaseName" with
            | None -> settings.DefaultDatabaseName
            | Some value -> DatabaseName value

        { RootDataDirectory = settings.RootDataDirectory
          DatabaseName = databaseName }


    let private getConcept (uuid: string) =
        fun (next: HttpFunc) (context: HttpContext) ->
            let requestContext = createRequestContext context

            let options =
                { LoadAliases = false
                  LoadTimes = true }

            task {
                let! concept = ConceptMap.getConcept requestContext options (Uuid uuid)

                let result = json concept next context
                return! result
            }

    let webApp = choose [ routef "/api/v2/concepts/%s" getConcept ]

    let start settings =
        let configureApp (app: IApplicationBuilder) = app.UseGiraffe webApp

        let configureServices (services: IServiceCollection) =
            services.AddGiraffe() |> ignore

            services.AddSingleton<IOptions<ServerSettings>>(Options.Create(settings))
            |> ignore


        // For all

        // Start the web server.
        Host
            .CreateDefaultBuilder()
            .ConfigureWebHostDefaults(fun webHostBuilder ->
                webHostBuilder.Configure(configureApp).ConfigureServices(configureServices)
                |> ignore)
            .Build()
            .Run()
