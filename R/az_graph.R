az_graph <- R6::R6Class("az_graph",

public=list(
    host=NULL,
    tenant=NULL,
    token=NULL,

    # authenticate and get subscriptions
    initialize=function(tenant, app=.az_cli_app_id, password=NULL, username=NULL, auth_type=NULL,
                        host="https://graph.windows.net/", aad_host="https://login.microsoftonline.com/",
                        config_file=NULL, token=NULL)
    {
        if(is_azure_token(token))
        {
            self$host <- if(token$version == 1)
                token$resource
            else token$scope
            self$tenant <- if(token$tenant == "common")
                "myorganization"
            else token$tenant
            self$token <- token
            return(NULL)
        }

        if(!is.null(config_file))
        {
            conf <- jsonlite::fromJSON(config_file)
            if(!is.null(conf$tenant)) tenant <- conf$tenant
            if(!is.null(conf$app)) app <- conf$app
            if(!is.null(conf$auth_type)) auth_type <- conf$auth_type
            if(!is.null(conf$password)) password <- conf$password
            if(!is.null(conf$host)) host <- conf$host
            if(!is.null(conf$aad_host)) aad_host <- conf$aad_host
        }

        aad_tenant <- if(missing(tenant) || tenant == "myorganization")
            "common"
        else tenant

        self$host <- host
        self$tenant <- normalize_graph_tenant(tenant)
        aad_tenant <- normalize_tenant(aad_tenant)
        app <- normalize_guid(app)

        self$token <- get_azure_token(self$host, 
            tenant=aad_tenant,
            app=app,
            password=password,
            username=username,
            auth_type=auth_type, 
            aad_host=aad_host)
        NULL
    },

    create_app=function()
    {},

    get_app=function()
    {},

    delete_app=function()
    {},

    list_apps=function()
    {},

    create_service_principal=function()
    {},

    get_service_principal=function()
    {},

    delete_service_principal=function()
    {},

    list_service_principals=function()
    {},

    print=function(...)
    {
        cat("<Azure Active Directory Graph client>\n")
        cat("<Authentication>\n")
        fmt_token <- gsub("\n  ", "\n    ", format_auth_header(self$token))
        cat(" ", fmt_token)
        cat("---\n")
        cat(format_public_methods(self))
        invisible(self)
    }
))
