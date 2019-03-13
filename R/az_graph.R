#' @export
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
            if(!is.null(conf$graph_host)) host <- conf$graph_host
            if(!is.null(conf$aad_host)) aad_host <- conf$aad_host
        }

        self$host <- host
        self$tenant <- normalize_graph_tenant(tenant)
        tenant <- normalize_tenant(tenant)
        app <- normalize_guid(app)

        self$token <- get_azure_token(self$host, 
            tenant=tenant,
            app=app,
            password=password,
            username=username,
            auth_type=auth_type, 
            aad_host=aad_host)
        NULL
    },

    create_app=function(name, ..., password=NULL, create_service_principal=TRUE)
    {
        res <- az_app$new(self$token, self$tenant, displayName=name, password=password, ...)
        if(create_service_principal)
            res$create_service_principal()
        res
    },

    get_app=function(app_id=NULL, object_id=NULL)
    {
        az_app$new(self$token, self$tenant, app_id, object_id)
    },

    delete_app=function(app_id=NULL, object_id=NULL, confirm=TRUE)
    {
        self$get_app(app_id, object_id)$delete(confirm=confirm)
    },

    create_service_principal=function(app_id, ...)
    {
        az_service_principal$new(self$token, self$tenant, app_id=app_id, ..., mode="create")
    },

    get_service_principal=function(app_id=NULL, object_id=NULL)
    {
        az_service_principal$new(self$token, self$tenant, app_id, object_id, mode="get")
    },

    delete_service_principal=function(app_id=NULL, object_id=NULL, confirm=TRUE)
    {
        self$get_service_principal(app_id, object_id)$delete(confirm=confirm)
    },

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


normalize_graph_tenant <- function(tenant)
{
    tenant <- tolower(tenant)
    if(tenant == "common")
        "myorganization"
    else normalize_tenant(tenant)
}
