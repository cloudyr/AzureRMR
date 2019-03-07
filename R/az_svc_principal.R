az_service_principal <- R6::R6Class("az_service_principal",

public=list(

    token=NULL,
    tenant=NULL,

    # app data from server
    properties=NULL,

    initialize=function(token, tenant=NULL, object_id=NULL, app_id=NULL, ..., deployed_properties=list(), mode="get")
    {
        self$token <- token
        self$tenant <- tenant

        self$properties <- if(!is_empty(list(...)) || mode == "create")
            private$init_and_deploy(..., password=password, password_duration=password_duration)
        else if(!is_empty(deployed_properties))
            private$init_from_parms(deployed_properties)
        else private$init_from_host(object_id, app_id)
    },

    delete=function(confirm=TRUE)
    {
        if(confirm && interactive())
        {
            msg <- paste0("Do you really want to delete the '", self$properties$displayName,
                          "' service principal? (y/N) ")
            yn <- readline(msg)
            if(tolower(substr(yn, 1, 1)) != "y")
                return(invisible(NULL))
        }

        op <- file.path("servicePrincipals", self$properties$objectId)
        call_azure_graph(self$token, self$tenant, op, http_verb="DELETE")
        invisible(NULL)
    }
),

private=list(

    init_and_deploy=function(...)
    {
        properties <- list(...)

        call_azure_graph(self$token, self$tenant, "servicePrincipals", body=properties, encode="json", http_verb="POST")
    },

    init_from_parms=function(parms)
    {
        parms
    },

    init_from_host=function(object_id, app_id)
    {
        op <- if(is.null(object_id))
            file.path("servicePrincipalsByAppId", app_id)
        else file.path("servicePrincipals", object_id)

        call_azure_graph(self$token, self$tenant, op)
    }
))
