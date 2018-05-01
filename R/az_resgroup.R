#' @export
az_resource_group <- R6::R6Class("az_resource_group",

public=list(
    subscription=NULL,
    id=NULL,
    name=NULL,
    location=NULL,
    managed_by=NULL,
    properties=NULL,
    tags=NULL,
    resources=NA, # NULL = no resources, NA = not yet populated

    initialize=function(token, subscription, name)  # TODO: also allow initialisation with explicit data args
    {
        private$token <- token
        self$subscription <- subscription
        self$name <- name

        op <- paste0("resourcegroups/", self$name)
        cont <- call_azure_sm(private$token, self$subscription, operation=op, api_version="2018-05-01")
        self$id <- cont$id
        self$location <- cont$location
        self$managed_by <- cont$managedBy
        self$properties <- cont$properties
        self$tags <- cont$tags

        private$set_res()
        NULL
    },

    create_resource=function(...) { },
    get_resource=function(...) { },
    delete_resource=function(...) { },
    list_resources=function() { }
),

private=list(
    token=NULL,
    set_res=function()
    {
        op <- paste0("resourcegroups/", self$name, "/resources")
        cont <- call_azure_sm(private$token, self$subscription, operation=op, api_version="2018-05-01")
        self$resources <- sapply(cont$value, `[[`, "name")
        NULL
    }
))
