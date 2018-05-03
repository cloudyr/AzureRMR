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
    resources=NULL, # char() = no resources, NULL = not yet populated
    token=NULL,

    initialize=function(token, subscription, name) # TODO: also allow initialisation with explicit data args
    {
        self$token <- token
        self$subscription <- subscription
        self$name <- name

        op <- paste0("resourcegroups/", self$name)
        cont <- call_azure_rm(self$token, self$subscription, op)
        self$id <- cont$id
        self$location <- cont$location
        self$managed_by <- cont$managedBy
        self$properties <- cont$properties
        self$tags <- cont$tags

        private$set_reslist()
        NULL
    },

    create_resource=function(...) { },
    update_resource=function(...) { },
    get_resource=function(...) { },
    delete_resource=function(...) { },
    list_resources=function() { }
),

private=list(

    set_reslist=function()
    {
        op <- paste0("resourcegroups/", self$name, "/resources")
        cont <- call_azure_rm(self$token, self$subscription, op)
        self$resources <- sapply(cont$value, `[[`, "name")
        NULL
    }
))
