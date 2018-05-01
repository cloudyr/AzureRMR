
#' @export
az_subscription <- R6::R6Class("az_subscription",
public=list(
    subscription=NULL,
    resource_groups=NULL,

    initialize=function(token, subscription)
    {
        private$token <- token
        self$subscription <- subscription
        private$set_rgrps()
        NULL
    },

    create_resource_group=function(resource_group) { },
    get_resource_group=function(resource_group) { },
    delete_resource_group=function(resource_group) { }
),

private=list(
    token=NULL,
    set_rgrps=function()
    {
        res <- call_azure_sm(private$token, self$subscription, operation="resourcegroups", api_version="2018-02-01")
        httr::stop_for_status(res)

        cont <- httr::content(res)$value
        self$resource_groups <- sapply(cont, `[[`, "name")
        NULL
    }
))
