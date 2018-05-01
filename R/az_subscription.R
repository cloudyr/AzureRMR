### Azure subscription class: all info about a subscription

#' @export
az_subscription <- R6::R6Class("az_subscription",
public=list(
    id=NULL,
    name=NULL,
    state=NULL,
    policies=NULL,
    authorization_source=NULL,
    resource_groups=NA, # NULL = no resource groups, NA = not yet populated

    initialize=function(token, id)
    {
        private$token <- token
        self$id <- id
        info <- call_azure_sm(token, id, "", api_version="2018-05-01")
        self$name <- info$displayName
        self$state <- info$state
        self$policies <- info$subscriptionPolicies
        self$authorization_source <- info$authorizationSource

        private$set_rgrps()
        NULL
    },

    # return a resource group object
    get_resource_group=function(resource_group)
    {
        if(is.null(self$resource_groups))
            stop("No resource groups associated with this subscription")
        if(is.numeric(resource_group))
            resource_group <- self$resource_groups[resource_group][1]
        az_resource_group$new(private$token, self$id, resource_group)
    },

    create_resource_group=function(resource_group) { },
    delete_resource_group=function(resource_group) { },
    list_resource_groups=function() { },
    list_resources=function() { }
),

private=list(
    token=NULL,
    set_rgrps=function()
    {
        cont <- call_azure_sm(private$token, self$id, operation="resourcegroups", api_version="2018-05-01")
        self$resource_groups <- sapply(cont$value, `[[`, "name")
        NULL
    }
))
