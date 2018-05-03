### Azure subscription class: all info about a subscription

#' @export
az_subscription <- R6::R6Class("az_subscription",

public=list(
    id=NULL,
    name=NULL,
    state=NULL,
    policies=NULL,
    authorization_source=NULL,
    resource_groups=NULL, # char() = no resource groups, NULL = not yet populated
    token=NULL,

    initialize=function(token, id)
    {
        self$token <- token
        self$id <- id
        info <- call_azure_rm(token, id, "")
        self$name <- info$displayName
        self$state <- info$state
        self$policies <- info$subscriptionPolicies
        self$authorization_source <- info$authorizationSource

        private$set_rglist()
        NULL
    },

    # return a resource group object
    get_resource_group=function(resource_group)
    {
        if(is_empty(self$resource_groups))
            stop("No resource groups associated with this subscription")
        if(is.numeric(resource_group))
            resource_group <- self$resource_groups[resource_group]
        az_resource_group$new(self$token, self$id, resource_group[1])
    },

    create_resource_group=function(resource_group) { },
    update_resource_group=function(resource_group) { },
    delete_resource_group=function(resource_group) { },
    list_resource_groups=function() { },
    list_resources=function() { }
),

private=list(

    set_rglist=function()
    {
        cont <- call_azure_rm(self$token, self$id, "resourcegroups")
        self$resource_groups <- vapply(cont$value, `[[`, "name", FUN.VALUE=character(1))
        NULL
    }
))
