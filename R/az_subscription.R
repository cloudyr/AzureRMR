### Azure subscription class: all info about a subscription

#' @export
az_subscription <- R6::R6Class("az_subscription",

public=list(
    id=NULL,
    name=NULL,
    state=NULL,
    policies=NULL,
    authorization_source=NULL,
    token=NULL,

    initialize=function(token, id=NULL, parms=list())
    {
        if(is_empty(id) && is_empty(parms))
            stop("Must supply either subscription ID, or parameter list")

        self$token <- token

        if(is_empty(parms))
            parms <- call_azure_rm(self$token, subscription=id, operation="")

        self$id <- parms$subscriptionId
        self$name <- parms$displayName
        self$state <- parms$state
        self$policies <- parms$subscriptionPolicies
        self$authorization_source <- parms$authorizationSource
        NULL
    },

    get_resource_group=function(name)
    {
        az_resource_group$new(self$token, self$id, name)
    },

    list_resource_groups=function()
    {
        cont <- call_azure_rm(self$token, self$id, "resourcegroups")
        lst <- lapply(cont$value, function(parms) az_resource_group$new(self$token, self$id, parms=parms))
        named_list(lst)
    },

    list_locations=function()
    {
        cont <- call_azure_rm(self$token, self$id, "locations")
        locs <- do.call(rbind, lapply(cont$value, data.frame, stringsAsFactors=FALSE))
        within(locs,
        {
            id <- NULL
            longitude <- as.numeric(longitude)
            latitude <- as.numeric(latitude)
        })
    },

    create_resource_group=function(name, location)
    {
        az_resource_group$new(self$token, self$id, name, location=location, create=TRUE)
    },

    delete_resource_group=function(name)
    {
        self$get_resource_group(name)$delete()
    },

    list_resources=function() { }
))
