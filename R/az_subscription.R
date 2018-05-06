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

    # API versions vary across different providers; find the latest
    get_provider_api_version=function(provider=NULL, resource_type=NULL, which=1)
    {
        if(is_empty(provider))
        {
            apis <- named_list(call_azure_rm(self$token, self$id, "providers")$value, "namespace")
            lapply(apis, function(api)
            {
                api <- named_list(api$resourceTypes, "resourceType")
                sapply(api, function(x) x$apiVersions[[which]])
            })
        }
        else
        {
            op <- file.path("providers", provider)
            apis <- named_list(call_azure_rm(self$token, self$id, op)$resourceTypes, "resourceType")
            if(!is_empty(resource_type))
            {
                this_api <- apis[[resource_type]]
                this_api$apiVersions[[which]]
            }
            else sapply(apis, function(x) x$apiVersions[[which]])
        }
    },

    get_resource_group=function(name)
    {
        az_resource_group$new(self$token, self$id, name)
    },

    list_resource_groups=function()
    {
        # TODO: handle paging
        cont <- call_azure_rm(self$token, self$id, "resourcegroups")$value
        lst <- lapply(cont, function(parms) az_resource_group$new(self$token, self$id, parms=parms))
        named_list(lst)
    },

    create_resource_group=function(name, location)
    {
        az_resource_group$new(self$token, self$id, name, location=location, create=TRUE)
    },

    delete_resource_group=function(name)
    {
        self$get_resource_group(name)$delete()
    },

    list_resources=function()
    {
        # TODO: handle paging
        cont <- call_azure_rm(self$token, self$id, "resources")$value
        lst <- lapply(cont, function(parms) az_resource$new(self$token, self$id, deployed_properties=parms))
        named_list(lst)
    }
))
