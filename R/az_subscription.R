### Azure subscription class: all info about a subscription

#' Azure subscription class
#'
#' Class representing an Azure subscription.
#'
#' @docType class
#' @section Methods:
#' - `new(token, id, ...)`: Initialize a subscription object.
#' - `list_resource_groups()`: Return a list of resource group objects for this subscription.
#' - `get_resource_group(name)`: Return an object representing an existing resource group.
#' - `create_resource_group(name, location)`: Create a new resource group in the specified region/location, and return an object representing it.
#' - `delete_resource_group(name)`: Delete a resource group.
#' - `list_resources()`: List all resources deployed under this subscription.
#' - `list_locations()`: List locations available.
#' - `get_provider_api_version(provider, type)`: Get the current API version for the given resource provider and type. If no resource type is supplied, returns a vector of API versions, one for each resource type for the given provider. If neither provider nor type is supplied, returns the API versions for all resources and providers.
#'
#' @section Details:
#' Generally, the easiest way to create a subscription object is via the `get_subscription` or `list_subscriptions` methods of the [az_rm] class. To create a subscription object in isolation, call the `new()` method and supply an Oauth 2.0 token of class [AzureToken], along with the ID of the subscription.
#'
#' @seealso
#' [Azure Resource Manager overview](https://docs.microsoft.com/en-us/azure/azure-resource-manager/resource-group-overview)
#'
#' @format An R6 object of class `az_subscription`.
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
    get_provider_api_version=function(provider=NULL, type=NULL, which=1)
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
            op <- construct_path("providers", provider)
            apis <- named_list(call_azure_rm(self$token, self$id, op)$resourceTypes, "resourceType")
            if(!is_empty(type))
            {
                # case-insensitive matching
                names(apis) <- tolower(names(apis))
                this_api <- apis[[tolower(type)]]
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
        cont <- call_azure_rm(self$token, self$id, "resourcegroups")
        lst <- lapply(cont$value, function(parms) az_resource_group$new(self$token, self$id, parms=parms))
        # keep going until paging is complete
        while(!is_empty(cont$nextLink))
        {
            cont <- call_azure_url(self$token, cont$nextLink)
            lst <- c(lst, lapply(cont$value, function(parms) az_resource_group$new(self$token, self$id, parms=parms)))
        }
        named_list(lst)
    },

    create_resource_group=function(name, location, ...)
    {
        az_resource_group$new(self$token, self$id, name, location=location, ...)
    },

    delete_resource_group=function(name)
    {
        self$get_resource_group(name)$delete()
    },

    list_resources=function()
    {
        cont <- call_azure_rm(self$token, self$id, "resources")
        lst <- lapply(cont$value, function(parms) az_resource$new(self$token, self$id, deployed_properties=parms))
        # keep going until paging is complete
        while(!is_empty(cont$nextLink))
        {
            cont <- call_azure_url(self$token, cont$nextLink)
            lst <- c(lst, lapply(cont$value,
                function(parms) az_resource$new(self$token, self$id, deployed_properties=parms)))
        }
        named_list(lst, c("type", "name"))
    },

    print=function(...)
    {
        cat("<Azure subscription ", self$id, ">\n", sep="")
        cat(format_public_fields(self, exclude="id"))
        cat(format_public_methods(self))
        invisible(NULL)
    }
))
