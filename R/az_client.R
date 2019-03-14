#' AzureRMR client class
#'
#' Container class for interacting with Azure Resource Manager and Azure AD Graph.
#'
#' @docType class
#' @section Methods:
#' This class exposes the methods of the underlying `az_rm` and `az_graph` classes, via active bindings. See the documentation for those classes. It includes one additional method:
#'
#' `refresh()`: Refreshes the tokens for the `az_rm` and `az_graph` component objects.
#'
#' @section Initialization:
#' The recommended way to create new instances of this class is via the [create_azure_login] and [get_azure_login] functions.
#'
#' @seealso
#' [create_azure_login], [get_azure_login], [az_graph], [az_rm]
#'
#' [Azure Resource Manager overview](https://docs.microsoft.com/en-us/azure/azure-resource-manager/resource-group-overview),
#' [REST API reference](https://docs.microsoft.com/en-us/rest/api/resources/)
#'
#' [Azure AD Graph overview](https://docs.microsoft.com/en-us/azure/active-directory/develop/active-directory-graph-api),
#' [REST API reference](https://docs.microsoft.com/en-au/previous-versions/azure/ad/graph/api/api-catalog)
#'
#' @format An R6 object of class `az_client`.
#' @export
az_client <- R6::R6Class("az_client",

public=list(

    tenant=NULL,
    arm=NULL,
    graph=NULL,

    initialize=function(tenant, arm_client, graph_client)
    {
        self$tenant <- tenant
        self$arm <- arm_client
        self$graph <- graph_client
    },

    refresh=function()
    {
        self$graph$token$refresh()
        self$arm$token$refresh()
    },

    print=function(...)
    {
        cat("<AzureRMR client>\n")
        tenant <- if(self$tenant == "common")
            "common/myorganization"
        else self$tenant
        cat("  tenant:", tenant, "\n")
        cat("  app:", self$arm$token$client$client_id, "\n")
        cat("---\n")
        cat(format_public_methods(self))
        invisible(self)
    }
),

active=list(

    # dispatch to real methods
    get_subscription=function()
    self$arm$get_subscription,

    get_subscription_by_name=function()
    self$arm$get_subscription_by_name,

    list_subscriptions=function()
    self$arm$list_subscriptions,

    create_app=function()
    self$graph$create_app,

    get_app=function()
    self$graph$get_app,

    delete_app=function()
    self$graph$delete_app,

    create_service_principal=function()
    self$graph$create_service_principal,

    get_service_principal=function()
    self$graph$get_service_principal,

    delete_service_principal=function()
    self$graph$delete_service_principal
))
