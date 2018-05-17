### base Resource Manager class

#' Azure Resource Manager
#'
#' Base class for interacting with Azure Resource Manager.
#'
#' @docType class
#' @section Methods:
#' - `new(tenant, app, ...)`: Initialize a new ARM connection with the given credentials. See 'Authentication` for more details.
#' - `list_subscriptions()`: Returns a list of objects, one for each subscription associated with this app ID.
#' - `get_subscription(id)`: Returns an object representing a subscription.
#'
#' @section Authentication:
#' To authenticate with ARM, provide the following arguments to the `new` method:
#' - `tenant`: Your tenant ID.
#' - `app`: Your client/app ID which you registered in Azure Active Directory.
#' - `auth_type`: Either `"client_credentials"` (the default) or `"device_code"`.
#' - `secret`: if `auth_type == "client_credentials"`, your secret key.
#' - `host`: your ARM host. Defaults to `https://management.azure.com/`. Change this if you are using a government or private cloud.
#' - `aad_host`: Azure Active Directory host for authentication. Defaults to `https://login.microsoftonline.com/`. Change this if you are using a government or private cloud.
#' - `config_file`: Optionally, a JSON file containing any of the arguments listed above. Arguments supplied in this file take priority over those supplied on the command line.
#' - `token`: Optionally, an OAuth 2.0 token, of class [AzureToken]. This allows you to reuse the authentication details for an existing session. If supplied, all other arguments will be ignored.
#'
#' @seealso
#' [get_azure_token], [AzureToken],
#' [Azure Resource Manager overview](https://docs.microsoft.com/en-us/azure/azure-resource-manager/resource-group-overview),
#' [REST API reference](https://docs.microsoft.com/en-us/rest/api/resources/)
#'
#' @format An R6 object of class `az_rm`.
#' @export
az_rm <- R6::R6Class("az_rm",

public=list(
    host=NULL,
    tenant=NULL,
    token=NULL,

    # authenticate and get subscriptions
    initialize=function(tenant, app, auth_type="client_credentials", secret,
                        host="https://management.azure.com/", aad_host="https://login.microsoftonline.com/",
                        config_file=NULL, token=NULL)
    {
        if(is_azure_token(token))
        {
            self$host <- token$credentials$resource
            self$tenant <- sub("/.+$", "", httr::parse_url(token$endpoint$authorize)$path)
            self$token <- token
            return(NULL)
        }

        if(!is.null(config_file))
        {
            conf <- jsonlite::fromJSON(config_file)
            if(!is.null(conf$tenant)) tenant <- conf$tenant
            if(!is.null(conf$app)) app <- conf$app
            if(!is.null(conf$auth_type)) auth_type <- conf$auth_type
            if(!is.null(conf$secret)) secret <- conf$secret
            if(!is.null(conf$host)) host <- conf$host
            if(!is.null(conf$aad_host)) aad_host <- conf$aad_host
        }
        self$host <- host
        self$tenant <- tenant
        self$token <- get_azure_token(aad_host, tenant, app, auth_type, secret, host)
        NULL
    },

    # return a subscription object
    get_subscription=function(id)
    {
        az_subscription$new(self$token, id)
    },

    # return all subscriptions for this app
    list_subscriptions=function()
    {
        cont <- call_azure_rm(self$token, subscription="", operation="")
        lst <- lapply(cont$value, function(parms) az_subscription$new(self$token, parms=parms))
        # keep going until paging is complete
        while(!is_empty(cont$nextLink))
        {
            cont <- call_azure_url(self$token, cont$nextLink)
            lst <- c(lst, lapply(cont$value, function(parms) az_subscription$new(self$token, parms=parms)))
        }
        named_list(lst, "id")
    },

    print=function(...)
    {
        cat("<Azure Resource Manager client>\n")
        cat(format_auth_header(self$token))
        cat(format_public_methods(self))
        invisible(NULL)
    }
))

