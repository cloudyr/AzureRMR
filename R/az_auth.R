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
#' The recommended way to authenticate with ARM is via the [get_azure_login] function, which creates a new instance of this class.
#'
#' To authenticate with the `az_rm` class directly, provide the following arguments to the `new` method:
#' - `tenant`: Your tenant ID. This can be a name ("myaadtenant"), a fully qualified domain name ("myaadtenant.onmicrosoft.com" or "mycompanyname.com"), or a GUID.
#' - `app`: The client/app ID to use to authenticate with Azure Active Directory.
#' - `password`: if `auth_type == "client_credentials"`, the app secret; if `auth_type == "resource_owner"`, your account password.
#' - `username`: if `auth_type == "resource_owner"`, your username.
#' - `auth_type`: The OAuth authentication method to use, one of "client_credentials", "authorization_code", "device_code" or "resource_owner". See [get_azure_token] for how the default method is chosen, along with some caveats.
#' - `host`: your ARM host. Defaults to `https://management.azure.com/`. Change this if you are using a government or private cloud.
#' - `aad_host`: Azure Active Directory host for authentication. Defaults to `https://login.microsoftonline.com/`. Change this if you are using a government or private cloud.
#' - `config_file`: Optionally, a JSON file containing any of the arguments listed above. Arguments supplied in this file take priority over those supplied on the command line. You can also use the output from the Azure CLI `az ad sp create-for-rbac` command.
#' - `token`: Optionally, an OAuth 2.0 token, of class [AzureToken]. This allows you to reuse the authentication details for an existing session. If supplied, all other arguments will be ignored.
#'
#' @seealso
#' [create_azure_login], [get_azure_token], [AzureToken],
#' [Azure Resource Manager overview](https://docs.microsoft.com/en-us/azure/azure-resource-manager/resource-group-overview),
#' [REST API reference](https://docs.microsoft.com/en-us/rest/api/resources/)
#'
#' @examples
#' \dontrun{
#'
#' # start a new Resource Manager session
#' az <- az_rm$new(tenant="myaadtenant.onmicrosoft.com", app="app_id", password="password")
#'
#' # authenticate with credentials in a file
#' az <- az_rm$new(config_file="creds.json")
#'
#' # authenticate with device code
#' az <- az_rm$new(tenant="myaadtenant.onmicrosoft.com", app="app_id", auth_type="device_code")
#'
#' # retrieve a list of subscription objects
#' az$list_subscriptions()
#'
#' # a specific subscription
#' az$get_subscription("subscription_id")
#'
#' }
#' @format An R6 object of class `az_rm`.
#' @export
az_rm <- R6::R6Class("az_rm",

public=list(
    host=NULL,
    tenant=NULL,
    token=NULL,

    # authenticate and get subscriptions
    initialize=function(tenant, app, password=NULL, username=NULL, auth_type=NULL,
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
            if(!is.null(conf$password)) password <- conf$password
            if(!is.null(conf$host)) host <- conf$host
            if(!is.null(conf$aad_host)) aad_host <- conf$aad_host
        }
        self$host <- host
        self$tenant <- normalize_tenant(tenant)
        app <- normalize_guid(app)
        self$token <- get_azure_token(self$host, self$tenant, app, password, username, auth_type, aad_host)
        NULL
    },

    # return a subscription object
    get_subscription=function(id)
    {
        az_subscription$new(self$token, id)
    },

    # return a subscription object given its name
    get_subscription_by_name=function(name)
    {
        subs <- self$list_subscriptions()
        found <- sapply(subs, function(x) x$name == name)
        if(!any(found))
            stop("Subscription '", name, "' not found", call.=FALSE)
        subs[[found]]
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

