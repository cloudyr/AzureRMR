#' Login to Azure Resource Manager
#'
#' @param tenant The Azure Active Directory tenant for which to obtain a login client. Can be a name ("myaadtenant"), a fully qualified domain name ("myaadtenant.onmicrosoft.com" or "mycompanyname.com"), or a GUID.
#' @param app The client/app ID to use to authenticate with Azure Active Directory.
#' @param password If `auth_type == "client_credentials"`, the app secret; if `auth_type == "resource_owner"`, your account password.
#' @param username If `auth_type == "resource_owner"`, your username.
#' @param auth_type The OAuth authentication method to use, one of "client_credentials", "authorization_code", "device_code" or "resource_owner". See [get_azure_token] for how the default method is chosen.
#' @param host Your ARM host. Defaults to `https://management.azure.com/`. Change this if you are using a government or private cloud.
#' @param aad_host Azure Active Directory host for authentication. Defaults to `https://login.microsoftonline.com/`. Change this if you are using a government or private cloud.
#' @param config_file Optionally, a JSON file containing any of the arguments listed above. Arguments supplied in this file take priority over those supplied on the command line. You can also use the output from the Azure CLI `az ad sp create-for-rbac` command.
#' @param refresh For `get_azure_login`, whether to refresh the authentication token on loading the client.
#' @param confirm For `delete_azure_login`, whether to ask for confirmation before deleting.
#' @param ... Other arguments passed to `az_rm$new()`.
#'
#' @details
#' This function creates a login client to authenticate with Azure Resource Manager (ARM), using the supplied arguments. The Azure Active Directory (AAD) authentication token is obtained using [get_azure_token], which automatically caches and reuses tokens for subsequent sessions.
#'
#' @section Authentication methods:
#' The OAuth authentication type can be one of four possible values: "authorization_code", "client_credentials", "device_code", or "resource_owner". The first two are provided by the [httr::Token2.0] token class, while the last two are provided by the AzureToken class which extends httr::Token2.0. Here is a short description of these methods.
#'
#' 1. Using the authorization_code method is a 3-step process. First, `get_azure_login` contacts the AAD authorization endpoint to obtain a temporary access code. It then contacts the AAD access endpoint, passing it the code. The access endpoint sends back a login URL which `get_azure_login` opens in your browser, where you can enter your credentials. Once this is completed, the endpoint returns the OAuth token via a HTTP redirect URI.
#'
#' 2. The device_code method is similar in concept to authorization_code, but is meant for situations where you are unable to browse the Internet -- for example if you don't have a browser installed or your computer has input constraints. First, `get_azure_login` contacts the AAD devicecode endpoint, which responds with a login URL and an access code. You then visit the URL and enter the code, possibly using a different computer. Meanwhile, `get_azure_login` polls the AAD access endpoint for a token, which is provided once you have successfully entered the code.
#'
#' 3. The client_credentials method is much simpler than the above methods, requiring only one step. `get_azure_login` contacts the access endpoint, passing it the app secret (which you supplied in the `password` argument). Assuming the secret is valid, the endpoint then returns the OAuth token.
#'
#' 4. The resource_owner method also requires only one step. In this method, `get_azure_login` passes your (personal) username and password to the AAD access endpoint, which validates your credentials and returns the token.
#'
#' If the authentication method is not specified, it is chosen based on the presence or absence of the `password` and `username` arguments:
#'
#' - Password and username present: resource_owner. 
#' - Password and username absent: authorization_code if the httpuv package is installed, device_code otherwise
#' - Password present, username absent: client_credentials
#' - Password absent, username present: error
#'
#' The httpuv package must be installed to use the authorization_code method, as this requires a web server to listen on the (local) redirect URI. See [httr::oauth2.0_token] for more information; note that Azure does not support the `use_oob` feature of the httr OAuth 2.0 token class.
#'
#' Similarly, since the authorization_code method opens a browser to load the AAD authorization page, your machine must have an Internet browser installed that can be run from inside R. In particular, if you are using a Linux [Data Science Virtual Machine](https://azure.microsoft.com/en-us/services/virtual-machines/data-science-virtual-machines/) in Azure, you may run into difficulties; use one of the other methods instead.
#'
#' @section Value:
#' An object of class `az_rm`, representing the ARM login client.
#'
#' @seealso
#' [az_rm], [get_azure_token], [Azure CLI documentation](https://docs.microsoft.com/en-us/cli/azure/?view=azure-cli-latest)
#' 
#' @examples
#' \dontrun{
#'
#' # this will create a Resource Manager client for the AAD tenant 'microsoft.onmicrosoft.com',
#' # using the client_credentials method
#' az <- get_azure_login("microsoft", app="{app_id}", password="{password}")
#'
#' # you can also login using credentials in a json file
#' az <- get_azure_login(config_file="~/creds.json")
#'
#' }
#' @rdname azure_login
#' @export
create_azure_login <- function(tenant, app, password=NULL, username=NULL, auth_type=NULL,
                               host="https://management.azure.com/", aad_host="https://login.microsoftonline.com/",
                               config_file=NULL, ...)
{
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

    hash <- token_hash_from_original_args(resource_host=host,
        tenant=tenant,
        app=app,
        password=password,
        username=username,
        auth_type=auth_type,
        aad_host=aad_host)
    tokenfile <- file.path(config_dir(), hash)
    if(file.exists(tokenfile))
    {
        message("Deleting existing Azure Active Directory token for this set of credentials")
        file.remove(tokenfile)
    }

    tenant <- normalize_tenant(tenant)
    app <- normalize_guid(app)

    message("Creating Azure Resource Manager login for tenant '", tenant, "'")
    client <- az_rm$new(tenant, app, password, username, auth_type, host, aad_host, config_file, ...)

    # save login info for future sessions
    arm_logins <- load_arm_logins()
    arm_logins[[tenant]] <- unique(c(arm_logins[[tenant]], client$token$hash()))
    save_arm_logins(arm_logins)

    client
}


#' @rdname azure_login
#' @export
get_azure_login <- function(tenant, ..., refresh=TRUE)
{
    tenant <- normalize_tenant(tenant)

    arm_logins <- load_arm_logins()
    this_login <- arm_logins[[tenant]]
    if(is_empty(this_login))
        create_azure_login(tenant, ...)

    choice <- if(length(this_login) > 1)
        utils::menu(this_login)
    else 1

    message("Loading Azure Resource Manager login for tenant '", tenant, "'")
    token <- readRDS(file.path(config_dir(), this_login[choice]))
    client <- az_rm$new(token=token)

    if(refresh)
        client$token$refresh()

    client
}


#' @rdname azure_login
#' @export
delete_azure_login <- function(tenant, confirm=TRUE)
{
    tenant <- normalize_tenant(tenant)

    if(confirm && interactive())
    {
        yn <- readline(
            paste0("Do you really want to delete the Azure Resource Manager login(s) for tenant ",
                   tenant, "? (y/N) "))
        if(tolower(substr(yn, 1, 1)) != "y")
            return(invisible(NULL))
    }

    arm_logins <- load_arm_logins()
    arm_logins[[tenant]] <- NULL
    save_arm_logins(arm_logins)
    invisible(NULL)
}


#' @rdname azure_login
#' @export
list_azure_logins <- function()
{
    arm_logins <- load_arm_logins()
    logins <- sapply(arm_logins, function(tenant)
    {
        sapply(tenant, function(hash)
        {
            file <- file.path(config_dir(), hash)
            az_rm$new(token=readRDS(file))
        }, simplify=FALSE)
    }, simplify=FALSE)

    logins
}


load_arm_logins <- function()
{
    file <- file.path(config_dir(), "arm_logins.json")
    jsonlite::fromJSON(file)
}


save_arm_logins <- function(logins)
{
    if(is_empty(logins))
        names(logins) <- character(0)

    file <- file.path(config_dir(), "arm_logins.json")
    writeLines(jsonlite::toJSON(logins, auto_unbox=TRUE, pretty=TRUE), file)
    invisible(NULL)
}

