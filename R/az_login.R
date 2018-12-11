config_dir <- function()
{
    rappdirs::user_config_dir(appname="AzureRMR", appauthor="AzureR", roaming=FALSE)
}


#' Functions to login to Azure Resource Manager
#'
#' @param tenant The Azure Active Directory tenant for which to obtain a login client. Can be a name ("myaadtenant"), a fully qualified domain name ("myaadtenant.onmicrosoft.com" or "mycompanyname.com"), or a GUID.
#' @param app The app ID to authenticate with.
#' @param auth_type The type of authentication to use. Can be either "device_code" or "client_credentials". Use the latter if you supply a password.
#' @param auth_type Either `"client_credentials"` (the default) or `"device_code"`.
#' @param password If `auth_type == "client_credentials"`, your password.
#' @param host Your ARM host. Defaults to `https://management.azure.com/`. Change this if you are using a government or private cloud.
#' @param aad_host Azure Active Directory host for authentication. Defaults to `https://login.microsoftonline.com/`. Change this if you are using a government or private cloud.
#' @param refresh For `get_azure_login`, whether to refresh the authentication token on loading the client.
#' @param confirm For `delete_azure_login`, whether to ask for confirmation before deleting.
#'
#' @details
#' These functions allow you to login to Azure Resource Manager (ARM).
#' - `create_azure_login` will create a login client. R will display a code and prompt you to visit the Microsoft login page in your browser. You then enter the code along with your Azure Active Directory credentials, which completes the authentication process. You only have to create a login client once per tenant: the resulting ARM client object is saved on your machine and reused automatically in subsequent R sessions.
#' - `get_azure_login` will load a previously saved ARM client object for the given tenant. If this is the first time you are logging in for this tenant, the client object is created.
#' - `delete_azure_login` deletes the client object for the given tenant from your machine. Note that this doesn't invalidate any client you may be using in your R session.
#' - `list_azure_logins` lists client objects that have been previously saved.
#' - `refresh_azure_logins` refreshes all client objects existing on your machine.
#'
#' `create_azure_login` is roughly equivalent to the Azure CLI command `az login` with no arguments, and in fact uses the same app ID as the CLI by default.
#'
#' @return
#' For `create_azure_login` and `get_azure_login`, an object of class `az_rm`, representing the ARM client. For `list_azure_logins`, a list of such objects.
#'
#' @seealso
#' [az_rm], [Azure CLI documentation](https://docs.microsoft.com/en-us/cli/azure/?view=azure-cli-latest)
#' 
#' @examples
#' \dontrun{
#'
#' # this will create a Resource Manager client for the AAD tenant 'microsoft.onmicrosoft.com'
#' # only has to be run once per tenant
#' az <- create_azure_login("microsoft")
#'
#' # in subsequent sessions, you can retrieve the client without re-authenticating:
#' # authentication details will automatically be refreshed
#' az <- get_azure_login("microsoft")
#'
#' # refresh (renew) authentication details for clients for all tenants
#' refresh_azure_logins()
#'
#' }
#' @rdname azure_login
#' @export
create_azure_login <- function(tenant, app, auth_type, password, host, aad_host, ...)
{
    tenant <- normalize_tenant(tenant)

    message("Creating Resource Manager client for tenant ", tenant)
    client <- az_rm$new(tenant, app=app, ...)

    save_client(client, tenant)
    client
}


#' @rdname azure_login
#' @export
get_azure_login <- function(tenant, ..., refresh=TRUE)
{
    tenant <- normalize_tenant(tenant)

    login_exists <- file.exists(file.path(config_dir(), tenant))
    if(!login_exists)
        return(create_azure_login(tenant, ...))

    client <- readRDS(file.path(config_dir(), tenant))
    if(refresh)
    {
        # refresh and save
        client$token$refresh()
        save_client(client, tenant)
    }
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
            paste0("Do you really want to delete the Resource Manager client for tenant ", tenant, "? (y/N) "))
        if(tolower(substr(yn, 1, 1)) != "y")
            return(invisible(NULL))
    }

    file.remove(file.path(config_dir(), tenant))
    invisible(NULL)
}


#' @rdname azure_login
#' @export
list_azure_logins <- function()
{
    tenants <- dir(config_dir(), full.names=TRUE)
    lst <- lapply(tenants, readRDS)
    names(lst) <- basename(tenants)
    lst
}


#' @rdname azure_login
#' @export
refresh_azure_logins <- function()
{
    refresh_and_save <- function(tenant)
    {
        client <- readRDS(tenant)
        client$token$refresh()
        save_client(client, basename(tenant))
    }

    lapply(dir(config_dir(), full.names=TRUE), refresh_and_save)
    invisible(NULL)
}


save_client <- function(client, tenant)
{
    tenant <- normalize_tenant(tenant)

    saveRDS(client, file=file.path(config_dir(), tenant))
    invisible(client)
}


normalize_tenant <- function(tenant)
{
    # see https://docs.microsoft.com/en-us/dotnet/api/system.guid.parse
    # for possible input formats for GUIDs
    is_guid <- function(x)
    {
        grepl("^[0-9a-f]{32}$", x) ||
        grepl("^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$", x) ||
        grepl("^\\{[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\\}$", x) ||
        grepl("^\\([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\\)$", x)
    }

    # check if supplied a guid; if not, check if a fqdn; if not, append '.onmicrosoft.com'
    if(is_guid(tenant))
        return(tenant)

    if(!grepl("\\.", tenant))
        tenant <- paste(tenant, "onmicrosoft.com", sep=".")
    tenant
}
