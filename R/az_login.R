config_dir <- function()
{
    rappdirs::user_config_dir(appname="AzureRMR", appauthor="AzureR", roaming=FALSE)
}


#' Functions to login to Azure Resource Manager
#'
#' @param tenant The Azure Active Directory tenant for which to obtain a login client. Can be a name ("myaadtenant"), a fully qualified domain name ("myaadtenant.onmicrosoft.com" or "mycompanyname.com"), or a GUID.
#' @param app The app ID to authenticate with.
#' @param password If `auth_type == "client_credentials"`, your password.
#' @param auth_type The type of authentication to use, either "device_code" or "client_credentials". Defaults to the latter if no password is provided, otherwise the former.
#' @param host Your ARM host. Defaults to `https://management.azure.com/`. Change this if you are using a government or private cloud.
#' @param aad_host Azure Active Directory host for authentication. Defaults to `https://login.microsoftonline.com/`. Change this if you are using a government or private cloud.
#' @param config_file Optionally, a JSON file containing any of the arguments listed above. Arguments supplied in this file take priority over those supplied on the command line. You can also use the output from the Azure CLI `az ad sp create-for-rbac` command.
#' @param refresh For `get_azure_login`, whether to refresh the authentication token on loading the client.
#' @param confirm For `delete_azure_login`, whether to ask for confirmation before deleting.
#' @param ... Other arguments passed to `az_rm$new()`.
#'
#' @details
#' These functions allow you to authenticate with Azure Resource Manager (ARM).
#' - `create_azure_login` creates a login client, using the supplied credentials. You only have to create a login client once per tenant; the resulting object is saved on your machine and reused automatically in subsequent R sessions.
#' - `get_azure_login` will load a previously saved ARM client object for the given tenant. If this is the first time you are logging in for this tenant, the client object is created via `create_login_client`.
#' - `delete_azure_login` deletes the client object for the given tenant from your machine. Note that this doesn't invalidate any client you may be using in your R session.
#' - `list_azure_logins` lists client objects that have been previously saved.
#' - `refresh_azure_logins` refreshes all client objects existing on your machine.
#'
#' `create_azure_login` is roughly equivalent to the Azure CLI command `az login` with no arguments.
#'
#' @return
#' For `create_azure_login` and `get_azure_login`, an object of class `az_rm`, representing the ARM login client. For `list_azure_logins`, a list of such objects.
#'
#' @seealso
#' [az_rm], [Azure CLI documentation](https://docs.microsoft.com/en-us/cli/azure/?view=azure-cli-latest)
#' 
#' @examples
#' \dontrun{
#'
#' # this will create a Resource Manager client for the AAD tenant 'microsoft.onmicrosoft.com'
#' # only has to be run once per tenant
#' az <- create_azure_login("microsoft", app="{app_id}", password="{password}")
#'
#' # you can also login using credentials in a json file
# az <- create_azure_login(config_file="~/creds.json")
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
create_azure_login <- function(tenant, app, password=NULL,
                               auth_type=if(is.null(password)) "device_code" else "client_credentials",
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

    tenant <- normalize_tenant(tenant)
    message("Creating Azure Active Directory login for tenant '", tenant, "'")
    client <- az_rm$new(tenant, app, password, auth_type, host, aad_host, config_file, ...)
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

    message("Loading Azure Active Directory login for tenant '", tenant, "'")
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
            paste0("Do you really want to delete the Azure Active Directory login for tenant ", tenant, "? (y/N) "))
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
