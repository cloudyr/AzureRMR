# app id used by Azure CLI
.az_cli_app_id <- "04b07795-8ddb-461a-bbee-02f9e1bf7b46"

config_dir <- function()
{
    rappdirs::user_config_dir(appname="AzureRMR", appauthor="AzureR", roaming=FALSE)
}


# environment to store ARM client objects: will be mirrored on disk (approximately)
logins <- new.env()

#' Functions to login to Azure Resource Manager
#'
#' @param tenant The Azure Active Directory tenant for which to obtain a login client. Can be a name ("mytenant"), a fully qualified domain name ("mytenant.microsoft.com"), or a GUID.
#' @param refresh For `get_az_login`, whether to refresh the authentication token on loading the client.
#' @param confirm For `delete_az_login`, whether to ask for confirmation before deleting.
#'
#' @details
#' These functions allow you to login to Azure Resource Manager (ARM).
#' - `create_az_login` will create a login client. R will display a code and prompt you to visit the Microsoft login page in your browser. You then enter the code along with your Azure Active Directory credentials, which completes the authentication process. You only have to create a login client once per tenant: the resulting ARM client object is saved on your machine and reused automatically in subsequent R sessions.
#' - `get_az_login` will load a previously saved ARM client object for the given tenant. If this is the first time you are logging in for this tenant, the client object is created.
#' - `delete_az_login` deletes the client object for the given tenant from your machine. Note that this doesn't invalidate any existing client you may have in your R session.
#' - `list_az_logins` lists client objects that have been previously saved.
#' - `refresh_az_logins` refreshes all client objects existing on your machine.
#'
#' `create_az_login` is roughly equivalent to the Azure CLI command `az login` with no arguments, and in fact uses the same app ID as the CLI.
#'
#' @return
#' For `create_az_login` and `get_az_login`, an object of class `az_rm`, representing the ARM client. For `list_az_logins`, a list of such objects.
#'
#' @seealso
#' [az_rm], [Azure CLI documentation](https://docs.microsoft.com/en-us/cli/azure/?view=azure-cli-latest)
#' 
#' @examples
#' \dontrun{
#'
#' # this will create a Resource Manager client for the AAD tenant 'microsoft.onmicrosoft.com'
#' # only has to be run once per tenant
#' az <- create_az_login("microsoft")
#'
#' # in subsequent sessions, you can retrieve the client without re-authenticating
#' # authentication details will automatically be refreshed
#' az <- get_az_login("microsoft")
#'
#' # refresh (renew) authentication details for clients for all tenants
#' refresh_az_logins()
#'
#' }
#' @rdname az_login
#' @export
create_az_login <- function(tenant)
{
    tenant <- normalize_tenant(tenant)

    message("Creating Resource Manager client for tenant ", tenant)
    client <- az_rm$new(tenant, app=.az_cli_app_id, auth_type="device_code")

    logins[[tenant]] <- client
    save_client(client, tenant)

    client
}


#' @rdname az_login
#' @export
get_az_login <- function(tenant, refresh=TRUE)
{
    tenant <- normalize_tenant(tenant)

    login_exists <- exists(tenant, logins) && inherits(logins[[tenant]], "az_rm")
    if(!login_exists)
        return(create_az_login(tenant))

    client <- logins[[tenant]]
    if(refresh)
    {
        # refresh and save
        client$token$refresh()
        save_client(client, tenant)
    }
    client
}


#' @rdname az_login
#' @export
delete_az_login <- function(tenant, confirm=TRUE)
{
    tenant <- normalize_tenant(tenant)

    if(confirm && interactive())
    {
        yn <- readline(paste0("Do you really want to delete ARM login for tenant ", tenant, "? (y/N) "))
        if(tolower(substr(yn, 1, 1)) != "y")
            return(invisible(NULL))
    }

    # remove fron environment and from config dir
    file.remove(file.path(config_dir(), tenant, ".RDS"))
    rm(list=tenant, envir=logins)
}


#' @rdname az_login
#' @export
list_az_logins <- function()
{
    as.list(logins)
}


#' @rdname az_login
#' @export
refresh_az_logins <- function()
{
    refresh_and_save <- function(client_name)
    {
        client <- logins[[client_name]]
        client$token$refresh()
        save_client(client, client_name)
    }

    lapply(ls(logins), refresh_and_save)
    invisible(NULL)
}


save_client <- function(client, tenant)
{
    tenant <- normalize_tenant(tenant)

    logins[[tenant]] <- client
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
