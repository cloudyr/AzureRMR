# app id used by Azure CLI
.az_cli_app_id <- "04b07795-8ddb-461a-bbee-02f9e1bf7b46"

# TODO: use rappdirs package
config_dir <- function()
{
    "~/.AzureRMR"
}


# environment to store ARM client objects: will be mirrored on disk (approximately)
logins <- new.env()


#' @export
create_az_login <- function(tenant)
{
    tenant <- normalize_tenant(tenant)

    message("Creating Resource Manager client for tenant", tenant)
    client <- az_rm$new(tenant, app=.az_cli_app_id, auth_type="device_code")

    logins[[tenant]] <- client
    save_client(client, tenant)

    client
}


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


#' @export
delete_az_login <- function(tenant)
{
    tenant <- normalize_tenant(tenant)

    # remove fron environment and from config dir
    rm(list=tenant, envir=logins)
    file.remove(file.path(config_dir(), tenant, ".RDS"))
}


#' @export
list_az_logins <- function()
{
    as.list(logins)
}


#' @export
renew_az_logins <- function()
{
    renew_and_save <- function(client_name)
    {
        client <- logins[[client_name]]
        client$token$refresh()
        save_client(client, client_name)
    }

    lapply(ls(logins), renew_and_save)
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
