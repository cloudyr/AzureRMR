#' Login to Azure Resource Manager and Active Directory Graph

#' @param tenant The Azure Active Directory tenant for which to obtain a login client. Can be a name ("myaadtenant"), a fully qualified domain name ("myaadtenant.onmicrosoft.com" or "mycompanyname.com"), or a GUID. The default is to login via the "common" tenant, which will infer your actual tenant from your credentials.
#' @param app The client/app ID to use to authenticate with Azure Active Directory. The default is to login interactively using the Azure CLI cross-platform app, but you can supply your own app credentials as well.
#' @param password If `auth_type == "client_credentials"`, the app secret; if `auth_type == "resource_owner"`, your account password.
#' @param username If `auth_type == "resource_owner"`, your username.
#' @param auth_type The OAuth authentication method to use, one of "client_credentials", "authorization_code", "device_code" or "resource_owner". If `NULL`, this is chosen based on the presence of the `username` and `password` arguments.
#' @param arm_host Your Azure Resource Manager host. Defaults to `https://management.azure.com/`. Change this if you are using a government or private cloud.
#' @param graph_host Your Azure Active Directory Graph host. Defaults to `https://graph.windows.net/`. Change this if you are using a government or private cloud.
#' @param aad_host Azure Active Directory host for authentication. Defaults to `https://login.microsoftonline.com/`. Change this if you are using a government or private cloud.
#' @param config_file Optionally, a JSON file containing any of the arguments listed above. Arguments supplied in this file take priority over those supplied on the command line. You can also use the output from the Azure CLI `az ad sp create-for-rbac` command.
#' @param refresh For `get_azure_login`, whether to refresh the authentication tokens on loading the client.
#' @param selection For `get_azure_login`, if you have multiple logins for a given tenant, which one to use. This can be a number, or the ID of the AAD app used to authenticate. If not supplied, `get_azure_login` will print a menu and ask you to choose a login.
#' @param confirm For `delete_azure_login`, whether to ask for confirmation before deleting.
#' @param ... Other arguments passed to `az_rm$new()` and `az_graph$new()`.
#'
#' @details
#' `create_azure_login` creates a login client to authenticate with Azure Resource Manager (ARM) and Azure Active Directory (AAD) Graph, using the supplied arguments. The authentication tokens are obtained using [get_azure_token], which automatically caches and reuses tokens for subsequent sessions. Credentials are only cached if you allowed AzureRMR to create a data directory at package startup.
#'
#' `create_azure_login()` without any arguments is roughly equivalent to the Azure CLI command `az login`. Note that if you are doing an interactive login, you will see _two_ authentication screens, as a separate token needs to be obtained for ARM and for AAD Graph.
#'
#' `get_azure_login` returns a login client by retrieving previously saved credentials. It searches for saved credentials according to the supplied tenant; if multiple logins are found, it will prompt for you to choose one.
#'
#' One difference between `create_azure_login` and `get_azure_login` is the former will delete any previously saved credentials that match the arguments it was given. You can use this to force AzureRMR to remove obsolete tokens that may be lying around.
#'
#' @section Linux DSVM note:
#' If you are using a Linux [Data Science Virtual Machine](https://azure.microsoft.com/en-us/services/virtual-machines/data-science-virtual-machines/) in Azure, you may have problems running `create_azure_login()` (ie, without any arguments). In this case, try `create_azure_login(auth_type="device_code")`.
#'
#' @return
#' For `get_azure_login` and `create_azure_login`, an object of class `az_client`, representing the login client. This encapsulates functionality for interacting with both ARM and AAD Graph. For `list_azure_logins`, a (possibly nested) list of such objects.
#'
#' If the AzureR data directory for saving credentials does not exist, `get_azure_login` will throw an error.
#'
#' @seealso
#' [az_rm], [az_graph], [AzureAuth::get_azure_token] for more details on authentication methods
#'
#' [Azure Resource Manager overview](https://docs.microsoft.com/en-us/azure/azure-resource-manager/resource-group-overview),
#' [REST API reference](https://docs.microsoft.com/en-us/rest/api/resources/)
#'
#' [Azure AD Graph API](https://docs.microsoft.com/en-au/azure/active-directory/develop/active-directory-graph-api)
#'
#' [Authentication in Azure Active Directory](https://docs.microsoft.com/en-us/azure/active-directory/develop/authentication-scenarios)
#'
#' [Azure CLI documentation](https://docs.microsoft.com/en-us/cli/azure/?view=azure-cli-latest)
#' 
#' @examples
#' \dontrun{
#'
#' # without any arguments, this will create a client using your AAD credentials
#' az <- create_azure_login() 
#'
#' # retrieve the login in subsequent sessions
#' az <- get_azure_login()
#'
#' # this will create a Resource Manager client for the AAD tenant 'microsoft.onmicrosoft.com',
#' # using the client_credentials method
#' az <- create_azure_login("microsoft", app="{app_id}", password="{password}")
#'
#' # you can also login using credentials in a json file
#' az <- create_azure_login(config_file="~/creds.json")
#'
#' }
#' @rdname azure_login
#' @export
create_azure_login <- function(tenant="common", app=.az_cli_app_id, password=NULL, username=NULL, auth_type=NULL,
                               arm_host="https://management.azure.com/", graph_host="https://graph.windows.net/",
                               aad_host="https://login.microsoftonline.com/",
                               config_file=NULL, ...)
{
    if(!is.null(config_file))
    {
        conf <- jsonlite::fromJSON(config_file)
        if(!is.null(conf$tenant)) tenant <- conf$tenant
        if(!is.null(conf$app)) app <- conf$app
        if(!is.null(conf$auth_type)) auth_type <- conf$auth_type
        if(!is.null(conf$password)) password <- conf$password
        if(!is.null(conf$arm_host)) arm_host <- conf$arm_host
        if(!is.null(conf$graph_host)) graph_host <- conf$graph_host
        if(!is.null(conf$aad_host)) aad_host <- conf$aad_host
    }

    tenant <- normalize_tenant(tenant)
    app <- normalize_guid(app)

    arm_hash <- token_hash(
        resource=arm_host,
        tenant=tenant,
        app=app,
        password=password,
        username=username,
        auth_type=auth_type,
        aad_host=aad_host
    )
    graph_hash <- token_hash(
        resource=graph_host,
        tenant=tenant,
        app=app,
        password=password,
        username=username,
        auth_type=auth_type,
        aad_host=aad_host
    )

    files <- file.path(AzureR_dir(), c(arm_hash, graph_hash))
    if(any(file.exists(files)))
    {
        message("Deleting existing tokens for this set of credentials")
        file.remove(files)
    }

    message("Creating Azure Resource Manager client for ", format_tenant(tenant))
    arm <- az_rm$new(tenant, app, password, username, auth_type, arm_host, aad_host, config_file, ...)

    message("Creating Azure Active Directory Graph client for ", format_tenant(tenant))
    graph <- az_graph$new(tenant, app, password, username, auth_type, graph_host, aad_host, config_file, ...)

    client <- az_client$new(tenant, arm, graph)

    # save login info for future sessions
    add_login_app(tenant, client)

    client
}


#' @rdname azure_login
#' @export
get_azure_login <- function(tenant="common", selection=NULL, refresh=FALSE)
{
    if(!dir.exists(AzureR_dir()))
        stop("AzureR data directory does not exist; cannot load saved logins")

    tenant <- normalize_tenant(tenant)

    logins <- load_logins()
    this_login <- logins[[tenant]]
    if(is_empty(this_login))
    {
        msg <- paste0("No AzureRMR logins found for ", format_tenant(tenant),
                      ";\nuse create_azure_login() to create one")
        stop(msg, call.=FALSE)
    }

    if(length(this_login) == 1 && is.null(selection))
        selection <- 1
    else if(is.null(selection))
    {
        tokens <- lapply(this_login, function(login)
            readRDS(file.path(AzureR_dir(), login[1])))

        choices <- sapply(tokens, function(token)
        {
            app <- token$client$client_id
            paste0("App ID: ", app, "\n   Authentication method: ", token$auth_type)
        })

        msg <- paste0("Choose an AzureRMR login for ", format_tenant(tenant))
        selection <- utils::menu(choices, title=msg)
    }

    if(selection == 0)
        return(NULL)

    files <- file.path(AzureR_dir(), this_login[[selection]])
    if(is_empty(files) || !all(file.exists(files)))
        stop("Authentication tokens not found for this login", call.=FALSE)

    message("Loading AzureRMR login for ", format_tenant(tenant))
    client <- az_client$new(tenant, az_rm$new(token=readRDS(files[1])), az_graph$new(token=readRDS(files[2])))

    if(refresh)
        client$refresh()

    client
}


#' @rdname azure_login
#' @export
delete_azure_login <- function(tenant="common", confirm=TRUE)
{
    if(!dir.exists(AzureR_dir()))
    {
        warning("AzureR data directory does not exist; no logins to delete")
        return(invisible(NULL))
    }

    tenant <- normalize_tenant(tenant)

    if(confirm && interactive())
    {
        msg <- paste0("Do you really want to delete the AzureRMR login(s) for ", format_tenant(tenant), "? (y/N) ")
        yn <- readline(msg)
        if(tolower(substr(yn, 1, 1)) != "y")
            return(invisible(NULL))
    }

    logins <- load_logins()
    logins[[tenant]] <- NULL
    save_logins(logins)
    invisible(NULL)
}


#' @rdname azure_login
#' @export
list_azure_logins <- function()
{
    logins <- load_logins()
    logins <- sapply(logins, function(tenant)
    {
        sapply(tenant, function(app)
        {
            files <- file.path(AzureR_dir(), app)
            arm <- az_rm$new(token=readRDS(files[1]))
            graph <- az_graph$new(token=readRDS(files[2]))
            az_client$new(arm$tenant, arm, graph)
        }, simplify=FALSE)
    }, simplify=FALSE)

    logins
}


add_login_app <- function(tenant, client)
{
    logins <- load_logins()
    tenant_logins <- logins[[tenant]]

    newapp <- client$arm$token$client$client_id
    newhashes <- c(client$arm$token$hash(), client$graph$token$hash())

    if(newapp %in% names(tenant_logins))
        tenant_logins[[newapp]] <- newhashes
    else
    {
        newlogin <- structure(list(newhashes), names=newapp)
        tenant_logins <- c(tenant_logins, newlogin)
        tenant_logins <- tenant_logins[order(names(tenant_logins))]
    }

    logins[[tenant]] <- tenant_logins
    save_logins(logins)
}


load_logins <- function()
{
    file <- file.path(AzureR_dir(), "logins.json")
    if(!file.exists(file))
        return(named_list())
    jsonlite::fromJSON(file)
}


save_logins <- function(logins)
{
    if(!dir.exists(AzureR_dir()))
    {
        message("AzureR data directory does not exist; login credentials not saved")
        return(invisible(NULL))
    }

    if(is_empty(logins))
        names(logins) <- character(0)

    file <- file.path(AzureR_dir(), "logins.json")
    writeLines(jsonlite::toJSON(logins, auto_unbox=TRUE, pretty=TRUE), file)
    invisible(NULL)
}


format_tenant <- function(tenant)
{
    if(tenant %in% c("default", "common", "myorganization"))
        "default tenant"
    else paste0("tenant '", tenant, "'")
}
