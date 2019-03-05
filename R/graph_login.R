create_graph_login <- function(tenant="myorganization", app=.az_cli_app_id, password=NULL, username=NULL,
                               auth_type=NULL,
                               host="https://graph.windows.net/", aad_host="https://login.microsoftonline.com/",
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

    # convert graph tenant to AAD tenant
    # tenant supplied -> use that tenant for AAD and graph
    # tenant not supplied -> use "common" for AAD and "myorganization" for graph
    # scenarios not supported:
    # - AAD tenant = "foo.com", graph tenant = "myorganization"
    # - AAD tenant = "common", graph tenant = "foo.com"
    aad_tenant <- if(missing(tenant) || tenant == "myorganization")
        "common"
    else tenant

    tenant <- normalize_graph_tenant(tenant)
    aad_tenant <- normalize_tenant(aad_tenant)
    app <- normalize_guid(app)

    hash <- token_hash(
        resource=host,
        tenant=aad_tenant,
        app=app,
        password=password,
        username=username,
        auth_type=auth_type,
        aad_host=aad_host
    )
    tokenfile <- file.path(AzureR_dir(), hash)
    if(file.exists(tokenfile))
    {
        message("Deleting existing Azure Active Directory token for this set of credentials")
        file.remove(tokenfile)
    }

    message("Creating Azure Active Directory Graph login for ", format_tenant(tenant))
    client <- az_graph$new(tenant, app, password, username, auth_type, host, aad_host, config_file, ...)

    # save login info for future sessions
    graph_logins <- load_graph_logins()
    graph_logins[[tenant]] <- sort(unique(c(graph_logins[[tenant]], client$token$hash())))
    save_graph_logins(graph_logins)

    client
}


get_graph_login <- function(tenant="myorganization", selection=NULL, refresh=TRUE)
{
    if(!dir.exists(AzureR_dir()))
        stop("AzureR data directory does not exist; cannot load saved logins")

    tenant <- normalize_graph_tenant(tenant)

    arm_logins <- load_arm_logins()
    this_login <- arm_logins[[tenant]]
    if(is_empty(this_login))
    {
        msg <- paste0("No Azure Active Directory Graph logins found for ", format_tenant(tenant),
                      ";\nuse create_graph_login() to create one")
        stop(msg, call.=FALSE)
    }

    if(length(this_login) == 1)
        selection <- 1
    else if(is.null(selection))
    {
        tokens <- lapply(this_login, function(f)
            readRDS(file.path(AzureR_dir(), f)))

        choices <- sapply(tokens, function(token)
        {
            app <- token$client$client_id
            paste0("App ID: ", app, "\n   Authentication method: ", token$auth_type)
        })

        msg <- paste0("Choose an Azure Active Directory Graph login for ", format_tenant(tenant))
        selection <- utils::menu(choices, title=msg)
    }

    if(selection == 0)
        return(NULL)

    file <- if(is.numeric(selection))
        this_login[selection]
    else if(is.character(selection))
        this_login[which(this_login == selection)] # force an error if supplied hash doesn't match available logins

    file <- file.path(AzureR_dir(), file)
    if(is_empty(file) || !file.exists(file))
        stop("Azure Active Directory token not found for this login", call.=FALSE)

    message("Loading Azure Active Directory Graph login for ", format_tenant(tenant))

    token <- readRDS(file)
    client <- az_rm$new(token=token)

    if(refresh)
        client$token$refresh()

    client
}


delete_graph_login <- function(tenant="myorganization", confirm=TRUE)
{
    if(!dir.exists(AzureR_dir()))
    {
        warning("AzureR data directory does not exist; no logins to delete")
        return(invisible(NULL))
    }

    tenant <- normalize_graph_tenant(tenant)

    if(confirm && interactive())
    {
        msg <- paste0("Do you really want to delete the Azure Active Directory Graph login(s) for ",
                      format_tenant(tenant), "? (y/N) ")

        yn <- readline(msg)
        if(tolower(substr(yn, 1, 1)) != "y")
            return(invisible(NULL))
    }

    graph_logins <- load_graph_logins()
    graph_logins[[tenant]] <- NULL
    save_graph_logins(graph_logins)
    invisible(NULL)
}


list_graph_logins <- function()
{
    graph_logins <- load_graph_logins()
    logins <- sapply(graph_logins, function(tenant)
    {
        sapply(tenant, function(hash)
        {
            file <- file.path(AzureR_dir(), hash)
            az_rm$new(token=readRDS(file))
        }, simplify=FALSE)
    }, simplify=FALSE)

    logins
}


load_graph_logins <- function()
{
    file <- file.path(AzureR_dir(), "graph_logins.json")
    if(!file.exists(file))
        return(named_list())
    jsonlite::fromJSON(file)
}


save_graph_logins <- function(logins)
{
    if(!dir.exists(AzureR_dir()))
    {
        message("AzureR data directory does not exist; login credentials not saved")
        return(invisible(NULL))
    }

    if(is_empty(logins))
        names(logins) <- character(0)

    file <- file.path(AzureR_dir(), "graph_logins.json")
    writeLines(jsonlite::toJSON(logins, auto_unbox=TRUE, pretty=TRUE), file)
    invisible(NULL)
}


normalize_graph_tenant <- function(tenant)
{
    tenant <- tolower(tenant)
    if(is_guid(tenant) || tenant == "myorganization")
        return(normalize_guid(tenant))
    if(!grepl("\\.", tenant))
        stop("Graph API tenant must be a domain name, GUID or 'myorganization'")
    tenant
}
