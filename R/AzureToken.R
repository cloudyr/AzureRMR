#' Azure OAuth authentication
#'
#' Azure OAuth 2.0 token class, inheriting from the [Token2.0 class][httr::Token2.0] in httr. Rather than calling the initialization method directly, tokens should be created via [get_azure_token()].
#'
#' @docType class
#' @section Methods:
#' - `refresh`: Refreshes the token. For expired Azure tokens using client credentials, refreshing really means requesting a new token.
#' - `validate`: Checks if the token is still valid. For Azure tokens using client credentials, this just checks if the current time is less than the token's expiry time.
#' - `hash`: Computes an MD5 hash on selected fields of the token. Used internally for identification purposes when caching.
#'
#' @seealso
#' [get_azure_token], [httr::Token]
#'
#' @format An R6 object of class `AzureToken`.
#' @export
AzureToken <- R6::R6Class("AzureToken", inherit=httr::Token2.0,

public=list(

    # need to do hacky init to support explicit re-authentication instead of using a refresh token
    initialize=function(endpoint, app, user_params, use_device=FALSE, client_credentials=TRUE)
    {
        params <- list(scope=NULL, user_params=user_params, type=NULL, use_oob=FALSE, as_header=TRUE,
                       use_basic_auth=FALSE, config_init=list(),
                       client_credentials=client_credentials, use_device=use_device)

        # if this is an existing object, don't use cached value
        # avoids infinite loop when refresh() calls initialize()
        tokenfile <- file.path(config_dir(), token_hash(endpoint, app, params))
        if(file.exists(tokenfile) && !isTRUE(private$initialized))
        {
            message("Loading cached token")
            token <- readRDS(tokenfile)
            self$app <- token$app
            self$endpoint <- token$endpoint
            self$params <- token$params
            self$cache_path <- token$cache_path
            self$private_key <- token$private_key
            self$credentials <- token$credentials
            private$initialized <- TRUE
            return(self$refresh())
        }
        private$initialized <- TRUE

        # use httr initialize for authorization_code, client_credentials methods
        if(!use_device && is.null(user_params$username))
            return(super$initialize(app=app, endpoint=endpoint, params=params, cache_path=FALSE))

        self$app <- app
        self$endpoint <- endpoint
        self$params <- params
        self$cache_path <- NULL
        self$private_key <- NULL

        # use our own init functions for device_code, resource_owner methods
        if(use_device)
            private$init_with_device(user_params)
        else private$init_with_username(user_params)

        saveRDS(self, tokenfile)
        self
    },

    # overrides httr::Token method
    hash=function()
    {
        token_hash(self$endpoint, self$app, self$params)
    },

    # overrides httr::Token method
    cache=function()
    {
        filename <- file.path(config_dir(), self$hash())
        saveRDS(self, filename)
        invisible(NULL)
    },

    # overrides httr::Token2.0 method
    can_refresh=function()
    {
        TRUE  # always can refresh
    },

    # overrides httr::Token2.0 method
    validate=function()
    {
        if(!is.null(self$endpoint$validate))
            return(super$validate())

        expdate <- as.POSIXct(as.numeric(self$credentials$expires_on), origin="1970-01-01")
        curdate <- Sys.time()
        curdate < expdate
    },

    # overrides httr::Token2.0 method
    refresh=function()
    {
        # use a refresh token if it exists
        # don't call superclass method b/c of different caching logic
        if(!is.null(self$credentials$refresh_token))
        {
            body <- list(
                refresh_token=self$credentials$refresh_token,
                client_id=self$app$key,
                client_secret=self$app$secret,
                grant_type="refresh_token"
            )
            body <- modifyList(body, self$params$user_params)

            access_uri <- sub("devicecode$", "token", self$endpoint$access)
            res <- httr::POST(access_uri, body=body, encode="form")

            if(httr::status_code(res) >= 300)
            {
                delete_azure_token(hash=self$hash(), confirm=FALSE)
                stop("Unable to refresh", call.=FALSE)
            }
            self$credentials <- utils::modifyList(self$credentials, httr::content(res))
        }
        else # re-authenticate if no refresh token
        {
            # save the hash so we can delete the cached token on failure (initialize can modify state)
            hash <- self$hash()

            res <- try(self$initialize(self$endpoint, self$app, self$params$user_params,
                    use_device=self$params$use_device,
                    client_credentials=self$params$client_credentials), silent=TRUE)
            if(inherits(res, "try-error"))
            {
                delete_azure_token(hash=hash, confirm=FALSE)
                stop("Unable to reauthenticate", call.=FALSE)
            }
        }

        self$cache()
        self
    }
),

private=list(
    initialized=NULL,

    # device code authentication: after sending initial request, loop until server indicates code has been received
    # after init_oauth2.0, oauth2.0_access_token
    init_with_device=function(user_params)
    {
        creds <- httr::oauth2.0_access_token(self$endpoint, self$app, code=NULL, user_params=user_params,
            redirect_uri=NULL)

        cat(creds$message, "\n")  # tell user to enter the code

        req_params <- list(client_id=self$app$key, grant_type="device_code", code=creds$device_code)
        req_params <- utils::modifyList(user_params, req_params)
        access_uri <- sub("devicecode$", "token", self$endpoint$access)

        message("Waiting for device code in browser...\nPress Esc/Ctrl + C to abort")
        interval <- as.numeric(creds$interval)
        ntries <- as.numeric(creds$expires_in) %/% interval
        for(i in seq_len(ntries))
        {
            Sys.sleep(interval)

            res <- httr::POST(access_uri, httr::add_headers(`Cache-Control`="no-cache"), encode="form",
                              body=req_params)

            status <- httr::status_code(res)
            cont <- httr::content(res)
            if(status == 400 && cont$error == "authorization_pending")
            {
                # do nothing
            }
            else if(status >= 300)
                httr::stop_for_status(res)
            else break
        }
        if(status >= 300)
            stop("Unable to authenticate")

        message("Authentication complete.")
        self$credentials <- cont
        NULL
    },

    # resource owner authentication: send username/password
    init_with_username=function(user_params)
    {
        body <- list(
            resource=user_params$resource,
            client_id=self$app$key,
            grant_type="password",
            username=user_params$username,
            password=user_params$password)

        res <- httr::POST(self$endpoint$access, httr::add_headers(`Cache-Control`="no-cache"), encode="form",
                          body=body)

        httr::stop_for_status(res, task="get an access token")
        self$credentials <- httr::content(res)
        NULL
    }
))


#' Manage Azure Active Directory OAuth 2.0 tokens
#'
#' These functions extend the OAuth functionality in httr for use with Azure Active Directory (AAD).
#'
#' @param resource_host URL for your resource host. For Resource Manager in the public Azure cloud, this is `https://management.azure.com/`.
#' @param tenant Your tenant. This can be a name ("myaadtenant"), a fully qualified domain name ("myaadtenant.onmicrosoft.com" or "mycompanyname.com"), or a GUID.
#' @param app The client/app ID to use to authenticate with.
#' @param password The password, either for the app, or your username if supplied. See 'Details' below.
#' @param username Your AAD username, if using the resource owner grant. See 'Details' below.
#' @param auth_type The authentication type. See 'Details' below.
#' @param aad_host URL for your AAD host. For the public Azure cloud, this is `https://login.microsoftonline.com/`. Change this if you are using a government or private cloud.
#'
#' @details
#' `get_azure_token` does much the same thing as [httr::oauth2.0_token()], but customised for Azure. It obtains an OAuth token, first by checking if a cached value exists on disk, and if not, acquiring it from the AAD server. `delete_azure_token` deletes a cached token, and `list_azure_tokens` lists currently cached tokens.
#'
#' @section Authentication methods:
#' The OAuth authentication type can be one of four possible values: "authorization_code", "client_credentials", "device_code", or "resource_owner". The first two are provided by the [httr::Token2.0] token class, while the last two are provided by the AzureToken class which extends httr::Token2.0. Here is a short description of these methods.
#'
#' 1. Using the authorization_code method is a 3-step process. First, `get_azure_token` contacts the AAD authorization endpoint to obtain a temporary access code. It then contacts the AAD access endpoint, passing it the code. The access endpoint sends back a login URL which `get_azure_token` opens in your browser, where you can enter your credentials. Once this is completed, the endpoint returns the OAuth token via a HTTP redirect URI.
#'
#' 2. The device_code method is similar in concept to authorization_code, but is meant for situations where you are unable to browse the Internet -- for example if you don't have a browser installed or your computer has input constraints. First, `get_azure_token` contacts the AAD devicecode endpoint, which responds with a login URL and an access code. You then visit the URL and enter the code, possibly using a different computer. Meanwhile, `get_azure_token` polls the AAD access endpoint for a token, which is provided once you have successfully entered the code.
#'
#' 3. The client_credentials method is much simpler than the above methods, requiring only one step. `get_azure_token` contacts the access endpoint, passing it the app secret (which you supplied in the `password` argument). Assuming the secret is valid, the endpoint then returns the OAuth token.
#'
#' 4. The resource_owner method also requires only one step. In this method, `get_azure_token` passes your (personal) username and password to the AAD access endpoint, which validates your credentials and returns the token.
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
#' @section Caching:
#' AzureRMR differs from httr in its handling of token caching in a number of ways. First, caching is based on all the inputs to `get_azure_token` as listed above. Second, it defines its own directory for cached tokens, using the rappdirs package. On recent Windows versions, this will usually be in the location `C:\\Users\\(username)\\AppData\\Local\\AzureR\\AzureRMR`. On Linux, it will be in `~/.config/AzureRMR`, and on MacOS, it will be in `~/Library/Application Support/AzureRMR`. Note that a single directory is used for all tokens, and the working directory is not touched (which eliminates the risk of accidentally introducing cached tokens into source control).
#'
#' To list all cached tokens on disk, use `list_azure_tokens`. This returns a list of token objects, named according to their MD5 hashes.
#'
#' To delete a cached token, use `delete_azure_token`. This takes the same inputs as `get_azure_token`, or you can specify the MD5 hash directly in the `hash` argument.
#'
#' @section Value:
#' For `get_azure_token`, an object of class `AzureToken` representing the AAD token. For `list_azure_tokens`, a list of such objects retrieved from disk.
#' 
#' @seealso
#' [AzureToken], [httr::oauth2.0_token], [httr::Token],
#'
#' [OAuth authentication for Azure Active Directory](https://docs.microsoft.com/en-us/azure/active-directory/develop/active-directory-protocols-oauth-code),
#' [Device code flow on OAuth.com](https://www.oauth.com/oauth2-servers/device-flow/token-request/),
#' [OAuth 2.0 RFC](https://tools.ietf.org/html/rfc6749) for the gory details on how OAuth works
#'
#' @examples
#' \dontrun{
#'
#' # authenticate with Azure Resource Manager:
#' # no user credentials are supplied, so this will use the authorization_code
#' # method if httpuv is installed, and device_code if not
#' arm_token <- get_azure_token(
#'    resource_host="https://management.azure.com/",
#'    tenant="myaadtenant.onmicrosoft.com",
#'    app="app_id")
#'
#' # you can force a specific authentication method with the auth_type argument
#' arm_token <- get_azure_token(
#'    resource_host="https://management.azure.com/",
#'    tenant="myaadtenant.onmicrosoft.com",
#'    app="app_id",
#'    auth_type="device_code")
#'
#' # to use the client_credentials method, supply the app secret as the password
#' arm_token <- get_azure_token(
#'    resource_host="https://management.azure.com/",
#'    tenant="myaadtenant.onmicrosoft.com",
#'    app="app_id",
#'    password="app_secret")
#'
#' # authenticate with Azure storage
#' storage_token <- get_azure_token(
#'    resource_host="https://storage.azure.com/",
#'    tenant="myaadtenant.onmicrosoft.com",
#'    app="app_id")
#'
#' # authenticate to your resource with the resource_owner method: provide your username and password
#' owner_token <- get_azure_token(
#'    resource_host="https://myresource/",
#'    tenant="myaadtenant",
#'    app="app_id",
#'    username="user",
#'    password="abcdefg")
#'
#' # list saved tokens
#' list_azure_tokens()
#'
#' # delete a saved token from disk
#' delete_azure_token(
#'    resource_host="https://myresource/",
#'    tenant="myaadtenant",
#'    app="app_id",
#'    username="user",
#'    password="abcdefg")
#'
#' # delete a saved token by specifying its MD5 hash
#' delete_azure_token(hash="7ea491716e5b10a77a673106f3f53bfd")
#'
#' }
#' @export
get_azure_token <- function(resource_host, tenant, app, password=NULL, username=NULL, auth_type=NULL,
                            aad_host="https://login.microsoftonline.com/")
{
    tenant <- normalize_tenant(tenant)
    if(is_guid(app))
        app <- normalize_guid(app)
    base_url <- construct_path(aad_host, tenant)

    if(is.null(auth_type))
        auth_type <- select_auth_type(password, username)

    # fail if authorization_code selected but httpuv not available
    if(auth_type == "authorization_code" && system.file(package="httpuv") == "")
        stop("httpuv package must be installed to use authorization_code method", call.=FALSE)

    switch(auth_type,
        client_credentials=
            auth_with_client_creds(base_url, app, password, resource_host),
        device_code=
            auth_with_device(base_url, app, resource_host),
        authorization_code=
            auth_with_code(base_url, app, resource_host),
        resource_owner=
            auth_with_username(base_url, app, password, username, resource_host),
        stop("Invalid auth_type argument", call.=FALSE))
}


auth_with_client_creds <- function(base_url, app, password, resource)
{
    endp <- httr::oauth_endpoint(base_url=base_url, authorize="oauth2/authorize", access="oauth2/token")
    app <- httr::oauth_app("azure", key=app, secret=password, redirect_uri=NULL)

    AzureToken$new(endp, app, user_params=list(resource=resource), use_device=FALSE, client_credentials=TRUE)
}


auth_with_device <- function(base_url, app, resource)
{
    endp <- httr::oauth_endpoint(base_url=base_url, authorize="oauth2/authorize", access="oauth2/devicecode")
    app <- httr::oauth_app("azure", key=app, secret=NULL)

    AzureToken$new(endp, app, user_params=list(resource=resource), use_device=TRUE, client_credentials=FALSE)
}


auth_with_code <- function(base_url, app, resource)
{
    endp <- httr::oauth_endpoint(base_url=base_url, authorize="oauth2/authorize", access="oauth2/token")
    app <- httr::oauth_app("azure", key=app, secret=NULL)

    AzureToken$new(endp, app, user_params=list(resource=resource), use_device=FALSE, client_credentials=FALSE)
}


auth_with_username <- function(base_url, app, password, username, resource)
{
    endp <- httr::oauth_endpoint(base_url=base_url, authorize="oauth2/authorize", access="oauth2/token")
    app <- httr::oauth_app("azure", key=app, secret=NULL)

    AzureToken$new(endp, app, user_params=list(resource=resource, username=username, password=password),
        use_device=FALSE, client_credentials=FALSE)
}


# select authentication method based on input arguments and presence of httpuv
select_auth_type <- function(password, username)
{
    got_pwd <- !is.null(password)
    got_user <- !is.null(username)

    if(got_pwd && got_user)
        "resource_owner"
    else if(!got_pwd && !got_user)
    {
        if(system.file(package="httpuv") == "")
        {
            message("httpuv not installed, defaulting to device code authentication")
            "device_code"
        }
        else "authorization_code"
    }
    else if(got_pwd && !got_user)
        "client_credentials"
    else stop("Can't select authentication method", call.=FALSE)
}


#' @param hash The MD5 hash of this token, computed from the above inputs. Used by `delete_azure_token` to identify a cached token to delete.
#' @param confirm For `delete_azure_token`, whether to prompt for confirmation before deleting a token.
#' @rdname get_azure_token
#' @export
delete_azure_token <- function(resource_host, tenant, app, password=NULL, username=NULL, auth_type=NULL,
                               aad_host="https://login.microsoftonline.com/",
                               hash=NULL,
                               confirm=TRUE)
{
    if(is.null(hash))
    {
        # reconstruct the hash for the token object from the inputs
        tenant <- normalize_tenant(tenant)
        if(is_guid(app))
            app <- normalize_guid(app)
        base_url <- construct_path(aad_host, tenant)

        if(is.null(auth_type))
            auth_type <- select_auth_type(password, username)

        base_url <- construct_path(aad_host, tenant)
        use_device <- auth_type == "device_code"
        client_credentials <- auth_type == "client_credentials"

        endp <- httr::oauth_endpoint(base_url=base_url,
            authorize="oauth2/authorize",
            access=if(use_device) "oauth2/devicecode" else NULL)
        app <- httr::oauth_app("azure", app,
            secret=if(client_credentials) password else NULL,
            redirect_uri=if(client_credentials) NULL else httr::oauth_callback())

        user_params <- list(resource=resource_host)
        if(auth_type == "resource_owner")
            user_params <- c(user_params, password=NULL, username=NULL)

        params <- list(scope=NULL, user_params=user_params, type=NULL, use_oob=FALSE, as_header=TRUE,
                       use_basic_auth=FALSE, config_init=list(),
                       client_credentials=client_credentials, use_device=use_device)

        hash <- token_hash(endp, app, params)
    }

    if(confirm && interactive())
    {
        yn <- readline(
            paste0("Do you really want to delete this Azure Active Directory token? (y/N) "))
        if(tolower(substr(yn, 1, 1)) != "y")
            return(invisible(NULL))
    }
    file.remove(file.path(config_dir(), hash))
    invisible(NULL)
}


#' @rdname get_azure_token
#' @export
list_azure_tokens <- function()
{
    tokens <- dir(config_dir(), full.names=TRUE)
    lst <- lapply(tokens, function(fname)
    {
        x <- readRDS(fname)
        if(is_azure_token(x))
            x
        else NULL
        })
    names(lst) <- basename(tokens)
    lst[!sapply(lst, is.null)]
}


token_hash <- function(endpoint, app, params)
{
    msg <- serialize(list(endpoint, app, params), NULL, version=2)
    paste(openssl::md5(msg[-(1:14)]), collapse="")
}

