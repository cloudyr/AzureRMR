#' Azure OAuth authentication
#'
#' Azure OAuth 2.0 token class, inheriting from the [Token2.0 class][httr::Token2.0] in httr. Rather than calling the initialization method directly, tokens should be created via [get_azure_token()].
#'
#' @docType class
#' @section Methods:
#' - `refresh`: Refreshes the token. For expired Azure tokens using client credentials, refreshing really means requesting a new token.
#' - `validate`: Checks if the token is still valid. For Azure tokens using client credentials, this just checks if the current time is less than the token's expiry time.
#'
#' @section Caching:
#' This class never caches its tokens, unlike httr::Token2.0.
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
        private$az_use_device <- use_device

        params <- list(scope=NULL, user_params=user_params, type=NULL, use_oob=FALSE, as_header=TRUE,
                       use_basic_auth=use_device, config_init=list(), client_credentials=client_credentials)

        if(is.null(user_params$username))
            super$initialize(app=app, endpoint=endpoint, params=params, credentials=NULL, cache_path=FALSE)
        else
        {
            self$app <- app
            self$endpoint <- endpoint
            self$params <- params
            self$cache_path <- NULL
            self$private_key <- NULL
            # handle username/password authentication
            private$init_with_username(app=app, endpoint=endpoint, user_params)
        }

        # if auth is via device, token now contains initial server response; call devicecode handler to get actual token
        if(use_device)
            private$init_with_device(endpoint, app, user_params)

        # ensure password is never NULL (important for renewing)
        if(is_empty(self$app$secret))
            self$app$secret <- ""
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
        if(!is.null(self$credentials$refresh_token))
            return(super$refresh())

        # re-authenticate if no refresh token
        self$initialize(self$endpoint, self$app, self$params$user_params, use_device=private$az_use_device,
            client_credentials=self$params$client_credentials)
        NULL
    }
),

private=list(
    az_use_device=NULL,

    # device code authentication: after sending initial request, loop until server indicates code has been received
    # after init_oauth2.0, oauth2.0_access_token
    init_with_device=function(endpoint, app, user_params)
    {
        cat(self$credentials$message, "\n")  # tell user to enter the code

        req_params <- list(client_id=app$key, grant_type="device_code", code=self$credentials$device_code)
        req_params <- utils::modifyList(user_params, req_params)
        endpoint$access <- sub("devicecode$", "token", endpoint$access)

        interval <- as.numeric(self$credentials$interval)
        ntries <- as.numeric(self$credentials$expires_in) %/% interval
        for(i in seq_len(ntries))
        {
            Sys.sleep(interval)

            res <- httr::POST(endpoint$access, httr::add_headers(`Cache-Control`="no-cache"), encode="form",
                              body=req_params)

            status <- httr::status_code(res)
            cont <- httr::content(res)
            if(status == 400 && cont$error == "authorization_pending")
            {
                msg <- sub("[\r\n].*", "", cont$error_description)
                cat(msg, "\n")
            }
            else if(status >= 300)
                httr::stop_for_status(res)
            else break
        }
        if(status >= 300)
            stop("Unable to authenticate")

        # replace original fields with authenticated fields
        self$endpoint <- endpoint
        self$credentials <- cont
        NULL
    },

    # resource owner authentication: send username/password
    init_with_username=function(endpoint, app, user_params)
    {
        body <- list(
            client_id=app$key,
            grant_type="password",
            username=user_params$username,
            password=user_params$password)

        res <- httr::POST(endpoint$access, httr::add_headers(`Cache-Control`="no-cache"), encode="form",
                          body=body)

        httr::stop_for_status(res, task="get an access token")
        httr::content(res)
    }
))


#' Generate an Azure OAuth token
#'
#' This extends the OAuth functionality in httr to allow for device code authentication.
#'
#' @param resource_host URL for your resource host. For Resource Manager in the public Azure cloud, this is `https://management.azure.com/`.
#' @param tenant Your tenant ID.
#' @param app The client/app ID to use to authenticate with Azure Active Directory (AAD).
#' @param password The password, either for the app, or your username if supplied. See 'Details' below.
#' @param username Your AAD username, if using the resource owner grant. See 'Details' below.
#' @param auth_type The authentication type. See 'Details' below.
#' @param aad_host URL for your AAD host. For the public Azure cloud, this is `https://login.microsoftonline.com/`.
#'
#' @details
#' This function does much the same thing as [httr::oauth2.0_token()], but with support for device authentication and with unnecessary options removed. Device authentication removes the need to save a password on your machine. Instead, the server provides you with a code, along with a URL. You then visit the URL in your browser and enter the code, which completes the authentication process.
#'
#' The OAuth authentication type can be one of 4 possible values: "authorization_code", "device_code", "client_credentials" or "resource_owner". If this is not specified, the value is chosen based on the presence or absence of the `password` and `username` arguments:
#'
#' - Password and username present: "resource_owner"
#' - Password and username absent: "authorization_code" if the httpuv package is installed, "device_code" otherwise
#' - Password present, username absent: "client_credentials"
#' - Password absent, username present: error
#'
#' The httpuv package must be installed to use the "authorization_code" method, as this requires a web server to listen on the (local) redirect URI. See [httr::oauth2.0_token] for more information; note that Azure does not support the `use_oob` feature of the httr OAuth 2.0 token class.
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
#' arm_token <- get_azure_token(
#'    resource_host="https://management.azure.com/",  # authenticate with Azure Resource Manager
#'    tenant="myaadtenant.onmicrosoft.com",
#'    app="app_id")
#'
#' storage_token <- get_azure_token(
#'    resource_host="https://storage.azure.com/",  # authenticate with Azure storage
#'    tenant="myaadtenant.onmicrosoft.com",
#'    app="app_id",
#'    password="password")
#'
#' }
#' @export
get_azure_token <- function(resource_host, tenant, app, password=NULL, username=NULL, auth_type=NULL,
                            aad_host="https://login.microsoftonline.com/")
{
    tenant <- normalize_tenant(tenant)
    base_url <- construct_path(aad_host, tenant)

    if(is.null(auth_type))
        auth_type <- select_auth_type(password, username)

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
    app <- httr::oauth_app("azure", key=app, secret=password)

    AzureToken$new(endp, app, user_params=list(resource=resource), use_device=FALSE, client_credentials=TRUE)
}


auth_with_device <- function(base_url, app, resource)
{
    endp <- httr::oauth_endpoint(base_url=base_url, authorize="oauth2/authorize", access="oauth2/devicecode")
    app <- httr::oauth_app("azure", key=app, secret="")

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
        use_device=TRUE, client_credentials=FALSE)
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
            message("httpuv package not found, defaulting to device code authentication")
            "device_code"
        }
        else "authorization_code"
    }
    else if(got_pwd && !got_user)
        "client_credentials"
    else stop("Can't select authentication method", call.=FALSE)
}
