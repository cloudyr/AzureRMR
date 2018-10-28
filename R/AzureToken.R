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
    initialize=function(endpoint, app, user_params, use_device=FALSE)
    {
        private$az_use_device <- use_device

        params <- list(scope=NULL, user_params=user_params, type=NULL, use_oob=FALSE, as_header=TRUE,
                       use_basic_auth=use_device, config_init=list(), client_credentials=TRUE)

        super$initialize(app=app, endpoint=endpoint, params=params, credentials=NULL, cache_path=FALSE)

        # if auth is via device, token now contains initial server response; call devicecode handler to get actual token
        if(use_device)
            private$init_with_device(endpoint, app, user_params)
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
        self$initialize(self$endpoint, self$app, self$params$user_params, use_device=private$az_use_device)
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
        endpoint$access <- sub("devicecode", "token", endpoint$access)

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
    }
))


#' Generate an Azure OAuth token
#'
#' This extends the OAuth functionality in httr to allow for device code authentication.
#'
#' @param aad_host URL for your Azure Active Directory host. For the public Azure cloud, this is `https://login.microsoftonline.com/`.
#' @param tenant Your tenant ID.
#' @param app Your client/app ID which you registered in AAD.
#' @param auth_type The authentication type, either `"client_credentials"` or `"device_code"`.
#' @param password Your password. Required for `auth_type == "client_credentials"`, ignored for `auth_type == "device_code"`.
#' @param arm_host URL for your Azure Resource Manager host. For the public Azure cloud, this is `https://management.azure.com/`.
#'
#' @details
#' This function does much the same thing as [httr::oauth2.0_token()], but with support for device authentication and with unnecessary options removed. Device authentication removes the need to save a password on your machine. Instead, the server provides you with a code, along with a URL. You then visit the URL in your browser and enter the code, which completes the authentication process.
#' 
#' @seealso
#' [AzureToken], [httr::oauth2.0_token], [httr::Token],
#' [OAuth authentication for Azure Active Directory](https://docs.microsoft.com/en-us/azure/active-directory/develop/active-directory-protocols-oauth-code),
#' [Device code flow on OAuth.com](https://www.oauth.com/oauth2-servers/device-flow/token-request/)
#' @export
get_azure_token=function(aad_host, tenant, app, auth_type=c("client_credentials", "device_code"), password, arm_host)
{
    auth_type <- match.arg(auth_type)
    base_url <- construct_path(aad_host, tenant)
    if(auth_type == "client_credentials")
        auth_with_creds(base_url, app, password, arm_host)
    else auth_with_device(base_url, app, arm_host)
}


auth_with_creds <- function(base_url, app, password, resource)
{
    endp <- httr::oauth_endpoint(base_url=base_url, authorize="oauth2/authorize", access="oauth2/token")
    app <- httr::oauth_app("azure", key=app, password=password)

    AzureToken$new(endp, app, user_params=list(resource=resource))
}


auth_with_device <- function(base_url, app, resource)
{
    endp <- httr::oauth_endpoint(base_url=base_url, authorize="oauth2/authorize", access="oauth2/devicecode")
    app <- httr::oauth_app("azure", key=app, password=NULL)

    AzureToken$new(endp, app, user_params=list(resource=resource), use_device=TRUE)
}


is_azure_token <- function(object)
{
    R6::is.R6(object) && inherits(object, "AzureToken")
}
