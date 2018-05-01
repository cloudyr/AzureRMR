#' @export
AzureToken <- R6::R6Class("AzureToken", inherit=httr::Token2.0,

public=list(
    # need to do hacky init to support explicit re-authentication instead of using a refresh token
    initialize=function(endpoint, app, user_params, client_credentials, use_basic_auth)
    {
        private$az_client_credentials <- client_credentials
        private$az_use_basic_auth <- use_basic_auth

        params <- list(scope=NULL, user_params=user_params, type=NULL, use_oob=FALSE, as_header=TRUE,
                       use_basic_auth=use_basic_auth, config_init=list(), client_credentials=client_credentials)

        super$initialize(app=app, endpoint=endpoint, params=params, credentials=NULL, cache_path=FALSE)
    },

    # overrides httr::Token2.0 method
    validate=function()
    {
        if(!is.null(self$endpoint$request))
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
        self$initialize(self$endpoint, self$app, self$params$user_params,
                        private$az_client_credentials, private$az_use_basic_auth)
        NULL
    }
),

private=list(
    az_client_credentials=NULL,
    az_use_basic_auth=NULL
))


#' @export
get_azure_token=function(aad_host, tenant, app, auth_type, secret, arm_host)
{
    base_url <- file.path(aad_host, tenant, fsep="/")
    if(auth_type == "client credentials")
        auth_with_creds(base_url, app, secret, arm_host)
    else auth_with_device(base_url, app, arm_host)
}


auth_with_creds <- function(base_url, app, secret, resource)
{
    endp <- httr::oauth_endpoint(base_url=base_url, authorize="oauth2/authorize", access="oauth2/token")
    app <- httr::oauth_app("azure", key=app, secret=secret)

    AzureToken$new(endp, app, user_params=list(resource=resource), client_credentials=TRUE, use_basic_auth=FALSE)
}


auth_with_device <- function(base_url, app, resource)
{
    endp <- httr::oauth_endpoint(base_url=base_url, authorize="oauth2/authorize", access="oauth2/devicecode")
    app <- httr::oauth_app("azure", key=app)

    AzureToken$new(endp, app, user_params=list(resource=resource), client_credentials=FALSE, use_basic_auth=TRUE)
}
