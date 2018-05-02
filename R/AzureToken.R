#' @export
AzureToken <- R6::R6Class("AzureToken", inherit=httr::Token2.0,

public=list(

    # need to do hacky init to support explicit re-authentication instead of using a refresh token
    initialize=function(endpoint, app, user_params, use_device=FALSE)
    {
        private$az_use_device <- use_device

        params <- list(scope=NULL, user_params=user_params, type=NULL, use_oob=FALSE, as_header=TRUE,
                       use_basic_auth=FALSE, config_init=list(), client_credentials=TRUE)

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
        req_params <- list(client_id=app$key, grant_type="device_code", code=self$credentials$device_code)
        req_params <- utils::modifyList(user_params, req_params)

        endpoint$access <- sub("devicecode$", "token", endpoint$access)
        interval <- as.numeric(self$credentials$interval)
        for(i in 1:100)
        {
            Sys.sleep(interval)

            res <- POST(endpoint$access, encode="form", body=req_params, config=list())

            status <- httr::status_code(res)
            if(status == 400 && content(res)$error == "authorization_pending")
            {
                msg <- sub("[\r\n].*", "", content(res)$error_description)
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
        self$credentials <- content(res, as="parsed")
        NULL
    }
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

    AzureToken$new(endp, app, user_params=list(resource=resource))
}


auth_with_device <- function(base_url, app, resource)
{
    endp <- httr::oauth_endpoint(base_url=base_url, authorize="oauth2/authorize", access="oauth2/devicecode")
    app <- httr::oauth_app("azure", key=app)

    AzureToken$new(endp, app, user_params=list(resource=resource), use_device=TRUE)
}
