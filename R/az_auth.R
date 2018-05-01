### Azure Context class: authentication functionality for AAD

#' @export
az_context <- R6::R6Class("az_context",

public=list(
    host=NULL,
    tenant_id=NULL,
    subscriptions=NULL,

    # authenticate and get subscriptions
    initialize=function(tenant_id, app_id, auth_type=c("client credentials", "device code"), secret,
                        host="https://management.azure.com/", config_file=NULL)
    {
        if(!is.null(config_file))
        {
            conf <- jsonlite::fromJSON(config_file)
            if(!is.null(conf$tenant_id)) tenant_id <- conf$tenant_id
            if(!is.null(conf$app_id)) app_id <- conf$app_id
            if(!is.null(conf$auth_type)) auth_type <- conf$auth_type
            if(!is.null(conf$secret)) secret <- conf$secret
            if(!is.null(conf$host)) host <- conf$host
            }

        self$host <- host
        self$tenant_id <- tenant_id
        private$auth_type <- match.arg(auth_type)
        private$set_token(app_id, secret)
        private$set_subs()
        NULL
    },

    # refresh OAuth 2.0 authentication
    refresh=function()
    {
        tok <- private$token
        if(is.null(tok$credentials$refresh_token))
            private$set_token(tok$app$key, tok$app$secret) # re-authenticate if no refresh token
        else private$token$refresh()
        NULL
    },

    # return a subscription object
    get_subscription=function(sub)
    {
        if(is.null(self$subscriptions))
            stop("No subscriptions associated with this app")
        if(is.numeric(sub))
            sub <- self$subscriptions[[1]]
        az_subscription$new(private$token, sub)
    }
),

private=list(
    auth_type=NULL,
    token=NULL,

    # obtain access token via httr OAuth 2.0 functions
    set_token=function(app_id, secret)
    {
        base_url <- file.path("https://login.microsoftonline.com", self$tenant_id)
        private$token <- if(private$auth_type == "client credentials")
            auth_with_creds(base_url, app_id, secret, self$host)
        else auth_with_device(base_url, app_id, self$host)
        NULL
    },

    # obtain subscription IDs owned by this app
    set_subs=function()
    {
        host <- httr::parse_url(self$host)$hostname
        access <- paste(private$token$credentials$token_type, private$token$credentials$access_token)
        .headers <- c(Host=host, Authorization=access, `Content-type`="application/json")

        url <- file.path(self$host, "subscriptions?api-version=2015-01-01")
        res <- httr::GET(url, httr::add_headers(.headers=.headers))
        httr::stop_for_status(res)
        cont <- httr::content(res, as="parsed")

        df <- lapply(cont, data.frame, stringsAsFactors=FALSE)
        df <- do.call(rbind, df)

        # update subscription IDs; notify if more than one found
        if(length(df$subscriptionId) > 1)
            message("Note: more than one subscription ID for this application")
        self$subscriptions <- df$subscriptionId
        NULL
    }
))


auth_with_creds <- function(base_url, app_id, secret, resource)
{
    endp <- httr::oauth_endpoint(base_url=base_url, authorize="oauth2/authorize", access="oauth2/token")
    app <- httr::oauth_app("azure", key=app_id, secret=secret)

    httr::oauth2.0_token(endp, app, user_params=list(resource=resource), client_credentials=TRUE, cache=FALSE)
}


auth_with_device <- function(base_url, app_id, resource)
{
    endp <- httr::oauth_endpoint(base_url=base_url, authorize="oauth2/authorize", access="oauth2/devicecode")
    app <- httr::oauth_app("azure", key=app_id)

    httr::oauth2.0_token(endp, app, user_params=list(resource=resource), use_basic_auth=TRUE, cache=FALSE)
}


