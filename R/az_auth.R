### Azure Context class: authentication functionality for AAD

#' @export
az_context <- R6::R6Class("az_context",

public=list(
    host=NULL,
    tenant=NULL,
    subscriptions=NULL,

    # authenticate and get subscriptions
    initialize=function(tenant, app, auth_type=c("client credentials", "device code"), secret,
                        host="https://management.azure.com/", config_file=NULL)
    {
        if(!is.null(config_file))
        {
            conf <- jsonlite::fromJSON(config_file)
            if(!is.null(conf$tenant)) tenant <- conf$tenant
            if(!is.null(conf$app)) app <- conf$app
            if(!is.null(conf$auth_type)) auth_type <- conf$auth_type
            if(!is.null(conf$secret)) secret <- conf$secret
            if(!is.null(conf$host)) host <- conf$host
        }

        self$host <- host
        self$tenant <- tenant
        private$auth_type <- match.arg(auth_type)
        private$set_token(app, secret)
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
    get_subscription=function(subscription)
    {
        if(is.null(self$subscriptions))
            stop("No subscriptions associated with this app")
        if(is.numeric(subscription))
            subscription <- self$subscriptions[subscription][1]
        az_subscription$new(private$token, subscription)
    }
),

private=list(
    auth_type=NULL,
    token=NULL,

    # obtain access token via httr OAuth 2.0 functions
    set_token=function(app, secret)
    {
        base_url <- file.path("https://login.microsoftonline.com", self$tenant)
        private$token <- if(private$auth_type == "client credentials")
            auth_with_creds(base_url, app, secret, self$host)
        else auth_with_device(base_url, app, self$host)
        NULL
    },

    # obtain subscription IDs owned by this app
    set_subs=function()
    {
        cont <- call_azure_sm(private$token, subscription="", operation="", api_version="2016-06-01")

        df <- lapply(cont, data.frame, stringsAsFactors=FALSE)
        df <- do.call(rbind, df)

        # update subscription IDs; notify if more than one found
        if(length(df$subscriptionId) > 1)
            message("Note: more than one subscription ID for this application")
        self$subscriptions <- df$subscriptionId
        NULL
    }
))


auth_with_creds <- function(base_url, app, secret, resource)
{
    endp <- httr::oauth_endpoint(base_url=base_url, authorize="oauth2/authorize", access="oauth2/token")
    app <- httr::oauth_app("azure", key=app, secret=secret)

    httr::oauth2.0_token(endp, app, user_params=list(resource=resource), client_credentials=TRUE, cache=FALSE)
}


auth_with_device <- function(base_url, app, resource)
{
    endp <- httr::oauth_endpoint(base_url=base_url, authorize="oauth2/authorize", access="oauth2/devicecode")
    app <- httr::oauth_app("azure", key=app)

    httr::oauth2.0_token(endp, app, user_params=list(resource=resource), use_basic_auth=TRUE, cache=FALSE)
}


