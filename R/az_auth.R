### Azure Context class: authentication functionality for AAD
#' @include AzureToken.R

#' @export
az_context <- R6::R6Class("az_context",

public=list(
    host=NULL,
    tenant=NULL,
    subscriptions=NULL,
    auth_type=NULL,
    token=NULL,

    # authenticate and get subscriptions
    initialize=function(tenant, app, auth_type=c("client credentials", "device code"), secret,
                        host="https://management.azure.com/", aad_host="https://login.microsoftonline.com/",
                        config_file=NULL)
    {
        if(!is.null(config_file))
        {
            conf <- jsonlite::fromJSON(config_file)
            if(!is.null(conf$tenant)) tenant <- conf$tenant
            if(!is.null(conf$app)) app <- conf$app
            if(!is.null(conf$auth_type)) auth_type <- conf$auth_type
            if(!is.null(conf$secret)) secret <- conf$secret
            if(!is.null(conf$host)) host <- conf$host
            if(!is.null(conf$aad_host)) aad_host <- conf$aad_host
        }

        self$host <- host
        self$tenant <- tenant
        self$auth_type <- match.arg(auth_type)
        self$token <- get_azure_token(aad_host, tenant, app, self$auth_type, secret, host)

        private$set_subs()
        NULL
    },

    # return a subscription object
    get_subscription=function(subscription)
    {
        if(is.null(self$subscriptions))
            stop("No subscriptions associated with this app")
        if(is.numeric(subscription))
            subscription <- self$subscriptions[subscription][1]
        az_subscription$new(self$token, subscription)
    }
),

private=list(

    # obtain subscription IDs owned by this app
    set_subs=function()
    {
        cont <- call_azure_sm(self$token, subscription="", operation="")

        df <- lapply(cont$value, data.frame, stringsAsFactors=FALSE)
        df <- do.call(rbind, df)

        # update subscription IDs; notify if more than one found
        if(length(df$subscriptionId) > 1)
            message("Note: more than one subscription ID for this application")
        self$subscriptions <- df$subscriptionId
        NULL
    }
))


