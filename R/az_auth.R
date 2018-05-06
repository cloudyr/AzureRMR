### Azure Context class: authentication functionality for AAD

#' @export
az_context <- R6::R6Class("az_context",

public=list(
    host=NULL,
    tenant=NULL,
    token=NULL,

    # authenticate and get subscriptions
    initialize=function(tenant, app, auth_type=c("client credentials", "device code"), secret,
                        host="https://management.azure.com/", aad_host="https://login.microsoftonline.com/",
                        config_file=NULL)
    {
        if(!is.null(config_file))
        {
            conf <- jsonlite::fromJSON(file(config_file))
            if(!is.null(conf$tenant)) tenant <- conf$tenant
            if(!is.null(conf$app)) app <- conf$app
            if(!is.null(conf$auth_type)) auth_type <- conf$auth_type
            if(!is.null(conf$secret)) secret <- conf$secret
            if(!is.null(conf$host)) host <- conf$host
            if(!is.null(conf$aad_host)) aad_host <- conf$aad_host
        }

        self$host <- host
        self$tenant <- tenant
        self$token <- get_azure_token(aad_host, tenant, app, match.arg(auth_type), secret, host)
        NULL
    },

    # return a subscription object
    get_subscription=function(id)
    {
        az_subscription$new(self$token, id)
    },

    # return all subscriptions for this app
    list_subscriptions=function()
    {
        cont <- call_azure_rm(self$token, subscription="", operation="")
        lst <- lapply(cont$value, function(parms) az_subscription$new(self$token, parms=parms))
        # keep going until paging is complete
        while(!is_empty(cont$nextLink))
        {
            cont <- call_azure_url(self$token, cont$nextLink)
            lst <- c(lst, lapply(cont$value, function(parms) az_subscription$new(self$token, parms=parms)))
        }
        named_list(lst, "id")
    }
))

