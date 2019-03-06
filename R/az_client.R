az_client <- R6::R6Class("az_client",

public=list(

    tenant=NULL,
    arm=NULL,
    graph=NULL,

    initialize=function(tenant, arm_client, graph_client)
    {
        self$tenant <- tenant
        self$arm <- arm_client
        self$graph <- graph_client
    },

    # dispatcher methods
    get_subscription=function(...)
    self$arm$get_subscription(...),

    get_subscription_by_name=function(...)
    self$arm$get_subscription_by_name(...),

    list_subscriptions=function()
    self$arm$list_subscriptions(),

    create_app=function(...)
    self$graph$create_app(...),

    get_app=function(...)
    self$graph$get_app(...),

    delete_app=function(...)
    self$graph$delete_app(...),

    create_service_principal=function(...)
    self$graph$create_service_principal(...),

    get_service_principal=function(...)
    self$graph$get_service_principal(...),

    delete_service_principal=function(...)
    self$graph$delete_service_principal(...)
))
