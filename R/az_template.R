#' @export
az_template <- R6::R6Class("az_template",

public=list(
    subscription=NULL,
    resource_group=NULL,
    id=NULL,
    name=NULL,
    properties=NULL,
    token=NULL,

    # constructor: get an existing template, or deploy new template
    initialize=function(token, subscription, resource_group, name=NULL, ..., parms=list(...), create=FALSE)
    {
        if(is_empty(name) && is_empty(parms))
            stop("Must supply either template name, or parameter list")

        self$token <- token
        self$subscription <- subscription
        self$resource_group <- resource_group

        parms <- if(create)
            private$init_and_create(name, parms)
        else private$init(name, parms)

        self$id <- parms$id
        self$properties <- parms$properties
        NULL
    },

    delete=function(free_resources=FALSE) { },
    check=function() { }
),

private=list(
    is_valid=NULL,

    init=function(name, parms)
    {
        if(is_empty(parms))
        {
            self$name <- name
            parms <- private$tpl_op()
        }
        else
        {
            private$validate_parms(parms)
            self$name <- parms$name
        }
        parms
    },

    init_and_create=function(name, parms) { },

    validate_parms=function(parms)
    {
        required_names <- c("name")
        optional_names <- c("id", "properties")
        validate_object_names(names(parms), required_names, optional_names)
    },

    tpl_op=function(op="", ...)
    {
        op <- file.path("resourcegroups", self$resource_group, "providers/Microsoft.Resources/deployments", self$name, op)
        call_azure_rm(self$token, self$subscription, op, ...)
    }
))

