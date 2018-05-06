#' @export
az_resource_group <- R6::R6Class("az_resource_group",

public=list(
    subscription=NULL,
    id=NULL,
    name=NULL,
    location=NULL,
    managed_by=NULL,
    properties=NULL,
    tags=NULL,
    token=NULL,

    # constructor: can refer to an existing RG, or create a new RG
    initialize=function(token, subscription, name=NULL, ..., parms=list(...), create=FALSE)
    {
        if(is_empty(name) && is_empty(parms))
            stop("Must supply either resource group name, or parameter list")

        self$token <- token
        self$subscription <- subscription

        parms <- if(create)
            private$init_and_create(name, parms)
        else private$init(name, parms)

        self$id <- parms$id
        self$location <- parms$location
        self$managed_by <- parms$managedBy
        self$properties <- parms$properties
        self$tags <- parms$tags

        private$is_valid <- TRUE
        NULL
    },

    delete=function()
    {
        # TODO: allow wait until complete
        private$rg_op(http_verb="DELETE")
        message("Resource group '", self$name, "' deleted")
        private$is_valid <- FALSE
        invisible(NULL)
    },

    check=function()
    {
        res <- private$rg_op(http_verb="HEAD", http_status_handler="pass")
        private$is_valid <- httr::status_code(res) < 300
        private$is_valid
    },

    list_templates=function()
    {
        # TODO: handle paging
        res <- private$rg_op("providers/Microsoft.Resources/deployments")$value
        lst <- lapply(res, function(parms) az_template$new(self$token, self$subscription, self$name,
            deployed_properties=parms))
        named_list(lst)
    },

    deploy_template=function(template_name, template, parameters, ...)
    {
        az_template$new(self$token, self$subscription, self$name, template_name,
                    template, parameters, ...)
    },

    get_template=function(template_name)
    {
        az_template$new(self$token, self$subscription, self$name, template_name)
    },

    delete_template=function(template_name, free_resources=FALSE)
    {
        self$get_template(template_name)$delete(free_resources=free_resources)
    },

    list_resources=function()
    {
        # TODO: handle paging
        res <- private$rg_op("resources")$value
        lst <- lapply(res, function(parms) az_resource$new(self$token, self$subscription, deployed_properties=parms))
        named_list(lst)
    },

    get_resource=function(provider, path, type, name, id)
    {
        az_resource$new(self$token, self$subscription,
                        resource_group=self$name, provider=provider, path=path, type=type, name=name, id=id)
    },

    delete_resource=function(...)
    {
        self$get_resource(...)$delete()
    },

    create_resource=function(provider, path, type, name, id, ...)
    {
        az_resource$new(self$token, self$subscription,
                        resource_group=self$name, provider=provider, path=path, type=type, name=name, id=id, ...)
    }
),

private=list(
    is_valid=FALSE,

    init=function(name, parms)
    {
        if(is_empty(parms))
        {
            self$name <- name
            parms <- private$rg_op()
        }
        else
            {
            private$validate_parms(parms)
            self$name <- name
        }
        parms
    },

    init_and_create=function(name, parms)
    {
        parms <- c(name=name, parms)
        private$validate_parms(parms)
        self$name <- name
        private$rg_op(body=parms, encode="json", http_verb="PUT")
    },

    validate_parms=function(parms)
    {
        required_names <- c("location", "name")
        optional_names <- c("id", "managedBy", "tags", "properties")
        validate_object_names(names(parms), required_names, optional_names)
    },

    rg_op=function(op="", ...)
    {
        op <- file.path("resourcegroups", self$name, op)
        call_azure_rm(self$token, self$subscription, op, ...)
    }
))

