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
    initialize=function(token, subscription, resource_group, name=NULL, template, parameters, ...,
                        deployed_properties=list(...), create=FALSE)
    {
        if(is_empty(name) && is_empty(deployed_properties))
            stop("Must supply either template name, or deployed properties list")

        self$token <- token
        self$subscription <- subscription
        self$resource_group <- resource_group

        parms <- if(create)
            private$init_and_create(name, template, parameters, deployed_properties)
        else private$init(name, deployed_properties)

        self$id <- parms$id
        self$properties <- parms$properties

        private$is_valid <- TRUE
        NULL
    },

    delete=function(free_resources=FALSE)
    {
        if(free_resources)
        {
            message("Deleting resources for template '", self$name, "'...'")
            # recursively delete all resources for this template
        }

        private$tpl_op(http_verb="DELETE")
        message("Template '", self$name, "' deleted")
        private$is_valid <- FALSE
        invisible(NULL)
    },

    check=function()
    {
        res <- private$tpl_op(http_verb="HEAD", http_status_handler="pass")
        private$is_valid <- httr::status_code(res) < 300
        private$is_valid
    }
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

    # deployment workhorse function
    init_and_create=function(name, template, parameters, deploy_properties)
    {
        validate_deploy_properties(deploy_properties)

        default_properties <- list(
            debugSetting=list(detailLevel="requestContent, responseContent"),
            mode="Incremental"
        )
        properties <- modifyList(default_properties, deploy_properties)

        properties <- if(is.list(template))
            modifyList(properties, list(template=template))
        else if(is_url(template))
            modifyList(properties, list(templateLink=list(uri=template)))
        else modifyList(properties, list(template=jsonlite::fromJSON(template)))

        properties <- if(is.list(parameters))
            modifyList(properties, list(parameters=parameters))
        else if(is_url(parameters))
            modifyList(properties, list(parametersLink=list(uri=parameters)))
        else modifyList(properties, list(parameters=jsonlite::fromJSON(parameters)))

        self$name <- name
        private$tpl_op(body=properties, encode="json", http_verb="PUT")
    },

    validate_parms=function(properties)
    {
        required_names <- c("name")
        optional_names <- c("id", "properties")
        validate_object_names(names(properties), required_names, optional_names)
    },

    validate_deploy_properties=function(properties)
    {
        required_names <- c("debugSetting", "mode")
        optional_names <- c("onErrorDeployment")
        validate_object_names(names(properties), required_names, optional_names)
    },

    tpl_op=function(op="", ...)
    {
        op <- file.path("resourcegroups", self$resource_group, "providers/Microsoft.Resources/deployments", self$name, op)
        call_azure_rm(self$token, self$subscription, op, ...)
    },

    # check if a string appears to be a URL (only https allowed)
    is_url=function(x)
    {
        is.character(x) && length(x) == 1 && grepl("^https://", x)
    }
))

