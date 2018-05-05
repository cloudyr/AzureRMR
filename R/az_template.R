#' @export
az_template <- R6::R6Class("az_template",

public=list(
    subscription=NULL,
    resource_group=NULL,
    id=NULL,
    name=NULL,
    properties=NULL,
    token=NULL,

    # constructor overloads: 1) get an existing template from host; 2) from passed-in data; 3) deploy new template
    initialize=function(token, subscription, resource_group, name=NULL, template, parameters, ...,
                        deployed_properties=list())
    {
        self$token <- token
        self$subscription <- subscription
        self$resource_group <- resource_group

        parms <- if(!is_empty(name) && !missing(template) && !missing(parameters))
            private$init_and_deploy(name, template, parameters, ...)
        else if(!is_empty(name))
            private$init_from_host(name)
        else if(!is_empty(deployed_properties))
            private$init_from_parms(deployed_properties)
        else stop("Invalid initialization call")

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
            # TODO: recursively delete all resources for this template
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

    init_from_host=function(name)
    {
        self$name <- name
        private$tpl_op()
    },

    init_from_parms=function(parms)
    {
        private$validate_parms(parms)
        self$name <- parms$name
        parms
    },

    # deployment workhorse function
    # TODO: allow wait until complete
    init_and_deploy=function(name, template, parameters, ...)
    {
        default_properties <- list(
            debugSetting=list(detailLevel="requestContent, responseContent"),
            mode="Incremental"
        )
        properties <- modifyList(default_properties, list(...))
        private$validate_deploy_properties(properties)

        # fold template data into list of properties
        properties <- if(is.list(template))
            modifyList(properties, list(template=template))
        else if(is_url(template))
            modifyList(properties, list(templateLink=list(uri=template)))
        else modifyList(properties, list(template=jsonlite::fromJSON(template, simplifyVector=FALSE)))

        # fold parameter data into list of properties
        properties <- if(is.list(parameters))
            modifyList(properties, list(parameters=parameters))
        else if(is_url(parameters))
            modifyList(properties, list(parametersLink=list(uri=parameters)))
        else modifyList(properties, list(parameters=jsonlite::fromJSON(parameters, simplifyVector=FALSE)))

        self$name <- name
        private$tpl_op(body=list(properties=properties), encode="json", http_verb="PUT")
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
    }
))

