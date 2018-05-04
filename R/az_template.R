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
    init_and_create=function(name, template, parameters, deployment_properties)
    {
        validate_deploy_names(deployment_properties)

        default_properties <- list(
            debugSetting=list(detailLevel="requestContent, responseContent"),
            #onErrorDeployment=list(deploymentName="error", type="lastsuccessful"),
            mode="Incremental"
        )
        parms <- modifyList(default_properties, deployment_properties)

        parms <- if(is.list(template))
            modifyList(parms, list(template=template))
        else if(jsonlite::validate(template))
            modifyList(parms, list(template=jsonlite::fromJSON(template)))
        else if(is_url(template))
            modifyList(parms, list(templateLink=list(uri=template)))
        else stop("Invalid template")

        parms <- if(is.list(parameters))
            modifyList(parms, list(parameters=parameters))
        else if(jsonlite::validate(parameters))
            modifyList(parms, list(parameters=jsonlite::fromJSON(parameters)))
        else if(is_url(parameters))
            modifyList(parms, list(parametersLink=list(uri=parameters)))
        else stop("Invalid template parameters")

        self$name <- name
        private$tpl_op(body=parms, encode="json", http_verb="PUT")
    },

    validate_parms=function(parms)
    {
        required_names <- c("name")
        optional_names <- c("id", "properties")
        validate_object_names(names(parms), required_names, optional_names)
    },

    validate_deploy_parms=function(parms)
    {
        required_names <- c("debugSetting", "mode")
        optional_names <- c("onErrorDeployment")
        validate_object_names(names(parms), required_names, optional_names)
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

