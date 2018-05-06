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
                        deployed_properties=list(), wait=FALSE)
    {
        self$token <- token
        self$subscription <- subscription
        self$resource_group <- resource_group

        parms <- if(!is_empty(name) && !missing(template) && !missing(parameters))
            private$init_and_deploy(name, template, parameters, ..., wait=wait)
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

    cancel=function(free_resources=FALSE)
    {
        message("Cancelling deployment of template '", self$name, "'")
        if(free_resources)
            private$free_resources()
        else message("Associated resources have not been freed.")

        private$tpl_op("cancel", http_verb="POST")
        private$is_valid <- FALSE
        invisible(NULL)
    },

    delete=function(free_resources=FALSE)
    {
        message("Deleting template '", self$name, "'")
        if(free_resources)
            private$free_resources()
        else message("Associated resources will not be freed")

        private$tpl_op(http_verb="DELETE")
        private$is_valid <- FALSE
        invisible(NULL)
    },

    # update state of template: deployment accepted/deployment failed/updating/running/failed
    check=function()
    {
        self$initialize(self$token, self$subscription, self$resource_group, self$name)
        status <- self$properties$provisioningState
        if(status %in% c("Error", "Failed"))
            private$is_valid <- FALSE
        status
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
    init_and_deploy=function(name, template, parameters, ..., wait=FALSE)
    {
        message("Deploying template '", name, "'")

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

        # do we wait until template has finished provisioning?
        if(wait)
        {
            message("Waiting for provisioning to complete")
            for(i in 1:1000) # some resources can take a long time to provision (HDInsight)
            {
                Sys.sleep(5)
                message(".", appendLF=FALSE)

                status <- self$check()
                if(status == "Succeeded")
                    break
                else if(status %in% c("Error", "Failed"))
                    stop("Unable to deploy template", call.=FALSE)
            }
            message("\nDeployment successful")
        }
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

    free_resources=function()
    {
        free_dependency <- function(id)
        {
            res <- try(az_resource$new(self$token, self$subscription, id=id), silent=TRUE)
            if(!inherits(res, "try-error"))
                res$delete(wait=TRUE)
        }

        # assumptions:
        # - this is a flattened 2-level list of dependencies, not an actual tree
        # - earlier dependencies depend on later ones
        # - later entries in list will have been deleted in earlier entries
        deps <- self$properties$dependencies
        for(d in deps)
        {
            free_dependency(d$id)
            for(d2 in d$dependsOn)
                free_dependency(d2$id)
        }
    },

    tpl_op=function(op="", ...)
    {
        op <- file.path("resourcegroups", self$resource_group, "providers/Microsoft.Resources/deployments", self$name, op)
        call_azure_rm(self$token, self$subscription, op, ...)
    }
))

