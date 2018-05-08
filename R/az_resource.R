#' @export
az_resource <- R6::R6Class("az_resource",

public=list(
    subscription=NULL,
    resource_group=NULL,
    type=NULL,
    name=NULL,
    id=NULL,
    identity=NULL,
    kind=NULL,
    location=NULL,
    managed_by=NULL,
    plan=NULL,
    properties=NULL,
    sku=NULL,
    tags=NULL,
    token=NULL,
    is_synced=FALSE,

    # constructor overloads:
    # 1. deploy resource: resgroup, {provider, path}|type, name, ...
    # 2. deploy resource by id: id, ...
    # 3. get from passed-in data: deployed_properties
    # 4. get from host: resgroup, {provider, path}|type, name
    # 5. get from host by id: id
    initialize=function(token, subscription, resource_group, provider, path, type, name, id, ...,
                        deployed_properties=list(), api_version=NULL)
    {
        self$token <- token
        self$subscription <- subscription

        private$init_id_fields(resource_group, provider, path, type, name, id, deployed_properties)

        # by default this is unset at initialisation, for efficiency
        private$api_version <- api_version

        parms <- if(!is_empty(list(...)))
            private$init_and_deploy(...)
        else if(is_empty(deployed_properties))
            private$init_from_host()
        else private$init_from_parms(deployed_properties)

        self$identity <- parms$identity
        self$kind <- parms$kind
        self$location <- parms$location
        self$managed_by <- parms$managedBy
        self$plan <- parms$plan
        self$properties <- parms$properties
        self$sku <- parms$sku
        self$tags <- parms$tags

        private$is_valid <- TRUE
        NULL
    },

    # API versions vary across different providers; find the latest for this resource
    set_api_version=function(api_version=NULL)
    {
        if(!is_empty(api_version))
        {
            private$api_version <- api_version
            return()
        }

        slash <- regexpr("/", self$type)
        provider <- substr(self$type, 1, slash - 1)
        path <- substr(self$type, slash + 1, nchar(self$type))

        op <- file.path("providers", provider)
        apis <- named_list(call_azure_rm(self$token, self$subscription, op)$resourceTypes, "resourceType")

        names(apis) <- tolower(names(apis))
        private$api_version <- apis[[tolower(path)]]$apiVersions[[1]]
        if(is_empty(private$api_version))
            stop("Unable to retrieve API version for resource '", self$type, ".", call.=FALSE)

        invisible(private$api_version)
    },

    sync_fields=function(force=FALSE)
    {
        if(force || !self$is_synced)
            self$initialize(self$token, self$subscription, id=self$id)
        invisible(NULL)
    },

    delete=function(confirm=TRUE, wait=FALSE)
    {
        if(confirm && interactive())
        {
            yn <- readline(paste0("Do you really want to delete resource '", self$type, "/", self$name, "'? (y/N) "))
            if(tolower(substr(yn, 1, 1)) != "y")
                return(invisible(NULL))
        }

        private$res_op(http_verb="DELETE")
        message("Deleting resource '", file.path(self$type, self$name), "'")

        if(wait)
        {
            for(i in 1:1000)
            {
                status <- httr::status_code(private$res_op(http_status_handler="pass"))
                if(status >= 300)
                    break
                Sys.sleep(5)
            }
            if(status < 300)
                warning("Attempt to delete resource did not succeed", call.=FALSE)
        }

        private$is_valid <- FALSE
        invisible(NULL)
    },

    do_operation=function(http_verb="GET", ..., options=list())
    {
        private$res_op(..., http_verb=http_verb, options=options)
    },

    check=function()
    {
        # HEAD seems to be broken; do a GET and test whether it fails
        res <- try(private$res_op())
        !inherits(res, "try-error")
    }
),

private=list(
    is_valid=NULL,
    api_version=NULL,

    # initialise identifier fields from multiple ways of constructing object
    init_id_fields=function(resource_group, provider, path, type, name, id, parms=list())
    {
        # if this is supplied, fill in everything else from it
        if(!is_empty(parms))
        {
            resource_group <- sub("^.+resourceGroups/([^/]+)/.*$", "\\1", parms$id, ignore.case=TRUE)
            type <- parms$type
            name <- parms$name
            id <- parms$id
        }
        else if(!missing(id))
        {
            resource_group <- sub("^.+resourceGroups/([^/]+)/.*$", "\\1", id, ignore.case=TRUE)
            type <- dirname(sub("^.+providers/", "", id))
            name <- basename(id)
        }
        else
        {
            if(missing(type))
                type <- file.path(provider, path)
            id <- file.path("/subscriptions", self$subscription, "resourceGroups", resource_group, "providers", type, name)
        }
        self$resource_group <- resource_group
        self$type <- type
        self$name <- name
        self$id <- id
    },

    init_from_parms=function(parms)
    {
        private$validate_response_parms(parms)
        parms
    },

    init_from_host=function()
    {
        res <- private$res_op()
        self$is_synced <- attr(res, "status") < 202
        res
    },

    init_and_deploy=function(...)
    {
        properties <- list(...)

        # check if we were passed a json object
        if(length(properties) == 1 && is.character(properties[[1]]) && jsonlite::validate(properties[[1]]))
            properties <- jsonlite::fromJSON(properties[[1]], simplifyVector=FALSE)

        properties <- modifyList(properties, list(name=self$name, type=self$type))
        private$validate_deploy_parms(properties)
        private$res_op(body=properties, encode="json", http_verb="PUT")

        # allow time for provisioning setup, then get properties
        Sys.sleep(1)
        private$init_from_host()
    },

    validate_deploy_parms=function(parms)
    {
        required_names <- c("name", "type", "location")
        optional_names <- c("identity", "kind", "managedBy", "plan", "properties", "sku", "tags", "scale", "comments")
        validate_object_names(names(parms), required_names, optional_names)
    },

    validate_response_parms=function(parms)
    {
        required_names <- c("id", "name", "type", "location")
        optional_names <- c("identity", "kind", "managedBy", "plan", "properties", "sku", "tags")
        validate_object_names(names(parms), required_names, optional_names)
    },

    res_op=function(op="", ...)
    {
        # make sure we have an API to call
        if(is.null(private$api_version))
            self$set_api_version()

        op <- file.path("resourcegroups", self$resource_group, "providers", self$type, self$name, op)
        call_azure_rm(self$token, self$subscription, op, ..., api_version=private$api_version)
    }
))
