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

        if(is.null(api_version))
            private$set_api_version(provider, path, type, id)
        else private$api_version <- api_version

        parms <- if(!is_empty(list(...)))
            private$init_and_deploy(resource_group, provider, path, type, name, id, ...)
        else if(!is_empty(deployed_properties))
            private$init_from_parms(deployed_properties)
        else private$init_from_host(resource_group, provider, path, type, name, id)

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

    delete=function() { },
    check=function() { }
),

private=list(
    is_valid=NULL,
    api_version=NULL,

    init_from_parms=function(parms)
    {
        private$validate_response_parms(parms)

        self$resource_group <- private$extract_rg(parms$id)
        self$type <- parms$type
        self$name <- parms$name
        self$id <- parms$id
        parms
    },

    init_from_host=function(resource_group, provider, path, type, name, id)
    {
        init_from_host_by_id <- function(id)
        {
            self$id <- id

            parms <- private$res_id_op()

            self$resource_group <- private$extract_rg(parms$id)
            self$type <- parms$type
            self$name <- parms$name
            parms
        }
        init_from_host_by_args <- function(resource_group, type, name)
        {
            self$resource_group <- parms$resource_group
            self$type <- parms$type
            self$name <- parms$name

            parms <- private$res_op()

            self$id <- parms$id
            parms
        }

        if(!missing(id))
            init_from_host_by_id(id)
        else
        {
            if(!missing(provider) && !missing(path))
                type <- file.path(provider, path)
            init_from_host_by_args(resource_group, type, name)
        }
    },

    init_and_deploy=function(resource_group, provider, path, type, name, id, ...)
    {
        init_and_deploy_by_id <- function(id, properties)
        {
            self$id <- id

            parms <- res_id_op(body=properties, encode="json", http_verb="PUT")

            self$resource_group <- private$extract_rg(parms$id)
            self$type <- parms$type
            self$name <- parms$name
            parms
        }
        init_and_deploy_by_args <- function(resource_group, type, name, properties)
        {
            self$resource_group <- parms$resource_group
            self$type <- parms$type
            self$name <- parms$name

            res_op(body=properties, encode="json", http_verb="PUT")

            self$id <- parms$id
            parms
        }

        properties <- list(...)

        # check if properties is a json object (?)
        if(length(properties) == 1 && is.character(properties[[1]]) && jsonlite::validate(properties[[1]]))
            properties <- jsonlite::fromJSON(properties[[1]])

        validate_deploy_parms(properties)

        if(!missing(id))
            init_and_deploy_by_id(id, properties)
        else
        {
            if(!missing(provider) && !missing(path))
                type <- file.path(provider, path)
            init_and_deploy_by_args(resource_group, type, name, properties)
        }
    },

    validate_deploy_parms=function(parms)
    {
        required_names <- c("location")
        optional_names <- c("identity", "kind", "managedBy", "plan", "properties", "sku", "tags")
        validate_object_names(names(parms), required_names, optional_names)
    },

    validate_response_parms=function(parms)
    {
        required_names <- c("id", "name", "type", "location")
        optional_names <- c("identity", "kind", "managedBy", "plan", "properties", "sku", "tags")
        validate_object_names(names(parms), required_names, optional_names)
    },

    extract_rg=function(id)
    {
        sub("^.+resourceGroups/([^/]+)/.*$", "\\1", id, ignore.case=TRUE)
    },

    # API versions vary across different providers; find the latest for this resource
    set_api_version=function(provider, path, type, id)
    {
        if(missing(provider) && missing(path))
        {
            if(missing(type))
                type <- dirname(sub("^.+providers/", "", id))

            slash <- regexpr("/", type)
            provider <- substr(type, 1, slash - 1)
            path <- substr(type, slash + 1, nchar(type))
        }

        op <- file.path("providers", provider)
        apis <- named_list(call_azure_rm(self$token, self$id, op)$resourceTypes, "resourceType")

        private$api_version <- apis[[path]]$apiVersions[[1]]
    },

    res_op=function(op="", ...)
    {
        op <- file.path("resourcegroups", self$resource_group, "providers", self$type, self$name, op)
        call_azure_rm(self$token, self$subscription, op, ..., api_version=private$api_version)
    },

    res_id_op=function(op="", ...)
    {
        # strip off subscription, which is handled by call_azure_rm separately
        id <- sub("^.+/resourcegroups", "resourcegroups", self$id, ignore.case=TRUE)
        op <- file.path(id, op)
        call_azure_rm(self$token, self$subscription, op, ..., api_version=private$api_version)
    }
))
