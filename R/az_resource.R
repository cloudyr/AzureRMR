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
                        deployed_properties=list())
    {
        self$token <- token
        self$subscription <- subscription

        private$init_id_fields(resource_group, provider, path, type, name, id, deployed_properties)

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

    delete=function() { },
    check=function() { }
),

private=list(
    is_valid=NULL,
    api_version=NULL,

    # initialise identifier fields from multiple ways of constructing object
    init_id_fields=function(resource_group, provider, path, type, name, id, parms)
    {
        # if this is supplied, fill in everything else from it
        if(!is_empty(parms))
        {
            id <- parms$id
            resource_group <- sub("^.+resourceGroups/([^/]+)/.*$", "\\1", id, ignore.case=TRUE)
            type <- dirname(sub("^.+providers/", "", id))
            name <- basename(id)
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
        private$res_op()
    },

    init_and_deploy=function(...)
    {
        properties <- list(...)

        # check if properties is a json object (?)
        if(length(properties) == 1 && is.character(properties[[1]]) && jsonlite::validate(properties[[1]]))
            properties <- jsonlite::fromJSON(properties[[1]])

        validate_deploy_parms(properties)
        res_op(body=properties, encode="json", http_verb="PUT")
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
        apis <- named_list(call_azure_rm(self$token, self$subscription, op)$resourceTypes, "resourceType")

        private$api_version <- apis[[path]]$apiVersions[[1]]
    },

    res_op=function(op="", ...)
    {
        # make sure we have an API to call: this is unset at initialisation for efficiency
        if(is.null(private$api_version))
            private$set_api_version(type=self$type)

        op <- file.path("resourcegroups", self$resource_group, "providers", self$type, self$name, op)
        call_azure_rm(self$token, self$subscription, op, ..., api_version=private$api_version)
    }
))
