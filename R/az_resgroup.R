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

        self$name <- parms$name
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
        op <- paste0("resourcegroups/", self$name)
        call_azure_rm(self$token, self$subscription, op, http_verb="DELETE")
        message("Resource group '", self$name, "' deleted")
        private$is_valid <- FALSE
        invisible(NULL)
    },

    check=function()
    {
        op <- paste0("resourcegroups/", self$name)
        res <- call_azure_rm(self$token, self$subscription, op, http_verb="HEAD", http_status_handler="pass")
        private$is_valid <- httr::status_code(res) < 300
        private$is_valid
    },

    create_resource=function(...) { },
    delete_resource=function(...) { },
    get_resource=function(...) { },
    list_resources=function() { }
),

private=list(
    is_valid=FALSE,

    init=function(name, parms)
    {
        if(is_empty(parms))
        {
            op <- paste0("resourcegroups/", name)
            parms <- call_azure_rm(self$token, self$subscription, op)
        }
        else private$validate_parms(parms)
        parms
    },

    init_and_create=function(name, parms)
    {
        parms <- c(name=name, parms)
        private$validate_parms(parms)
        op <- paste0("resourcegroups/", name)
        parms <- call_azure_rm(self$token, self$subscription, op, body=parms, encode="json", http_verb="PUT")
    },

    validate_parms=function(parms)
    {
        required_names <- c("location", "name")
        optional_names <- c("id", "managedBy", "tags", "properties")
        validate_object_names(names(parms), required_names, optional_names)
    }
))
