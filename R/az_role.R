#' @export
az_role_definition <- R6::R6Class("az_role_definition",

public=list(

    id=NULL,
    name=NULL,
    type=NULL,
    properties=NULL,

    initialize=function(parameters)
    {
        self$id <- parameters$id
        self$name <- parameters$name
        self$type <- parameters$type
        self$properties <- parameters$properties
    },

    print=function(...)
    {
        cat("<Azure role definition>\n")
        cat("  role:", self$properties$roleName, "\n")
        cat("  description:", self$properties$description, "\n")
        cat("  role definition ID:", self$name, "\n")
        invisible(self)
    }
))


#' @export
az_role_assignment <- R6::R6Class("az_role_assignment",

public=list(

    # text name of role definition
    role_name=NULL,

    id=NULL,
    name=NULL,
    type=NULL,
    properties=NULL,
    token=NULL,

    initialize=function(token, parameters, role_name=NULL, api_func=NULL)
    {
        self$token <- token
        self$id <- parameters$id
        self$name <- parameters$name
        self$type <- parameters$type
        self$properties <- parameters$properties
        self$role_name <- role_name

        private$api_func <- api_func
    },

    remove=function(confirm=TRUE)
    {
        if(confirm && interactive())
        {
            yn <- readline(paste0("Do you really want to delete role assignment '", basename(self$id), "'? (y/N) "))
            if(tolower(substr(yn, 1, 1)) != "y")
                return(invisible(NULL))
        }

        op <- file.path("providers/Microsoft.Authorization/roleAssignments", basename(self$id))
        res <- private$api_func(op, api_version=getOption("azure_rbac_api_version"), http_verb="DELETE")
        if(attr(res, "status") == 204)
            warning("Role assignment not found or could not be deleted")
        invisible(NULL)
    },

    print=function(...)
    {
        cat("<Azure role assignment>\n")
        cat("  principal:", self$properties$principalId, "\n")

        if(!is_empty(self$role_name))
            cat("  role:", self$role_name, "\n")
        else cat("  role: <unknown>\n")

        cat("  role definition ID:", basename(self$properties$roleDefinitionId), "\n")
        cat("  role assignment ID:", self$name, "\n")
        invisible(self)
    }
),

private=list(

    api_func=NULL
))
