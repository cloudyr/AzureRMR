#' @export
az_role_definition <- R6::R6Class("az_role_definition",

public=list(

    id=NULL,
    name=NULL,
    type=NULL,
    properties=NULL,
    token=NULL,

    initialize=function(token, parameters)
    {
        self$token <- token
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

    initialize=function(token, parameters, role_name=NULL)
    {
        self$token <- token
        self$id <- parameters$id
        self$name <- parameters$name
        self$type <- parameters$type
        self$properties <- parameters$properties
        self$role_name <- role_name
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
))
