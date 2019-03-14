#' Azure role definition class
#'
#' @docType class
#' @section Fields:
#' - `id`: The full resource ID for this role definition.
#' - `type`: The resource type for a role definition. Always `Microsoft.Authorization/roleDefinitions`.
#' - `name`: A GUID that identifies this role definition.
#' - `properties`: Properties for the role definition.
#'
#' @section Methods:
#' This class has no methods.
#'
#' @section Initialization:
#' The recommended way to create new instances of this class is via the [get_role_definition] method for subscription, resource group and resource objects.
#'
#' Technically role assignments and role definitions are Azure _resources_, and could be implemented as subclasses of `az_resource`. AzureRMR treats them as distinct, due to limited RBAC functionality currently supported. In particular, role definitions are read-only: you can retrieve a definition, but not modify it, nor create new definitions.
#'
#' @seealso
#' [get_role_definition], [get_role_assignment], [az_role_assignment]
#'
#' [Overview of role-based access control](https://docs.microsoft.com/en-us/azure/role-based-access-control/overview)
#'
#' @format An R6 object of class `az_role_definition`.
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


#' Azure role assignment class
#'
#' @docType class
#' @section Fields:
#' - `id`: The full resource ID for this role assignment.
#' - `type`: The resource type for a role assignment. Always `Microsoft.Authorization/roleAssignments`.
#' - `name`: A GUID that identifies this role assignment.
#' - `role_name`: The role definition name (in text), eg "Contributor".
#' - `properties`: Properties for the role definition.
#' - `token`: An OAuth token, obtained via [get_azure_token].
#'
#' @section Methods:
#' - `remove(confirm=TRUE)`: Removes this role assignment.
#'
#' @section Initialization:
#' The recommended way to create new instances of this class is via the [create_role_assignment] and [get_role_assignment] methods for subscription, resource group and resource objects.
#'
#' Technically role assignments and role definitions are Azure _resources_, and could be implemented as subclasses of `az_resource`. AzureRMR treats them as distinct, due to limited RBAC functionality currently supported.
#'
#' @seealso
#' [create_role_assignment], [get_role_assignment], [get_role_definition], [az_role_definition]
#'
#' [Overview of role-based access control](https://docs.microsoft.com/en-us/azure/role-based-access-control/overview)
#'
#' @format An R6 object of class `az_role_assignment`.
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
            yn <- readline(paste0("Do you really want to delete role assignment '", self$name, "'? (y/N) "))
            if(tolower(substr(yn, 1, 1)) != "y")
                return(invisible(NULL))
        }

        op <- file.path("providers/Microsoft.Authorization/roleAssignments", self$name)
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
