#' Role-based access control (RBAC)
#'
#' Basic methods for RBAC: manage role assignments and retrieve role definitions. These are methods for the `az_subscription`, `az_resource_group` and `az_resource` classes.
#'
#' @section Usage:
#' ```
#' add_role_assignment(principal, role, scope = NULL)
#'
#' get_role_assignment(id)
#'
#' remove_role_assignment(id, confirm = TRUE)
#'
#' list_role_assignments(filter = "atScope()", as_data_frame = TRUE) 
#'
#' get_role_definition(id)
#'
#' list_role_definitions(filter="atScope()", as_data_frame = TRUE)
#' ```
#' @section Arguments:
#' - `principal`: For `add_role_assignment`, the principal for which to assign a role. This can be a GUID, or an object of class `az_app` or `az_storage_principal` (from the AzureGraph package).
#' - `role`: For `add_role_assignment`, the role to assign the principal. This can be a GUID, a string giving the role name (eg "Contributor"), or an object of class `[az_role_definition]`.
#' - `scope`: For `add_role_assignment`, an optional scope for the assignment.
#' - `id`: A role ID. For `get_role_assignment` and `remove_role_assignment`, this is a role assignment GUID. For `get_role_definition`, this can be a role definition GUID or a role name.
#' - `confirm`: For `remove_role_assignment`, whether to ask for confirmation before removing the role assignment.
#' - `filter`: For `list_role_assignments` and `list_role_definitions`, an optional filter condition to limit the returned roles.
#' - `as_data_frame`: For `list_role_assignments` and `list_role_definitions`, whether to return a data frame or a list of objects. See 'Value' below.
#'
#' @section Details:
#' AzureRMR implements a subset of the full RBAC functionality within Azure Active Directory. You can retrieve role definitions and add and remove role assignments, at the subscription, resource group and resource levels.
#'
#' @section Value:
#' The `add_role_assignment` and `get_role_assignment` methods return an object of class `az_role_assignment`. This is a simple R6 class, with one method: `remove` to remove the assignment.
#'
#' The `list_role_assignments` method returns a list of `az_role_assignment` objects if the `as_data_frame` argument is FALSE. If this is TRUE, it instead returns a data frame containing the most broadly useful fields for each assigned role: the role assignment ID, the principal, and the role name.
#'
#' The `get_role_definition` method returns an object of class `az_role_definition`. This is a plain-old-data R6 class (no methods), which can be used as input for creating role assignments (see the examples below).
#'
#' The `list_role_definitions` method returns a list of `az_role_definition` if the `as_data_frame` argument is FALSE. If this is TRUE, it instead returns a data frame containing the most broadly useful fields for each role definition: the definition ID and role name.
#'
#' @seealso
#' [az_rm], [az_role_definition], [az_role_assignment]
#'
#' [Overview of role-based access control](https://docs.microsoft.com/en-us/azure/role-based-access-control/overview)
#'
#' @examples
#' \dontrun{
#'
#' az <- get_azure_login("myaadtenant")
#' sub <- az$get_subscription("subscription_id")
#' rg <- sub$get_resource_group("rgname")
#' res <- rg$get_resource(type="provider_type", name="resname")
#'
#' sub$list_role_definitions()
#' sub$list_role_assignments()
#' sub$get_role_definition("Contributor")
#'
#' # get an app using the AzureGraph package
#' app <- az_graph$new("myaadtenant")$get_app("app_id")
#'
#' # subscription level
#' asn1 <- sub$add_role_assignment(app, "Reader")
#'
#' # resource group level
#' asn2 <- rg$add_role_assignment(app, "Contributor")
#'
#' # resource level
#' asn3 <- res$add_role_assignment(app, "Owner")
#'
#' res$remove_role_assignment(asn3$id)
#' rg$remove_role_assignment(asn2$id)
#' sub$remove_role_assignment(asn1$id)
#' 
#' }
#'
#' @aliases rbac add_role_assignment get_role_assignment remove_role_assignment list_role_assignments
#'   get_role_definition list_role_definitions
#' @rdname rbac
#' @name rbac
NULL

## subscription methods

az_subscription$set("public", "add_role_assignment", overwrite=TRUE,
function(principal, role, scope=NULL)
{
    if(!is_role_definition(role))
        role <- self$get_role_definition(role)
    add_role_assignment(principal, role, scope, private$sub_op)
})

az_subscription$set("public", "get_role_assignment", overwrite=TRUE,
function(id)
{
    get_role_assignment(id, self$list_role_definitions(), private$sub_op)
})

az_subscription$set("public", "remove_role_assignment", overwrite=TRUE,
function(id, confirm=TRUE)
{
    remove_role_assignment(id, confirm, private$sub_op)
})

az_subscription$set("public", "list_role_assignments", overwrite=TRUE,
function(filter="atScope()", as_data_frame=TRUE)
{
    list_role_assignments(filter, as_data_frame, self$list_role_definitions(), private$sub_op)
})

az_subscription$set("public", "get_role_definition", overwrite=TRUE,
function(id)
{
    get_role_definition(id, self$list_role_definitions(), private$sub_op)
})

az_subscription$set("public", "list_role_definitions", overwrite=TRUE,
function(filter="atScope()", as_data_frame=TRUE)
{
    list_role_definitions(filter, as_data_frame, private$sub_op)
})


## resource group methods

az_resource_group$set("public", "add_role_assignment", overwrite=TRUE,
function(principal, role, scope=NULL)
{
    if(!is_role_definition(role))
        role <- self$get_role_definition(role)
    add_role_assignment(principal, role, scope, private$rg_op)
})

az_resource_group$set("public", "get_role_assignment", overwrite=TRUE,
function(id)
{
    get_role_assignment(id, self$list_role_definitions(), private$rg_op)
})

az_resource_group$set("public", "remove_role_assignment", overwrite=TRUE,
function(id, confirm=TRUE)
{
    remove_role_assignment(id, confirm, private$rg_op)
})

az_resource_group$set("public", "list_role_assignments", overwrite=TRUE,
function(filter="atScope()", as_data_frame=TRUE)
{
    list_role_assignments(filter, as_data_frame, self$list_role_definitions(), private$rg_op)
})

az_resource_group$set("public", "get_role_definition", overwrite=TRUE,
function(id)
{
    get_role_definition(id, self$list_role_definitions(), private$rg_op)
})

az_resource_group$set("public", "list_role_definitions", overwrite=TRUE,
function(filter="atScope()", as_data_frame=TRUE)
{
    list_role_definitions(filter, as_data_frame, private$rg_op)
})


## resource methods

az_resource$set("public", "add_role_assignment", overwrite=TRUE,
function(principal, role, scope=NULL)
{
    if(!is_role_definition(role))
        role <- self$get_role_definition(role)
    add_role_assignment(principal, role, scope, private$res_op)
})

az_resource$set("public", "get_role_assignment", overwrite=TRUE,
function(id)
{
    get_role_assignment(id, self$list_role_definitions(), private$res_op)
})

az_resource$set("public", "remove_role_assignment", overwrite=TRUE,
function(id, confirm=TRUE)
{
    remove_role_assignment(id, confirm, private$res_op)
})

az_resource$set("public", "list_role_assignments", overwrite=TRUE,
function(filter="atScope()", as_data_frame=TRUE)
{
    list_role_assignments(filter, as_data_frame, self$list_role_definitions(), private$res_op)
})

az_resource$set("public", "get_role_definition", overwrite=TRUE,
function(id)
{
    get_role_definition(id, self$list_role_definitions(), private$res_op)
})

az_resource$set("public", "list_role_definitions", overwrite=TRUE,
function(filter="atScope()", as_data_frame=TRUE)
{
    list_role_definitions(filter, as_data_frame, private$res_op)
})


## implementations

add_role_assignment <- function(principal, role, scope, api_func)
{
    # obtain object ID from a service principal or registered app
    if(inherits(principal, "az_service_principal"))
        principal <- principal$properties$objectId
    else if(inherits(principal, "az_app"))
        principal <- principal$get_service_principal()$properties$objectId

    token <- environment(api_func)$self$token
    op <- file.path("providers/Microsoft.Authorization/roleAssignments", uuid::UUIDgenerate())
    body <- list(
        properties=list(
            roleDefinitionId=role$id,
            principalId=principal
        )
    )
    if(!is.null(scope))
        body$properties$scope <- scope

    res <- api_func(op, body=body, encode="json",
        api_version=getOption("azure_rbac_api_version"), http_verb="PUT")
    az_role_assignment$new(token, res, role$properties$roleName, api_func)
}

get_role_assignment <- function(id, defs, api_func)
{
    token <- environment(api_func)$self$token
    op <- file.path("providers/Microsoft.Authorization/roleAssignments", id)
    res <- api_func(op, api_version=getOption("azure_rbac_api_version"))

    role_name <- defs$name[defs$definition_id == basename(res$properties$roleDefinitionId)]
    az_role_assignment$new(token, res, role_name, api_func)
}

remove_role_assignment <- function(id, confirm, api_func)
{
    token <- environment(api_func)$self$token
    # pass minimal list of parameters to init, rather than making useless API calls
    res <- list(name=basename(id))
    az_role_assignment$new(token, res, api_func=api_func)$remove(confirm=confirm)
}

list_role_assignments <- function(filter, as_data_frame, defs, api_func)
{
    token <- environment(api_func)$self$token
    op <- "providers/Microsoft.Authorization/roleAssignments"
    lst <- api_func(op, options=list(`$filter`=filter), api_version=getOption("azure_rbac_api_version"))

    if(as_data_frame)
    {
        lst <- lapply(lst$value, function(res)
        {
            role_name <- defs$name[defs$definition_id == basename(res$properties$roleDefinitionId)]
            data.frame(assignment_id=res$name, principal=res$properties$principalId, role=role_name,
                       stringsAsFactors=FALSE)
        })
        do.call(rbind, lst)
    }
    else lapply(lst$value, function(res)
    {
        role_name <- defs$name[defs$id == basename(res$properties$roleDefinitionId)]
        az_role_assignment$new(token, res, role_name, api_func)
    })
}

get_role_definition <- function(id, defs, api_func)
{
    # if text rolename supplied, get full list of roles and extract from there
    if(!is_guid(id))
    {
        i <- which(tolower(defs$name) == tolower(id))
        if(is_empty(i))
            stop("Unknown role definition", call.=FALSE)
        id <- defs$definition_id[i]
    }

    op <- file.path("providers/Microsoft.Authorization/roleDefinitions", id)
    res <- api_func(op, api_version=getOption("azure_rbac_api_version"))
    az_role_definition$new(res)
}

list_role_definitions <- function(filter, as_data_frame, api_func)
{
    op <- "providers/Microsoft.Authorization/roleDefinitions"
    lst <- api_func(op, options=list(`$filter`=filter), api_version=getOption("azure_rbac_api_version"))

    if(as_data_frame)
    {
        lst <- lapply(lst$value, function(res)
            data.frame(definition_id=res$name, name=res$properties$roleName, stringsAsFactors=FALSE))
        do.call(rbind, lst)
    }
    else
    {
        lst <- lapply(lst$value, function(res)
            az_role_definition$new(res))
        
        names(lst) <- sapply(lst, function(res) res$properties$roleName)
        lst
    }
}
