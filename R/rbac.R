# basic methods for RBAC: manage role assignments, retrieve role definitions

## subscription methods

az_subscription$set("public", "add_role_assignment", overwrite=TRUE,
function(principal, role, scope=NULL, new_id=uuid::UUIDgenerate())
{
    add_role_assignment(principal, self$get_role_definition(role), scope, new_id, private$sub_op)
})

az_subscription$set("public", "get_role_assignment", overwrite=TRUE,
function(id)
{
    get_role_assignment(id, private$sub_op)
})

az_subscription$set("public", "remove_role_assignment", overwrite=TRUE,
function(role_id, app_id, confirm=TRUE)
{
    remove_role_assignment(id, app_id, confirm, private$sub_op)
})

az_subscription$set("public", "list_role_assignments", overwrite=TRUE,
function(filter="atScope()")
{
    list_role_assignments(filter, self$list_role_definitions(), private$sub_op)
})

az_subscription$set("public", "get_role_definition", overwrite=TRUE,
function(id)
{
    get_role_definition(id, self$list_role_definitions(), private$sub_op)
})

az_subscription$set("public", "list_role_definitions", overwrite=TRUE,
function(filter="atScope()")
{
    list_role_definitions(filter, self$id, private$sub_op)
})


## resource group methods

az_resource_group$set("public", "add_role_assignment", overwrite=TRUE,
function(principal, role, scope=NULL, new_id=uuid::UUIDgenerate())
{
    add_role_assignment(principal, self$get_role_definition(role), scope, new_id, private$rg_op)
})

az_resource_group$set("public", "get_role_assignment", overwrite=TRUE,
function(id)
{
    get_role_assignment(id, private$rg_op)
})

az_resource_group$set("public", "remove_role_assignment", overwrite=TRUE,
function(id, confirm=TRUE)
{
    remove_role_assignment(id, confirm, private$rg_op)
})

az_resource_group$set("public", "list_role_assignments", overwrite=TRUE,
function(filter="atScope()")
{
    list_role_assignments(filter, self$list_role_definitions(), private$rg_op)
})

az_resource_group$set("public", "get_role_definition", overwrite=TRUE,
function(id)
{
    get_role_definition(id, self$list_role_definitions(), private$rg_op)
})

az_resource_group$set("public", "list_role_definitions", overwrite=TRUE,
function(filter="atScope()")
{
    list_role_definitions(filter, private$rg_op)
})


## resource methods

az_resource$set("public", "add_role_assignment", overwrite=TRUE,
function(principal, role, scope=NULL, new_id=uuid::UUIDgenerate())
{
    add_role_assignment(principal, self$get_role_definition(role), scope, new_id, private$res_op)
})

az_resource$set("public", "get_role_assignment", overwrite=TRUE,
function(id)
{
    get_role_assignment(id, private$res_op)
})

az_resource$set("public", "remove_role_assignment", overwrite=TRUE,
function(id, confirm=TRUE)
{
    remove_role_assignment(id, confirm, private$res_op)
})

az_resource$set("public", "list_role_assignments", overwrite=TRUE,
function(filter="atScope()")
{
    list_role_assignments(filter, self$list_role_definitions(), private$res_op)
})

az_resource$set("public", "get_role_definition", overwrite=TRUE,
function(id)
{
    get_role_definition(id, self$list_role_definitions(), private$res_op)
})

az_resource$set("public", "list_role_definitions", overwrite=TRUE,
function(filter="atScope()")
{
    list_role_definitions(filter, private$res_op)
})


## implementations

add_role_assignment <- function(principal, role, scope, new_id, api_func)
{
    # obtain object ID from a service principal or registered app
    if(is_service_principal(principal))
        principal <- principal$properties$objectId
    else if(is_app(principal))
        principal <- principal$get_service_principal()$properties$objectId

    op <- file.path("providers/Microsoft.Authorization/roleAssignments", new_id)
    body <- list(
        properties=list(
            roleDefinitionId=role$id,
            principalId=principal
        )
    )
    if(!is.null(scope))
        body$properties$scope <- scope

    api_func(op, body=body, encode="json",
        api_version=getOption("azure_rbac_api_version"), http_verb="PUT")
}

get_role_assignment <- function(id, api_func)
{
    op <- file.path("providers/Microsoft.Authorization/roleAssignments", id)
    api_func(op, api_version=getOption("azure_rbac_api_version"))
}

remove_role_assignment <- function(id, confirm, api_func)
{
    if(confirm && interactive())
    {
        yn <- readline(paste0("Do you really want to delete role assignment '", id, "'? (y/N) "))
        if(tolower(substr(yn, 1, 1)) != "y")
            return(invisible(NULL))
    }

    op <- file.path("providers/Microsoft.Authorization/roleAssignments", id)
    res <- api_func(op, api_version=getOption("azure_rbac_api_version"), http_verb="DELETE")
    if(attr(res, "status") == 204)
        warning("Role assignment not found or could not be deleted")
    invisible(NULL)
}

list_role_assignments <- function(filter, defs, api_func)
{
    op <- "providers/Microsoft.Authorization/roleAssignments"
    lst <- api_func(op, options=list(`$filter`=filter), api_version=getOption("azure_rbac_api_version"))

    role_def_names <- names(defs)
    role_def_ids <- sapply(defs, function(def) def$name)
    token <- environment(api_func)$self$token
    lapply(lst$value, function(res)
    {
        role_name <- role_def_names[basename(res$properties$roleDefinitionId) == role_def_ids]
        az_role_assignment$new(token, res, role_name)
    })
}

get_role_definition <- function(id, defs, api_func)
{
    # if text rolename supplied, get full list of roles and extract from there
    if(!is_guid(id))
    {
        i <- which(tolower(names(defs)) == tolower(id))
        if(is_empty(i))
            stop("Unknown role definition", call.=FALSE)
        id <- basename(defs[[i]]$id)
    }

    op <- file.path("providers/Microsoft.Authorization/roleDefinitions", id)
    res <- api_func(op, api_version=getOption("azure_rbac_api_version"))

    token <- environment(api_func)$self$token
    az_role_definition$new(token, res)
}

list_role_definitions <- function(filter, api_func)
{
    op <- "providers/Microsoft.Authorization/roleDefinitions"
    lst <- api_func(op, options=list(`$filter`=filter), api_version=getOption("azure_rbac_api_version"))

    token <- environment(api_func)$self$token
    lst <- lapply(lst$value, function(res)
        az_role_definition$new(token, res))
    
    names(lst) <- sapply(lst, function(res) res$properties$roleName)
    lst
}
