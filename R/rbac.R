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
    get_role_assignment(id, self$list_role_definitions(), private$sub_op)
})

az_subscription$set("public", "remove_role_assignment", overwrite=TRUE,
function(role, confirm=TRUE)
{
    remove_role_assignment(role, confirm)
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
    get_role_assignment(id, self$list_role_definitions(), private$rg_op)
})

az_resource_group$set("public", "remove_role_assignment", overwrite=TRUE,
function(role, confirm=TRUE)
{
    remove_role_assignment(role, confirm)
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
    get_role_assignment(id, self$list_role_definitions(), private$res_op)
})

az_resource$set("public", "remove_role_assignment", overwrite=TRUE,
function(role, confirm=TRUE)
{
    remove_role_assignment(role, confirm)
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

    token <- environment(api_func)$self$token
    op <- file.path("providers/Microsoft.Authorization/roleAssignments", new_id)
    body <- list(
        properties=list(
            roleDefinitionId=role$id,
            principalId=principal
        )
    )
    if(!is.null(scope))
        body$properties$scope <- scope

    print(body)

    res <- api_func(op, body=body, encode="json",
        api_version=getOption("azure_rbac_api_version"), http_verb="PUT")
    az_role_assignment$new(token, res, role$properties$roleName, api_func)
}

get_role_assignment <- function(id, defs, api_func)
{
    token <- environment(api_func)$self$token
    op <- file.path("providers/Microsoft.Authorization/roleAssignments", id)
    res <- api_func(op, api_version=getOption("azure_rbac_api_version"))

    role_def_names <- names(defs)
    role_def_ids <- sapply(defs, function(def) def$name)
    role_name <- role_def_names[basename(res$properties$roleDefinitionId) == role_def_ids]

    az_role_assignment$new(token, res, role_name, api_func)
}

remove_role_assignment <- function(role, confirm)
{
    role$remove(confirm=confirm)
}

list_role_assignments <- function(filter, defs, api_func)
{
    token <- environment(api_func)$self$token
    op <- "providers/Microsoft.Authorization/roleAssignments"
    lst <- api_func(op, options=list(`$filter`=filter), api_version=getOption("azure_rbac_api_version"))

    role_def_names <- names(defs)
    role_def_ids <- sapply(defs, function(def) def$name)
    lapply(lst$value, function(res)
    {
        role_name <- role_def_names[basename(res$properties$roleDefinitionId) == role_def_ids]
        az_role_assignment$new(token, res, role_name, api_func)
    })
}

get_role_definition <- function(id, defs, api_func)
{
    if(is_role_definition(id))
        return(id)

    # if text rolename supplied, get full list of roles and extract from there
    if(!is_guid(id))
    {
        i <- which(tolower(names(defs)) == tolower(id))
        if(is_empty(i))
            stop("Unknown role definition", call.=FALSE)
        id <- basename(defs[[i]]$id)
    }

    token <- environment(api_func)$self$token
    op <- file.path("providers/Microsoft.Authorization/roleDefinitions", id)
    res <- api_func(op, api_version=getOption("azure_rbac_api_version"))
    az_role_definition$new(token, res)
}

list_role_definitions <- function(filter, api_func)
{
    token <- environment(api_func)$self$token
    op <- "providers/Microsoft.Authorization/roleDefinitions"
    lst <- api_func(op, options=list(`$filter`=filter), api_version=getOption("azure_rbac_api_version"))

    lst <- lapply(lst$value, function(res)
        az_role_definition$new(token, res))
    
    names(lst) <- sapply(lst, function(res) res$properties$roleName)
    lst
}
