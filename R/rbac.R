#' Role-based access control
#'
#' Basic methods for RBAC: manage role assignments, retrieve role definitions.
#'
#' @aliases rbac
#' @aliases add_role_assignment get_role_assignment remove_role_assignment list_role_assignments
#' @aliases get_role_definition list_role_definitions
#' @rdname rbac
NULL

## subscription methods

az_subscription$set("public", "add_role_assignment", overwrite=TRUE,
function(principal, role, scope=NULL, new_id=uuid::UUIDgenerate())
{
    if(!is_role_definition(role))
        role <- self$get_role_definition(role)
    add_role_assignment(principal, role, scope, new_id, private$sub_op)
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
function(principal, role, scope=NULL, new_id=uuid::UUIDgenerate())
{
    if(!is_role_definition(role))
        role <- self$get_role_definition(role)
    add_role_assignment(principal, role, scope, new_id, private$rg_op)
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
function(principal, role, scope=NULL, new_id=uuid::UUIDgenerate())
{
    if(!is_role_definition(role))
        role <- self$get_role_definition(role)
    add_role_assignment(principal, role, scope, new_id, private$res_op)
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
    res <- list(id=id)
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
