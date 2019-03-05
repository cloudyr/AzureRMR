# basic methods for RBAC: manage role assignments, retrieve role definitions

## subscription methods

az_subscription$set("public", "add_role_assignment", overwrite=TRUE,
function(principal, role, scope=NULL, new_id=uuid::UUIDgenerate())
{
    add_role_assignment(principal, role, scope, new_id, private$sub_op)
})

az_subscription$set("public", "get_role_assignment", overwrite=TRUE,
function(id)
{
    get_role_assignment(id, private$sub_op)
})

az_subscription$set("public", "remove_role_assignment", overwrite=TRUE,
function(id, confirm=TRUE)
{
    remove_role_assignment(id, confirm, private$sub_op)
})

az_subscription$set("public", "list_role_assignments", overwrite=TRUE,
function()
{
    list_role_assignments(private$sub_op)
})

az_subscription$set("public", "get_role_definition", overwrite=TRUE,
function(id)
{
    get_role_definition(id, private$sub_op)
})

az_subscription$set("public", "list_role_definitions", overwrite=TRUE,
function()
{
    list_role_definitions(private$sub_op)
})


## resource group methods

az_resource_group$set("public", "add_role_assignment", overwrite=TRUE,
function(principal, role, scope=NULL, new_id=uuid::UUIDgenerate())
{
    add_role_assignment(principal, role, scope, new_id, private$rg_op)
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
function()
{
    list_role_assignments(private$rg_op)
})

az_resource_group$set("public", "get_role_definition", overwrite=TRUE,
function(id)
{
    get_role_definition(id, private$rg_op)
})

az_resource_group$set("public", "list_role_definitions", overwrite=TRUE,
function()
{
    list_role_definitions(private$rg_op)
})


## resource methods

az_resource$set("public", "add_role_assignment", overwrite=TRUE,
function(principal, role, scope=NULL, new_id=uuid::UUIDgenerate())
{
    add_role_assignment(principal, role, scope, new_id, private$res_op)
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
function()
{
    list_role_assignments(private$res_op)
})

az_resource$set("public", "get_role_definition", overwrite=TRUE,
function(id)
{
    get_role_definition(id, private$res_op)
})

az_resource$set("public", "list_role_definitions", overwrite=TRUE,
function()
{
    list_role_definitions(private$res_op)
})


## implementations ("static methods")

add_role_assignment <- function(principal, role, scope, new_id, api_func)
{
    role <- self$get_role_definition(role)
    op <- file.path("providers/Microsoft.Authorization/roleAssignments", new_id)
    api_func(op, api_version=getOption("azure_rbac_api_version"),
        body=list(
            roleDefinitionId=role$id,
            principalId=principal,
            scope=scope
        ),
        encoding="json",
        http_verb="PUT"
    )
}

get_role_assignment <- function(id)
{}

remove_role_assignment <- function(id, confirm, api_func)
{}

list_role_assignments <- function(api_func)
{
    op <- "providers/Microsoft.Authorization/roleAssignments"
    res <- api_func(op, api_version=getOption("azure_rbac_api_version"))
    defs <- self$list_role_definitions()

    roles <- lapply(res$value, function(x)
    {
        role_id <- x$name
        principal <- x$properties$principalId
        role_def_name <- defs$name[defs$id == basename(x$properties$roleDefinitionId)]
        scope <- x$properties$scope
        data.frame(id=role_id, principal, role=role_def_name, scope, stringsAsFactors=FALSE)
    })

    do.call(rbind, roles)
}

get_role_definition <- function(id, api_func)
{
    # if text rolename supplied, get full list of roles and extract from there
    if(!is_guid(id))
    {
        defs <- self$list_role_definitions()
        id <- defs[tolower(defs$name) == tolower(id), "id"]
    }
    if(is_empty(id))
        stop("Unknown role definition", call.=FALSE)

    op <- file.path("providers/Microsoft.Authorization/roleDefinitions", id)
    api_func(op, api_version=getOption("azure_rbac_api_version"))
}

list_role_definitions <- function(api_func)
{
    op <- "providers/Microsoft.Authorization/roleDefinitions"
    res <- api_func(op, api_version=getOption("azure_rbac_api_version"))
    defs <- lapply(res$value, function(x)
    {
        name <- x$properties$roleName
        id <- x$name
        description <- x$properties$description
        data.frame(name, id, description, stringsAsFactors=FALSE)
    })

    do.call(rbind, defs)
}
