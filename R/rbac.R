# basic methods for RBAC: manage role assignments, retrieve role definitions

## subscription methods

az_subscription$set("public", "add_role_assignment", overwrite=TRUE,
function(principal_id, role_definition_id, scope=NULL, new_id=uuid::UUIDgenerate())
{
    role <- self$get_role_definition(role_definition_id)
    op <- file.path("providers/Microsoft.Authorization/roleAssignments", new_id)
    call_azure_rm(self$token, self$id, op, api_version=getOption("azure_rbac_api_version"),
        body=list(
            roleDefinitionId=role$id,
            principalId=principal_id,
            scope=scope
        ),
        encoding="json",
        http_verb="PUT"
    )
})

az_subscription$set("public", "get_role_assignment", overwrite=TRUE,
function() {})

az_subscription$set("public", "remove_role_assignment", overwrite=TRUE,
function() {})

az_subscription$set("public", "list_role_assignments", overwrite=TRUE,
function()
{
    op <- "providers/Microsoft.Authorization/roleAssignments"
    res <- call_azure_rm(self$token, self$id, op, api_version=getOption("azure_rbac_api_version"))
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
})

az_subscription$set("public", "get_role_definition", overwrite=TRUE,
function(id)
{
    # if text rolename supplied, get full list of roles and extract from there
    if(!is_guid(id))
    {
        defs <- self$list_role_definitions()
        id <- defs[tolower(defs$name) == tolower(id), "id"]
    }
    if(is_empty(id))
        stop("Unknown find role definition", call.=FALSE)

    op <- file.path("providers/Microsoft.Authorization/roleDefinitions", id)
    call_azure_rm(self$token, self$id, op, api_version=getOption("azure_rbac_api_version"))
})

az_subscription$set("public", "list_role_definitions", overwrite=TRUE,
function()
{
    op <- "providers/Microsoft.Authorization/roleDefinitions"
    res <- call_azure_rm(self$token, self$id, op, api_version=getOption("azure_rbac_api_version"))
    defs <- lapply(res$value, function(x)
    {
        name <- x$properties$roleName
        id <- x$name
        description <- x$properties$description
        data.frame(name, id, description, stringsAsFactors=FALSE)
    })

    do.call(rbind, defs)
})


## resource group methods

az_resource_group$set("public", "add_role_assignment", overwrite=TRUE,
function() {})

az_resource_group$set("public", "get_role_assignment", overwrite=TRUE,
function() {})

az_resource_group$set("public", "remove_role_assignment", overwrite=TRUE,
function() {})

az_resource_group$set("public", "list_role_assignments", overwrite=TRUE,
function()
{
    op <- "providers/Microsoft.Authorization/roleAssignments"
    res <- private$rg_op(op, api_version=getOption("azure_rbac_api_version"))
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
})

az_resource_group$set("public", "get_role_definition", overwrite=TRUE,
function(id)
{
    # if text rolename supplied, get full list of roles and extract from there
    if(!is_guid(id))
    {
        defs <- self$list_role_definitions()
        id <- defs[tolower(defs$name) == tolower(id), "id"]
    }
    if(is_empty(id))
        stop("Unknown find role definition", call.=FALSE)

    op <- file.path("providers/Microsoft.Authorization/roleDefinitions", id)
    private$rg_op(op, api_version=getOption("azure_rbac_api_version"))
})

az_resource_group$set("public", "list_role_definitions", overwrite=TRUE,
function()
{
    op <- "providers/Microsoft.Authorization/roleDefinitions"
    res <- private$rg_op(op, api_version=getOption("azure_rbac_api_version"))
    defs <- lapply(res$value, function(x)
    {
        name <- x$properties$roleName
        id <- x$name
        description <- x$properties$description
        data.frame(name, id, description, stringsAsFactors=FALSE)
    })

    do.call(rbind, defs)
})


## resource methods

az_resource$set("public", "add_role_assignment", overwrite=TRUE,
function() {})

az_resource$set("public", "get_role_assignment", overwrite=TRUE,
function() {})

az_resource$set("public", "remove_role_assignment", overwrite=TRUE,
function() {})

az_resource$set("public", "list_role_assignments", overwrite=TRUE,
function()
{
    op <- "providers/Microsoft.Authorization/roleAssignments"
    res <- private$res_op(op, api_version=getOption("azure_rbac_api_version"))
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
})

az_resource$set("public", "get_role_definition", overwrite=TRUE,
function(id)
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
    private$res_op(op, api_version=getOption("azure_rbac_api_version"))
})

az_resource$set("public", "list_role_definitions", overwrite=TRUE,
function()
{
    op <- "providers/Microsoft.Authorization/roleDefinitions"
    res <- private$res_op(op, api_version=getOption("azure_rbac_api_version"))
    defs <- lapply(res$value, function(x)
    {
        name <- x$properties$roleName
        id <- x$name
        description <- x$properties$description
        data.frame(name, id, description, stringsAsFactors=FALSE)
    })

    do.call(rbind, defs)
})

