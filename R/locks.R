#' Management locks
#'
#' Create, retrieve and delete locks. These are methods for the `az_subscription`, `az_resource_group` and `az_resource` classes.
#'
#' @section Usage:
#' ```
#' create_lock(name, level = c("cannotdelete", "readonly"), notes = "")
#'
#' get_lock(name)
#'
#' delete_lock(name)
#'
#' list_locks()
#' ```
#' @section Arguments:
#' - `name`: The name of a lock.
#' - `level`: The level of protection that the lock provides.
#' - `notes`: An optional character string to describe the lock.
#'
#' @section Details:
#' Management locks in Resource Manager can be assigned at the subscription, resource group, or resource level. They serve to protect a resource against unwanted changes. A lock can either protect against deletion (`level="cannotdelete"`) or against modification of any kind (`level="readonly"`).
#'
#' Locks assigned at parent scopes also apply to lower ones, recursively. The most restrictive lock in the inheritance takes precedence. To modify/delete a resource, any existing locks for its subscription and resource group must also be removed.
#'
#' Note if you logged in via a custom service principal, it must have "Owner" or "User Access Administrator" access to manage locks.
#'
#' @section Value:
#' The `create_lock` and `get_lock` methods return a lock object, which is itself an Azure resource. The `list_locks` method returns a list of such objects. The `delete_lock` method returns NULL on a successful delete.
#'
#' The `get_role_definition` method returns an object of class `az_role_definition`. This is a plain-old-data R6 class (no methods), which can be used as input for creating role assignments (see the examples below).
#'
#' The `list_role_definitions` method returns a list of `az_role_definition` if the `as_data_frame` argument is FALSE. If this is TRUE, it instead returns a data frame containing the most broadly useful fields for each role definition: the definition ID and role name.
#'
#' @seealso
#' [rbac]
#'
#' [Overview of management locks](https://learn.microsoft.com/en-us/azure/azure-resource-manager/resource-group-lock-resources)
#'
#' @examples
#' \dontrun{
#'
#' az <- get_azure_login("myaadtenant")
#' sub <- az$get_subscription("subscription_id")
#' rg <- sub$get_resource_group("rgname")
#' res <- rg$get_resource(type="provider_type", name="resname")
#'
#' sub$create_lock("lock1", "cannotdelete")
#' rg$create_lock("lock2", "cannotdelete")
#'
#' # error! resource is locked
#' res$delete()
#'
#' # subscription level
#' rg$delete_lock("lock2")
#' sub$delete_lock("lock1")
#'
#' # now it works
#' res$delete()
#'
#' }
#' @aliases lock create_lock get_lock delete_lock list_locks
#' @rdname lock
#' @name lock
NULL

## subscription methods

az_subscription$set("public", "create_lock", overwrite=TRUE,
function(name, level=c("cannotdelete", "readonly"), notes="")
{
    create_lock(name, match.arg(level), notes, private$sub_op, self$token, self$id)
})

az_subscription$set("public", "get_lock", overwrite=TRUE,
function(name)
{
    get_lock(name, private$sub_op, self$token, self$id)
})

az_subscription$set("public", "delete_lock", overwrite=TRUE,
function(name)
{
    delete_lock(name, private$sub_op)
})

az_subscription$set("public", "list_locks", overwrite=TRUE,
function()
{
    list_locks(private$sub_op, self$token, self$id)
})


## resource group methods

az_resource_group$set("public", "create_lock", overwrite=TRUE,
function(name, level=c("cannotdelete", "readonly"), notes="")
{
    create_lock(name, match.arg(level), notes, private$rg_op, self$token, self$subscription)
})

az_resource_group$set("public", "get_lock", overwrite=TRUE,
function(name)
{
    get_lock(name, private$rg_op, self$token, self$subscription)
})

az_resource_group$set("public", "delete_lock", overwrite=TRUE,
function(name)
{
    delete_lock(name, private$rg_op)
})

az_resource_group$set("public", "list_locks", overwrite=TRUE,
function()
{
    list_locks(private$rg_op, self$token, self$subscription)
})


## resource methods

az_resource$set("public", "create_lock", overwrite=TRUE,
function(name, level=c("cannotdelete", "readonly"), notes="")
{
    create_lock(name, match.arg(level), notes, private$res_op, self$token, self$subscription)
})

az_resource$set("public", "get_lock", overwrite=TRUE,
function(name)
{
    get_lock(name, private$res_op, self$token, self$subscription)
})

az_resource$set("public", "delete_lock", overwrite=TRUE,
function(name)
{
    delete_lock(name, private$res_op)
})

az_resource$set("public", "list_locks", overwrite=TRUE,
function()
{
    list_locks(private$res_op, self$token, self$subscription)
})



## implementations

create_lock <- function(name, level, notes, api_func, token, subscription)
{
    api <- getOption("azure_api_mgmt_version")
    op <- file.path("providers/Microsoft.Authorization/locks", name)
    body <- list(properties=list(level=level))
    if(notes != "")
        body$notes <- notes

    res <- api_func(op, body=body, encode="json", http_verb="PUT", api_version=api)
    az_resource$new(token, subscription, deployed_properties=res, api_version=api)
}

get_lock <- function(name, api_func, token, subscription)
{
    api <- getOption("azure_api_mgmt_version")
    op <- file.path("providers/Microsoft.Authorization/locks", name)
    res <- api_func(op, api_version=api)
    az_resource$new(token, subscription, deployed_properties=res, api_version=api)
}

delete_lock <- function(name, api_func)
{
    api <- getOption("azure_api_mgmt_version")
    op <- file.path("providers/Microsoft.Authorization/locks", name)
    api_func(op, http_verb="DELETE", api_version=api)
    invisible(NULL)
}

list_locks <- function(api_func, token, subscription)
{
    api <- getOption("azure_api_mgmt_version")
    op <- "providers/Microsoft.Authorization/locks"
    cont <- api_func(op, api_version=api)
    lst <- lapply(get_paged_list(cont, token), function(parms)
        az_resource$new(token, subscription, deployed_properties=parms, api_version=api))

    names(lst) <- sapply(lst, function(x) sub("^.+providers/(.+$)", "\\1", x$id))
    lst
}
