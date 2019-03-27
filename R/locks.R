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

delete_lock=function(name, api_func)
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
