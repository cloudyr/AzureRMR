#' @importFrom utils modifyList
NULL

.onLoad <- function(libname, pkgname)
{
    azure_api_version="2018-05-01"
    options(azure_api_version=azure_api_version)

    config_dir <- config_dir()
    if(!dir.exists(config_dir))
        dir.create(config_dir, recursive=TRUE)

    rds <- dir(path=config_dir)

    lapply(rds, function(tenant)
    {
        assign(tenant, readRDS(file.path(config_dir, tenant)), envir=logins)
        NULL
    })

    invisible(NULL)
}

