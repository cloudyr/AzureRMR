#' @importFrom utils modifyList
NULL

.onLoad <- function(libname, pkgname)
{
    azure_api_version="2018-05-01"
    options(azure_api_version=azure_api_version)

    config_dir <- config_dir()
    if(!dir.exists(config_dir))
        dir.create(config_dir, recursive=TRUE)

    invisible(NULL)
}


config_dir <- function()
{
    rappdirs::user_config_dir(appname="AzureRMR", appauthor="AzureR", roaming=FALSE)
}
