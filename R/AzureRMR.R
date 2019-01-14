#' @importFrom utils modifyList
NULL

.onLoad <- function(libname, pkgname)
{
    azure_api_version="2018-05-01"
    options(azure_api_version=azure_api_version)

    config_dir <- config_dir()
    if(!dir.exists(config_dir))
        dir.create(config_dir, recursive=TRUE)

    arm_logins <- file.path(config_dir(), "arm_logins.json")
    if(!file.exists(arm_logins))
        writeLines(jsonlite::toJSON(structure(list(), names=character(0))), arm_logins)

    invisible(NULL)
}


config_dir <- function()
{
    rappdirs::user_config_dir(appname="AzureRMR", appauthor="AzureR", roaming=FALSE)
}
