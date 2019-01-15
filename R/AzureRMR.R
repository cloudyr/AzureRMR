#' @importFrom utils modifyList
NULL

.onLoad <- function(libname, pkgname)
{
    azure_api_version="2018-05-01"
    options(azure_api_version=azure_api_version)

    AzureRMR_dir <- AzureRMR_dir()
    if(!dir.exists(AzureRMR_dir))
        dir.create(AzureRMR_dir, recursive=TRUE)

    invisible(NULL)
}


#' Data directory for AzureRMR
#'
#' @details
#' AzureRMR stores authentication credentials and OAuth tokens in a user-specific directory, using the rappdirs package. On recent Windows versions, this will usually be in the location `C:\\Users\\(username)\\AppData\\Local\\AzureR\\AzureRMR`. On Linux, it will be in `~/.local/share/AzureRMR`, and on MacOS, it will be in `~/Library/Application Support/AzureRMR`. The working directory is not touched (which significantly lessens the risk of accidentally introducing cached tokens into source control).
#'
#' @return
#' A string containing the data directory.
#'
#' @seealso
#' [get_azure_token], [get_azure_login]
#'
#' @export
AzureRMR_dir <- function()
{
    rappdirs::user_data_dir(appname="AzureRMR", appauthor="AzureR", roaming=FALSE)
}
