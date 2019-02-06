#' @import AzureAuth
#' @importFrom utils modifyList
NULL


.onLoad <- function(libname, pkgname)
{
    azure_api_version="2018-05-01"
    options(azure_api_version=azure_api_version)

    make_AzureRMR_dir()

    invisible(NULL)
}


# default authentication app ID: leverage the az CLI
.az_cli_app_id <- "04b07795-8ddb-461a-bbee-02f9e1bf7b46"


# create a directory for saving creds -- ask first, to satisfy CRAN requirements
make_AzureRMR_dir <- function()
{
    AzureRMR_dir <- AzureRMR_dir()
    if(!dir.exists(AzureRMR_dir) && interactive())
    {
        yn <- readline(paste0(
                "AzureRMR can cache Azure Resource Manager logins in the directory:\n\n",
                AzureRMR_dir, "\n\n",
                "This saves you having to re-authenticate with Azure in future sessions. Create this directory? (Y/n) "))
        if(tolower(substr(yn, 1, 1)) == "n")
            return(invisible(NULL))

        dir.create(AzureRMR_dir, recursive=TRUE)
    }
}


#' Data directory for AzureRMR
#'
#' @details
#' AzureRMR can store authentication credentials and OAuth tokens in a user-specific directory, using the rappdirs package. On recent Windows versions, this will usually be in the location `C:\\Users\\(username)\\AppData\\Local\\AzureR\\AzureRMR`. On Unix/Linux, it will be in `~/.local/share/AzureRMR`, and on MacOS, it will be in `~/Library/Application Support/AzureRMR`. The working directory is not touched (which significantly lessens the risk of accidentally introducing cached tokens into source control).
#'
#' On package startup, if this directory does not exist, AzureRMR will prompt you for permission to create it. It's recommended that you allow the directory to be created, as otherwise you will have to reauthenticate with Azure every time. Note that many cloud engineering tools, including the [Azure CLI](https://docs.microsoft.com/en-us/cli/azure/?view=azure-cli-latest), save authentication credentials in this way.
#'
#' @return
#' A string containing the data directory.
#'
#' @seealso
#' [get_azure_token], [get_azure_login]
#'
#' [rappdirs::user_data_dir]
#'
#' @export
AzureRMR_dir <- function()
{
    rappdirs::user_data_dir(appname="AzureRMR", appauthor="AzureR", roaming=FALSE)
}
