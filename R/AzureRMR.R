#' @import AzureAuth
#' @importFrom utils modifyList
NULL


.onLoad <- function(libname, pkgname)
{
    azure_api_version <- "2018-05-01"
    azure_api_mgmt_version <- "2016-09-01"
    graph_api_version <- "1.6"

    options(azure_api_version=azure_api_version)
    options(azure_api_mgmt_version=azure_api_mgmt_version)

    make_AzureR_dir()

    invisible(NULL)
}


# default authentication app ID: leverage the az CLI
.az_cli_app_id <- "04b07795-8ddb-461a-bbee-02f9e1bf7b46"


# create a directory for saving creds -- ask first, to satisfy CRAN requirements
make_AzureR_dir <- function()
{
    AzureR_dir <- AzureR_dir()
    if(!dir.exists(AzureR_dir) && interactive())
    {
        yn <- readline(paste0(
                "AzureRMR can cache Azure Resource Manager logins in the directory:\n\n",
                AzureR_dir, "\n\n",
                "This saves you having to re-authenticate with Azure in future sessions. Create this directory? (Y/n) "))
        if(tolower(substr(yn, 1, 1)) == "n")
            return(invisible(NULL))

        dir.create(AzureR_dir, recursive=TRUE)
    }
}
