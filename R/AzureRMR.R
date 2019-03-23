#' @import AzureAuth
#' @importFrom utils modifyList
NULL

utils::globalVariables(c("self", "private"))

.onLoad <- function(libname, pkgname)
{
    options(azure_api_version="2018-05-01")
    options(azure_api_mgmt_version="2016-09-01")
    options(azure_roledef_api_version="2018-01-01-preview")
    options(azure_roleasn_api_version="2018-12-01-preview")

    make_AzureR_dir()

    invisible(NULL)
}


# default authentication app ID: leverage the az CLI
.az_cli_app_id <- "04b07795-8ddb-461a-bbee-02f9e1bf7b46"

