.onLoad <- function(libname, pkgname)
{
    azure_api_version="2018-02-01"
    options(azure_api_version=azure_api_version)
    invisible(NULL)
}
