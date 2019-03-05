call_azure_graph <- function(token, tenant, operation, ...,
                           options=list(),
                           api_version=getOption("azure_graph_api_version"))
{
    url <- httr::parse_url(token$credentials$resource)
    url$path <- construct_path(tenant, operation)
    url$query <- modifyList(list(`api-version`=api_version), options)

    call_azure_url(token, httr::build_url(url), ...)
}

