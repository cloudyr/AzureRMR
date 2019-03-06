#' @param tenant For `call_azure_graph`, an Azure Active Directory tenant. Can be a GUID, a domain name, or "myorganization" to use the tenant of the logged-in user.
#' @rdname call_azure
#' @export
call_azure_graph <- function(token, tenant="myorganization", operation, ...,
                             options=list(),
                             api_version=getOption("azure_graph_api_version"))
{
    url <- httr::parse_url(token$credentials$resource)
    url$path <- construct_path(tenant, operation)
    url$query <- modifyList(list(`api-version`=api_version), options)

    call_azure_url(token, httr::build_url(url), ...)
}

