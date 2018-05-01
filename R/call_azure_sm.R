call_azure_sm <- function(token, subscription, operation, api_version,
                          http_verb=c("GET", "DELETE", "PUT", "POST", "HEAD"), ...)
{
    creds <- token$credentials

    url <- httr::parse_url(creds$resource)
    url$path <- file.path("subscriptions", subscription, operation, fsep="/")
    url$query <- list(`api-version`=api_version)
    headers <- httr::add_headers(Host=url$host,
                                 Authorization=paste(creds$token_type, creds$access_token),
                                 `Content-type`="application/json")

    verb <- get(match.arg(http_verb), getNamespace("httr"))
    verb(httr::build_url(url), headers, ...)
}
