#' @export
call_azure_sm <- function(token, subscription, operation, ...,
                          http_verb=c("GET", "DELETE", "PUT", "POST", "HEAD"),
                          http_condition_handler=c("stop", "warn", "message", "pass"),
                          api_version=getOption("azure_api_version"),
                          auto_refresh=TRUE)
{
    # if token has expired, renew it
    if(auto_refresh && !token$validate())
    {
        message("Access token has expired or is no longer valid; refreshing")
        token$refresh()
    }

    creds <- token$credentials

    url <- httr::parse_url(creds$resource)
    url$path <- file.path("subscriptions", subscription, operation, fsep="/")
    url$query <- list(`api-version`=api_version)
    headers <- httr::add_headers(Host=url$host,
                                 Authorization=paste(creds$token_type, creds$access_token),
                                 `Content-type`="application/json")

    verb <- get(match.arg(http_verb), getNamespace("httr"))

    # do actual API call
    res <- verb(httr::build_url(url), headers, ...)

    catch <- match.arg(http_condition_handler)
    if(catch != "pass")
    {
        catch <- get(paste0(catch, "_for_status"), getNamespace("httr"))
        catch(res)
    }
    httr::content(res, as="parsed")
}
