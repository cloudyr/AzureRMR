call_azure_sm <- function(token, subscription, operation, api_version,
                          http_verb=c("GET", "DELETE", "PUT", "POST", "HEAD"),
                          catch=c("stop", "warn", "message", "pass"), ...)
{
    creds <- token$credentials

    url <- httr::parse_url(creds$resource)
    url$path <- file.path("subscriptions", subscription, operation, fsep="/")
    url$query <- list(`api-version`=api_version)
    headers <- httr::add_headers(Host=url$host,
                                 Authorization=paste(creds$token_type, creds$access_token),
                                 `Content-type`="application/json")

    verb <- get(match.arg(http_verb), getNamespace("httr"))
    res <- verb(httr::build_url(url), headers, ...)

    catch <- match.arg(catch)
    if(catch != "pass")
    {
        catch <- get(paste0(match.arg(catch), "_for_status"), getNamespace("httr"))
        catch(res)
    }
    httr::content(res, as="parsed")
}
