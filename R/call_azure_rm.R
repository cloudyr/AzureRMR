#' @export
call_azure_rm <- function(token, subscription, operation, ..., api_version=getOption("azure_api_version"))
{
    url <- httr::parse_url(token$credentials$resource)
    url$path <- file.path("subscriptions", subscription, operation, fsep="/")
    url$query <- list(`api-version`=api_version)

    call_azure_url(token, httr::build_url(url), ...)
}


#' @export
call_azure_url <- function(token, url, ...,
                           http_verb=c("GET", "DELETE", "PUT", "POST", "HEAD"),
                           http_status_handler=c("stop", "warn", "message", "pass"),
                           auto_refresh=TRUE)
{
    headers <- process_headers(token, ..., auto_refresh=auto_refresh)
    verb <- get(match.arg(http_verb), getNamespace("httr"))

    # do actual API call
    res <- verb(url, headers, ...)

    process_response(res, match.arg(http_status_handler))
}


process_headers <- function(token, ..., auto_refresh)
{
    # if token has expired, renew it
    if(auto_refresh && !token$validate())
    {
        message("Access token has expired or is no longer valid; refreshing")
        token$refresh()
    }

    creds <- token$credentials
    host <- httr::parse_url(creds$resource)$host
    headers <- c(Host=host, Authorization=paste(creds$token_type, creds$access_token))

    # default content-type is json, set this if encoding not specified
    dots <- list(...)
    if(is_empty(dots) || !("encode" %in% names(dots)))
        headers <- c(headers, `Content-type`="application/json")

    httr::add_headers(.headers=headers)
}


process_response <- function(response, handler)
{
    if(handler != "pass")
    {
        handler <- get(paste0(handler, "_for_status"), getNamespace("httr"))
        handler(response, paste0("complete Resource Manager operation. Message:\n",
                                 sub("\\.$", "", arm_error_message(response))))
        cont <- httr::content(response)
        if(is.null(cont))
            cont <- list()
        attr(cont, "status") <- httr::status_code(response)
        cont
    }
    else response
}


# provide complete error messages from Resource Manager
arm_error_message <- function(response)
{
    cont <- httr::content(response)
    paste0(strwrap(cont$error$message), collapse="\n")
}

