#' Call the Azure Resource Manager REST API
#'
#' @param token An Azure OAuth token, of class [AzureToken].
#' @param subscription A subscription ID.
#' @param operation The operation to perform, which will form part of the URL path.
#' @param options A named list giving the URL query parameters.
#' @param api_version The API version to use, which will form part of the URL sent to the host.
#' @param url A complete URL to send to the host.
#' @param http_verb The HTTP verb as a string, one of `GET`, `PUT`, `POST`, `DELETE`, `HEAD` or `PATCH`.
#' @param http_status_handler How to handle in R the HTTP status code of a response. `"stop"`, `"warn"` or `"message"` will call the appropriate handlers in httr, while `"pass"` ignores the status code.
#' @param auto_refresh Whether to refresh/renew the OAuth token if it is no longer valid.
#' @param ... Other arguments passed to lower-level code, ultimately to the appropriate functions in httr.
#'
#' @details
#' These functions form the low-level interface between AzureRMR and Resource Manager. `call_azure_rm` builds a URL from its arguments and passes it to `call_azure_url`. Authentication is handled automatically.
#'
#' @return
#' If `http_status_handler` is one of `"stop"`, `"warn"` or `"message"`, the status code of the response is checked. If an error is not thrown, the parsed content of the response is returned with the status code attached as the "status" attribute.
#'
#' If `http_status_handler` is `"pass"`, the entire response is returned without modification.
#'
#' @seealso
#' [httr::GET], [httr::PUT], [httr::POST], [httr::DELETE], [httr::stop_for_status], [httr::content]
#' @rdname call_azure
#' @export
call_azure_rm <- function(token, subscription, operation, ...,
                          options=list(),
                          api_version=getOption("azure_api_version"))
{
    url <- httr::parse_url(token$credentials$resource)
    url$path <- construct_path("subscriptions", subscription, operation)
    url$query <- modifyList(list(`api-version`=api_version), options)

    call_azure_url(token, httr::build_url(url), ...)
}


#' @rdname call_azure
#' @export
call_azure_url <- function(token, url, ...,
                           http_verb=c("GET", "DELETE", "PUT", "POST", "HEAD", "PATCH"),
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

    # kiboze through possible message locations
    msg <- if(is.character(cont))
        cont
    else if(is.list(cont) && is.character(cont$message))
        cont$message
    else if(is.list(cont) && is.list(cont$error) && is.character(cont$error$message))
        cont$error$message
    else ""

    paste0(strwrap(msg), collapse="\n")
}

