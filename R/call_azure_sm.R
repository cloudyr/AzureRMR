#' @export
call_azure_rm <- function(token, subscription, operation, ...,
                          http_verb=c("GET", "DELETE", "PUT", "POST", "HEAD"),
                          http_status_handler=c("stop", "warn", "message", "pass"),
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

    headers <- c(Host=url$host, Authorization=paste(creds$token_type, creds$access_token))

    # default content-type is json, set this if encoding not specified
    dots <- list(...)
    if(is_empty(dots) || !("encode" %in% names(dots)))
        headers <- c(headers, `Content-type`="application/json")

    headers <- httr::add_headers(.headers=headers)
    verb <- get(match.arg(http_verb), getNamespace("httr"))

    # do actual API call
    res <- verb(httr::build_url(url), headers, ...)

    catch <- match.arg(http_status_handler)
    if(catch != "pass")
    {
        catch <- get(paste0(catch, "_for_status"), getNamespace("httr"))
        catch(res)
        httr::content(res)
    }
    else res
}


# TRUE for NULL and length-0 objects
is_empty <- function(x)
{
    length(x) == 0
}


# check that 1) all required names are present; 2) optional names may be present; 3) no other names are present
validate_object_names <- function(x, required, optional)
{
    valid <- all(required %in% x) && all(x %in% c(required, optional))
    if(!valid)
        stop("Invalid object names")
}


# set names on a list of objects, given each object contains its name field
named_list <- function(lst, name_field="name")
{
    names(lst) <- sapply(lst, `[[`, name_field)
    dups <- duplicated(tolower(names(lst)))
    if(any(dups))
    {
        duped_names <- names(lst)[dups]
        warning("Some names are duplicated: ", paste(duped_names, collapse=" "), call.=FALSE)
    }
    lst
}
