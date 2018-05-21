#' Format an Azure object
#'
#' Miscellaneous functions for printing Azure R6 objects
#'
#' @param token An Azure OAuth token.
#' @param env An R6 object's environment for printing.
#' @param exclude Objects in `env` to exclude from the printout.
#'
#' @details
#' These functions are utilities to aid in printing Azure R6 objects. They are not meant to be called by the user.
#'
#' @rdname format
#' @export
format_auth_header <- function(token)
{
    expiry <- as.POSIXct(as.numeric(token$credentials$expires_on), origin="1970-01-01")
    obtained <- expiry - as.numeric(token$credentials$expires_in)
    host <- token$credentials$resource
    tenant <- sub("/.+$", "", httr::parse_url(token$endpoint$access)$path)

    paste0("  Authentication details:\n",
           "    host: ", host, "\n",
           "    tenant: ", tenant, "\n",
           "    token obtained: ", format(obtained, usetz=TRUE), "\n",
           "    token valid to: ", format(expiry, usetz=TRUE), "\n",
           "---\n")
}


#' @rdname format
#' @export
format_public_fields <- function(env, exclude=character(0))
{
    objnames <- ls(env)
    std_fields <- "token"
    objnames <- setdiff(objnames, c(exclude, std_fields))
    is_method <- sapply(objnames, function(obj) is.function(.subset2(env, obj)))

    maxwidth <- as.integer(0.8 * getOption("width"))

    objnames <- objnames[!is_method]
    objconts <- sapply(objnames, function(n)
    {
        x <- get(n, env)
        deparsed <- if(is.list(x))
            paste0("list(", paste(names(x), collapse=", "), ")")
        else if(is.vector(x))
        {
            x <- paste0(x, collapse=", ")
            if(nchar(x) > maxwidth - nchar(n) - 10)
                x <- paste0(substr(x, 1, maxwidth - nchar(n) - 10), " ...")
            x
        }            
        else deparse(x)[[1]]

        paste0(strwrap(paste0(n, ": ", deparsed), width=maxwidth, indent=2, exdent=4),
               collapse="\n")
    })
    paste0(paste0(objconts, collapse="\n"), "\n---\n")
}


#' @rdname format
#' @export
format_public_methods <- function(env)
{
    objnames <- ls(env)
    std_methods <- c("clone", "print", "initialize")
    objnames <- setdiff(objnames, std_methods)
    is_method <- sapply(objnames, function(obj) is.function(.subset2(env, obj)))

    maxwidth <- as.integer(0.8 * getOption("width"))

    objnames <- strwrap(paste(objnames[is_method], collapse=", "), width=maxwidth, indent=4, exdent=4)
    paste0("  Methods:\n", paste0(objnames, collapse="\n"), "\n")
}
