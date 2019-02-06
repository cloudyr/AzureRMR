#' Informational functions
#'
#' These functions return whether the object is of the corresponding AzureRMR class.
#'
#' @param object An R object.
#'
#' @return
#' A boolean.
#' @rdname is
#' @export
is_azure_login <- function(object)
{
    R6::is.R6(object) && inherits(object, "az_rm")
}


#' @rdname is
#' @export
is_subscription <- function(object)
{
    R6::is.R6(object) && inherits(object, "az_subscription")
}


#' @rdname is
#' @export
is_resource_group <- function(object)
{
    R6::is.R6(object) && inherits(object, "az_resource_group")
}


#' @rdname is
#' @export
is_resource <- function(object)
{
    R6::is.R6(object) && inherits(object, "az_resource")
}


#' @rdname is
#' @export
is_template <- function(object)
{
    R6::is.R6(object) && inherits(object, "az_template")
}

