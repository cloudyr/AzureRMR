#' Informational functions
#'
#' These functions return whether the object is of the corresponding AzureRMR class.
#'
#' @param object An R object.
#'
#' @return
#' A boolean.
#' @rdname info
#' @export
is_azure_login <- function(object)
{
    R6::is.R6(object) && inherits(object, "az_rm")
}


#' @rdname info
#' @export
is_subscription <- function(object)
{
    R6::is.R6(object) && inherits(object, "az_subscription")
}


#' @rdname info
#' @export
is_resource_group <- function(object)
{
    R6::is.R6(object) && inherits(object, "az_resource_group")
}


#' @rdname info
#' @export
is_resource <- function(object)
{
    R6::is.R6(object) && inherits(object, "az_resource")
}


#' @rdname info
#' @export
is_template <- function(object)
{
    R6::is.R6(object) && inherits(object, "az_template")
}


#' @rdname info
#' @export
is_role_definition <- function(object)
{
    R6::is.R6(object) && inherits(object, "az_role_definition")
}


#' @rdname info
#' @export
is_role_assignment <- function(object)
{
    R6::is.R6(object) && inherits(object, "az_role_assignment")
}
