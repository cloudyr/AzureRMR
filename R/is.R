#' @export
is_subscription <- function(object)
{
    R6::is.R6(object) && inherits(object, "az_subscription")
}


#' @export
is_resource_group <- function(object)
{
    R6::is.R6(object) && inherits(object, "az_resource_group")
}


#' @export
is_resource <- function(object)
{
    R6::is.R6(object) && inherits(object, "az_resource")
}


#' @export
is_template <- function(object)
{
    R6::is.R6(object) && inherits(object, "az_template")
}

