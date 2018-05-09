# check that 1) all required names are present; 2) optional names may be present; 3) no other names are present
validate_object_names <- function(x, required, optional=character(0))
{
    valid <- all(required %in% x) && all(x %in% c(required, optional))
    if(!valid)
        stop("Invalid object names")
}


# set names on a list of objects, where each object contains its name field(s)
named_list <- function(lst, name_fields="name")
{
    name_fields <- sapply(name_fields, function(n) sapply(lst, `[[`, n))
    name_fields <- apply(name_fields, 1, function(nn) paste(nn, collapse="/"))

    names(lst) <- name_fields
    dups <- duplicated(tolower(names(lst)))
    if(any(dups))
    {
        duped_names <- names(lst)[dups]
        warning("Some names are duplicated: ", paste(unique(duped_names), collapse=" "), call.=FALSE)
    }
    lst
}


# check if a string appears to be a URL (only https allowed)
is_url=function(x)
{
    is.character(x) && length(x) == 1 && grepl("^https://", x)
}


# TRUE for NULL and length-0 objects
is_empty <- function(x)
{
    length(x) == 0
}
