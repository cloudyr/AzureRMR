% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/az_role.R
\docType{class}
\name{az_role_assignment}
\alias{az_role_assignment}
\title{Azure role assignment class}
\format{
An R6 object of class \code{az_role_assignment}.
}
\description{
Azure role assignment class
}
\section{Fields}{

\itemize{
\item \code{id}: The full resource ID for this role assignment.
\item \code{type}: The resource type for a role assignment. Always \code{Microsoft.Authorization/roleAssignments}.
\item \code{name}: A GUID that identifies this role assignment.
\item \code{role_name}: The role definition name (in text), eg "Contributor".
\item \code{properties}: Properties for the role definition.
\item \code{token}: An OAuth token, obtained via \link{get_azure_token}.
}
}

\section{Methods}{

\itemize{
\item \code{remove(confirm=TRUE)}: Removes this role assignment.
}
}

\section{Initialization}{

The recommended way to create new instances of this class is via the \link{add_role_assignment} and \link{get_role_assignment} methods for subscription, resource group and resource objects.

Technically role assignments and role definitions are Azure \emph{resources}, and could be implemented as subclasses of \code{az_resource}. AzureRMR treats them as distinct, due to limited RBAC functionality currently supported.
}

\seealso{
\link{add_role_assignment}, \link{get_role_assignment}, \link{get_role_definition}, \link{az_role_definition}

\href{https://learn.microsoft.com/en-us/azure/role-based-access-control/overview}{Overview of role-based access control}
}
