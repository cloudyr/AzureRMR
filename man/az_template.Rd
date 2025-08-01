% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/az_template.R
\docType{class}
\name{az_template}
\alias{az_template}
\title{Azure template class}
\format{
An R6 object of class \code{az_template}.
}
\description{
Class representing an Azure deployment template.
}
\section{Methods}{

\itemize{
\item \code{new(token, subscription, resource_group, name, ...)}: Initialize a new template object. See 'Initialization' for more details.
\item \code{check()}: Check the deployment status of the template; throw an error if the template has been deleted.
\item \code{cancel(free_resources=FALSE)}: Cancel an in-progress deployment. Optionally free any resources that have already been created.
\item \code{delete(confirm=TRUE, free_resources=FALSE)}: Delete a deployed template, after a confirmation check. Optionally free any resources that were created. If the template was deployed in Complete mode (its resource group is exclusive to its use), the latter process will delete the entire resource group. Otherwise resources are deleted in the order given by the template's output resources list; in this case, some may be left behind if the ordering is incompatible with dependencies.
\item \code{list_resources()}: Returns a list of Azure resource objects that were created by the template. This returns top-level resources only, not those that represent functionality provided by another resource.
\item \code{get_tags()}: Returns the tags for the deployment template (note: this is not the same as the tags applied to resources that are deployed).
}
}

\section{Initialization}{

Initializing a new object of this class can either retrieve an existing template, or deploy a new template on the host. Generally, the easiest way to create a template object is via the \code{get_template}, \code{deploy_template} or \code{list_templates} methods of the \link{az_resource_group} class, which handle the details automatically.

To initialize an object that refers to an existing deployment, supply the following arguments to \code{new()}:
\itemize{
\item \code{token}: An OAuth 2.0 token, as generated by \link{get_azure_token}.
\item \code{subscription}: The subscription ID.
\item \code{resource_group}: The resource group.
\item \code{name}: The deployment name`.
}

If you also supply the following arguments to \code{new()}, a new template will be deployed:
\itemize{
\item \code{template}: The template to deploy. This can be provided in a number of ways:
\enumerate{
\item A nested list of R objects, which will be converted to JSON via \code{jsonlite::toJSON}
\item A vector of strings containing unparsed JSON
\item The name of a template file
\item A URL from which the host can download the template
}
\item \code{parameters}: The parameters for the template. This can be provided using any of the same methods as the \code{template} argument.
\item \code{wait}: Optionally, whether to wait until the deployment is complete. Defaults to FALSE, in which case the method will return immediately.
}

You can use the \code{build_template_definition} and \code{build_template_parameters} helper functions to construct the inputs for deploying a template. These can take as inputs R lists, JSON text strings, or file connections, and can also be extended by other packages.
}

\examples{
\dontrun{

# recommended way to deploy a template: via a resource group object

tpl <- resgroup$deploy_template("mydeployment",
    template="template.json",
    parameters="parameters.json")

# retrieve list of created resource objects
tpl$list_resources()

# delete template (will not touch resources)
tpl$delete()

# delete template and free resources
tpl$delete(free_resources=TRUE)

}
}
\seealso{
\link{az_resource_group}, \link{az_resource}, \link{build_template_definition}, \link{build_template_parameters}
\href{https://learn.microsoft.com/en-us/azure/templates/}{Template overview}
}
