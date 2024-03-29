% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/az_resource.R
\docType{class}
\name{az_resource}
\alias{az_resource}
\title{Azure resource class}
\format{
An R6 object of class \code{az_resource}.
}
\description{
Class representing a generic Azure resource.
}
\section{Methods}{

\itemize{
\item \code{new(...)}: Initialize a new resource object. See 'Initialization' for more details.
\item \code{delete(confirm=TRUE, wait=FALSE)}: Delete this resource, after a confirmation check. Optionally wait for the delete to finish.
\item \code{update(...)}: Update this resource on the host.
\item \code{sync_fields()}: Synchronise the R object with the resource it represents in Azure. Returns the \code{properties$provisioningState} field, so you can query this programmatically to check if a resource has finished provisioning. Not all resource types require explicit provisioning, in which case this method will return NULL.
\item \code{set_api_version(api_version, stable_only=TRUE)}: Set the API version to use when interacting with the host. If \code{api_version} is not supplied, use the latest version available, either the latest stable version (if \code{stable_only=TRUE}) or the latest preview version (if \code{stable_only=FALSE}).
\item \code{get_api_version()}: Get the current API version.
\item \code{get_subresource(type, name)}: Get a sub-resource of this resource. See 'Sub-resources' below.
\item \code{create_subresource(type, name, ...)}: Create a sub-resource of this resource.
\item \code{delete_subresource(type, name, confirm=TRUE)}: Delete a sub-resource of this resource.
\item \code{do_operation(...)}: Carry out an operation. See 'Operations' for more details.
\item \code{set_tags(..., keep_existing=TRUE)}: Set the tags on this resource. The tags can be either names or name-value pairs. To delete a tag, set it to \code{NULL}.
\item \code{get_tags()}: Get the tags on this resource.
\item \code{create_lock(name, level)}: Create a management lock on this resource.
\item \code{get_lock(name)}: Returns a management lock object.
\item \code{delete_lock(name)}: Deletes a management lock object.
\item \code{list_locks()}: List all locks that apply to this resource. Note this includes locks created at the subscription or resource group level.
\item \code{add_role_assignment(name, ...)}: Adds a new role assignment. See 'Role-based access control' below.
\item \code{get_role_assignment(id)}: Retrieves an existing role assignment.
\item \code{remove_role_assignment(id)}: Removes an existing role assignment.
\item \code{list_role_assignments()}: Lists role assignments.
\item \code{get_role_definition(id)}: Retrieves an existing role definition.
\item \code{list_role_definitions()} Lists role definitions.
}
}

\section{Initialization}{

There are multiple ways to initialize a new resource object. The \code{new()} method can retrieve an existing resource, deploy/create a new resource, or create an empty/null object (without communicating with the host), based on the arguments you supply.

All of these initialization options have the following arguments in common.
\enumerate{
\item \code{token}: An OAuth 2.0 token, as generated by \link{get_azure_token}.
\item \code{subscription}: The subscription ID.
\item \code{api_version}: Optionally, the API version to use when interacting with the host. By default, this is NULL in which case the latest API version will be used.
\item A set of \emph{identifying arguments}:
\itemize{
\item \code{resource_group}: The resource group containing the resource.
\item \code{id}: The full ID of the resource. This is a string of the form \verb{/subscriptions/\{uuid\}/resourceGroups/\{resource-group-name\}/provider/\{resource-provider-name\}/\{resource-path\}/\{resource-name\}}.
\item \code{provider}: The provider of the resource, eg \code{Microsoft.Compute}.
\item \code{path}: The path to the resource, eg \code{virtualMachines}.
\item \code{type}: The combination of provider and path, eg \code{Microsoft.Compute/virtualMachines}.
\item \code{name}: The name of the resource instance, eg \code{myWindowsVM}.
}
}

Providing \code{id} will fill in the values for all the other identifying arguments. Similarly, providing \code{type} will fill in the values for \code{provider} and \code{path}. Unless you provide \code{id}, you must also provide \code{name}.

The default behaviour for \code{new()} is to retrieve an existing resource, which occurs if you supply only the arguments listed above. If you also supply an argument \code{deployed_properties=NULL}, this will create a null object. If you supply any other (named) arguments, \code{new()} will create a new object on the host, with the supplied arguments as parameters.

Generally, the easiest way to initialize an object is via the \code{get_resource}, \code{create_resource} or \code{list_resources} methods of the \link{az_resource_group} class, which will handle all the gory details automatically.
}

\section{Operations}{

The \code{do_operation()} method allows you to carry out arbitrary operations on the resource. It takes the following arguments:
\itemize{
\item \code{op}: The operation in question, which will be appended to the URL path of the request.
\item \code{options}: A named list giving the URL query parameters.
\item \code{...}: Other named arguments passed to \link{call_azure_rm}, and then to the appropriate call in httr. In particular, use \code{body} to supply the body of a PUT, POST or PATCH request.
\item \code{http_verb}: The HTTP verb as a string, one of \code{GET}, \code{PUT}, \code{POST}, \code{DELETE}, \code{HEAD} or \code{PATCH}.
}

Consult the Azure documentation for your resource to find out what operations are supported.
}

\section{Sub-resources}{

Some resource types can have sub-resources: objects exposed by Resource Manager that make up a part of their parent's functionality. For example, a storage account (type \code{Microsoft.Storage/storageAccounts}) provides the blob storage service, which can be accessed via Resource Manager as a sub-resource of type \code{Microsoft.Storage/storageAccounts/blobServices/default}.

To retrieve an existing sub-resource, use the \code{get_subresource()} method. You do not need to include the parent resource's type and name. For example, if \code{res} is a resource for a storage account, and you want to retrieve the sub-resource for the blob container "myblobs", call

\if{html}{\out{<div class="sourceCode">}}\preformatted{res$get_subresource(type="blobServices/default/containers", name="myblobs")
}\if{html}{\out{</div>}}

Notice that the storage account's resource type and name are omitted from the \code{get_subresource} arguments. Similarly, to create a new subresource, call the \code{create_subresource()} method with the same naming convention, passing any required fields as named arguments; and to delete it, call \code{delete_subresource()}.
}

\section{Role-based access control}{

AzureRMR implements a subset of the full RBAC functionality within Azure Active Directory. You can retrieve role definitions and add and remove role assignments, at the subscription, resource group and resource levels. See \link{rbac} for more information.
}

\examples{
\dontrun{

# recommended way to retrieve a resource: via a resource group object
# storage account:
stor <- resgroup$get_resource(type="Microsoft.Storage/storageAccounts", name="mystorage")
# virtual machine:
vm <- resgroup$get_resource(type="Microsoft.Compute/virtualMachines", name="myvm")

## carry out operations on a resource

# storage account: get access keys
stor$do_operation("listKeys", http_verb="POST")

# virtual machine: run a script
vm$do_operation("runCommand",
    body=list(
        commandId="RunShellScript", # RunPowerShellScript for Windows
        script=as.list("ifconfig > /tmp/ifconfig.out")
    ),
    encode="json",
    http_verb="POST")

## retrieve properties

# storage account: endpoint URIs
stor$properties$primaryEndpoints$file
stor$properties$primaryEndpoints$blob

# virtual machine: hardware profile
vm$properties$hardwareProfile

## update a resource: resizing a VM
properties <- list(hardwareProfile=list(vmSize="Standard_DS3_v2"))
vm$do_operation(http_verb="PATCH",
    body=list(properties=properties),
    encode="json")

# sync with Azure: useful to track resource creation/update status
vm$sync_fields()

## subresource: create a public blob container
stor$create_subresource(type="blobservices/default/containers", name="mycontainer",
    properties=list(publicAccess="container"))

## delete a subresource and resource
stor$delete_subresource(type="blobservices/default/containers", name="mycontainer")
stor$delete()

}
}
\seealso{
\link{az_resource_group}, \link{call_azure_rm}, \link{call_azure_url},
\href{https://learn.microsoft.com/en-us/rest/api/resources/resources}{Resources API reference}

For role-based access control methods, see \link{rbac}

For management locks, see \link{lock}
}
