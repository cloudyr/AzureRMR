#' Azure resource group class
#'
#' Class representing an Azure resource group.
#'
#' @docType class
#' @section Methods:
#' - `new(token, subscription, id, ...)`: Initialize a resource group object. See 'Initialization' for more details.
#' - `delete(confirm=TRUE)`: Delete this resource group, after a confirmation check. This is asynchronous: while the method returns immediately, the delete operation continues on the host in the background. For resource groups containing a large number of deployed resources, this may take some time to complete.
#' - `list_templates()`: List deployed templates in this resource group.
#' - `get_template(name)`: Return an object representing an existing template.
#' - `deploy_template(...)`: Deploy a new template. See 'Templates' for more details.
#' - `delete_template(name, confirm=TRUE, free_resources=FALSE)`: Delete a deployed template, and optionally free any resources that were created.
#' - `get_resource(...)`: Return an object representing an existing resource. See 'Resources' for more details.
#' - `create_resource(...)`: Create a new resource.
#' - `delete_resource(..., confirm=TRUE, wait=FALSE)`: Delete an existing resource. Optionally wait for the delete to finish.
#' - `resource_exists(...)`: Check if a resource exists.
#' - `list_resources()`: Return a list of resource group objects for this subscription.
#' - `create_lock(name, level)`: Create a management lock on this resource group (which will propagate to all resources within it). The `level` argument can be either "cannotdelete" or "readonly".
#' - `get_lock(name`): Returns a management lock object.
#' - `delete_lock(name)`: Deletes a management lock object.
#'
#' @section Initialization:
#' Initializing a new object of this class can either retrieve an existing resource group, or create a new resource group on the host. Generally, the easiest way to create a resource group object is via the `get_resource_group`, `create_resource_group` or `list_resource_groups` methods of the [az_subscription] class, which handle this automatically.
#'
#' To create a resource group object in isolation, supply (at least) an Oauth 2.0 token of class [AzureToken], the subscription ID, and the resource group name. If this object refers to a _new_ resource group, supply the location as well (use the `list_locations` method of the `az_subscription class` for possible locations). You can also pass any optional parameters for the resource group as named arguments to `new()`.
#'
#' @section Templates:
#' To deploy a new template, pass the following arguments to `deploy_template()`:
#' - `name`: The name of the deployment.
#' - `template`: The template to deploy. This can be provided in a number of ways:
#'   1. A nested list of name-value pairs representing the parsed JSON
#'   2. The name of a template file
#'   3. A vector of strings containing unparsed JSON
#'   4. A URL from which the template can be downloaded
#' - `parameters`: The parameters for the template. This can be provided using any of the same methods as the `template` argument.
#'
#' Retrieving or deleting a deployed template requires only the name of the deployment.
#'
#' @section Resources:
#' There are a number of arguments to `get_resource()`, `create_resource()` and `delete_resource()` that serve to identify the specific resource in question:
#' - `id`: The full ID of the resource, including subscription ID and resource group.
#' - `provider`: The provider of the resource, eg `Microsoft.Compute`.
#' - `path`: The full path to the resource, eg `virtualMachines`.
#' - `type`: The combination of provider and path, eg `Microsoft.Compute/virtualMachines`.
#' - `name`: The name of the resource instance, eg `myWindowsVM`.
#'
#' Providing the `id` argument will fill in the values for all the other arguments. Similarly, providing the `type` argument will fill in the values for `provider` and `path`. Unless you provide `id`, you must also provide `name`.
#'
#' To create/deploy a new resource, specify any extra parameters that the provider needs as named arguments to `create_resource()`.
#'
#' @seealso
#' [az_subscription], [az_template], [az_resource],
#' [Azure resource group overview](https://docs.microsoft.com/en-us/azure/azure-resource-manager/resource-group-overview#resource-groups),
#' [Resources API reference](https://docs.microsoft.com/en-us/rest/api/resources/resources),
#' [Template API reference](https://docs.microsoft.com/en-us/rest/api/resources/deployments)
#'
#' @examples
#' \dontrun{
#' 
#' # recommended way to retrieve a resource group object
#' rg <- get_azure_login("myaadtenant")$
#'     get_subscription("subscription_id")$
#'     get_resource_group("rgname")
#'
#' # list resources & templates in this resource group
#' rg$list_resources()
#' rg$list_templates()
#'
#' # get a resource (virtual machine)
#' rg$get_resource(type="Microsoft.Compute/virtualMachines", name="myvm")
#'
#' # create a resource (storage account)
#' rg$create_resource(type="Microsoft.Storage/storageAccounts", name="mystorage",
#'     kind="StorageV2",
#'     sku=list(name="Standard_LRS"))
#'
#' # delete a resource
#' rg$delete_resource(type="Microsoft.Storage/storageAccounts", name="mystorage")
#'
#' # deploy a template
#' rg$deploy_template("tplname",
#'     template="template.json",
#'     parameters="parameters.json")
#'
#' # deploy a template with parameters inline
#' rg$deploy_template("mydeployment",
#'     template="template.json",
#'     parameters=list(parm1="foo", parm2="bar"))
#'
#' # delete a template and free resources
#' rg$delete_template("tplname", free_resources=TRUE)
#'
#' # delete the resource group itself
#' rg$delete()
#'
#' }
#' @format An R6 object of class `az_resource_group`.
#' @export
az_resource_group <- R6::R6Class("az_resource_group",

public=list(
    subscription=NULL,
    id=NULL,
    name=NULL,
    location=NULL,
    managed_by=NULL,
    properties=NULL,
    tags=NULL,
    token=NULL,

    # constructor: can refer to an existing RG, or create a new RG
    initialize=function(token, subscription, name=NULL, ..., parms=list())
    {
        if(is_empty(name) && is_empty(parms))
            stop("Must supply either resource group name, or parameter list")

        self$token <- token
        self$subscription <- subscription

        parms <- if(!is_empty(list(...)))
            private$init_and_create(name, ...)
        else private$init(name, parms)

        self$id <- parms$id
        self$location <- parms$location
        self$managed_by <- parms$managedBy
        self$properties <- parms$properties
        self$tags <- parms$tags

        NULL
    },

    delete=function(confirm=TRUE)
    {
        if(confirm && interactive())
        {
            yn <- readline(paste0("Do you really want to delete resource group '", self$name, "'? (y/N) "))
            if(tolower(substr(yn, 1, 1)) != "y")
                return(invisible(NULL))
        }

        private$rg_op(http_verb="DELETE")
        message("Deleting resource group '", self$name, "'. This operation may take some time to complete.")
        invisible(NULL)
    },

    list_templates=function()
    {
        cont <- private$rg_op("providers/Microsoft.Resources/deployments")
        lst <- lapply(cont$value,
            function(parms) az_template$new(self$token, self$subscription, self$name, deployed_properties=parms))
        # keep going until paging is complete
        while(!is_empty(cont$nextLink))
        {
            cont <- call_azure_url(self$token, cont$nextLink)
            lst <- c(lst, lapply(cont$value,
                function(parms) az_template$new(self$token, self$subscription, self$name, deployed_properties=parms)))
        }
        named_list(lst)
    },

    deploy_template=function(name, template, parameters, ...)
    {
        az_template$new(self$token, self$subscription, self$name, name,
                        template, parameters, ...)
    },

    get_template=function(name)
    {
        az_template$new(self$token, self$subscription, self$name, name)
    },

    delete_template=function(name, confirm=TRUE, free_resources=FALSE)
    {
        self$get_template(name)$delete(confirm=confirm, free_resources=free_resources)
    },

    list_resources=function()
    {
        cont <- private$rg_op("resources")
        lst <- lapply(cont$value, function(parms) az_resource$new(self$token, self$subscription, deployed_properties=parms))
        # keep going until paging is complete
        while(!is_empty(cont$nextLink))
        {
            cont <- call_azure_url(self$token, cont$nextLink)
            lst <- c(lst, lapply(cont$value,
                function(parms) az_resource$new(self$token, self$subscription, deployed_properties=parms)))
        }
        names(lst) <- sapply(lst, function(x) sub("^.+providers/(.+$)", "\\1", x$id))
        lst
    },

    get_resource=function(provider, path, type, name, id, api_version=NULL)
    {
        az_resource$new(self$token, self$subscription,
                        resource_group=self$name, provider=provider, path=path, type=type, name=name, id=id,
                        api_version=api_version)
    },

    resource_exists=function(provider, path, type, name, id)
    {
        # HEAD seems to be broken; use GET and check if it succeeds
        res <- try(self$get_resource(provider, path, type, name, id), silent=TRUE)
        !inherits(res, "try-error")
    },

    delete_resource=function(provider, path, type, name, id, api_version=NULL, confirm=TRUE, wait=FALSE)
    {
        # supply deployed_properties arg to prevent querying host for resource info
        az_resource$
            new(self$token, self$subscription, self$name,
                provider=provider, path=path, type=type, name=name, id=id,
                deployed_properties=list(NULL), api_version=api_version)$
            delete(confirm=confirm, wait=wait)
    },

    create_resource=function(provider, path, type, name, id, location=self$location, ...)
    {
        az_resource$new(self$token, self$subscription,
                        resource_group=self$name, provider=provider, path=path, type=type, name=name, id=id,
                        location=location, ...)
    },

    create_lock=function(name, level=c("cannotdelete", "readonly"), notes="")
    {
        level <- match.arg(level)
        api <- getOption("azure_api_mgmt_version")
        op <- file.path("providers/Microsoft.Authorization/locks", name)
        body <- list(properties=list(level=level))
        if(notes != "")
            body$notes <- notes

        res <- private$rg_op(op, body=body, encode="json", http_verb="PUT", api_version=api)
        az_resource$new(self$token, self$subscription, deployed_properties=res, api_version=api)
    },

    get_lock=function(name)
    {
        api <- getOption("azure_api_mgmt_version")
        op <- file.path("providers/Microsoft.Authorization/locks", name)
        res <- private$rg_op(op, api_version=api)
        az_resource$new(self$token, self$subscription, deployed_properties=res, api_version=api)
    },

    delete_lock=function(name)
    {
        api <- getOption("azure_api_mgmt_version")
        op <- file.path("providers/Microsoft.Authorization/locks", name)
        private$rg_op(op, http_verb="DELETE", api_version=api)
        invisible(NULL)
    },

    print=function(...)
    {
        cat("<Azure resource group ", self$name, ">\n", sep="")
        cat(format_public_fields(self, exclude=c("subscription", "name")))
        cat(format_public_methods(self))
        invisible(self)
    }
),

private=list(

    init=function(name, parms)
    {
        if(is_empty(parms))
        {
            self$name <- name
            parms <- private$rg_op()
        }
        else
        {
            private$validate_parms(parms)
            self$name <- parms$name
        }
        parms
    },

    init_and_create=function(name, ...)
    {
        parms <- modifyList(list(...), list(name=name))
        private$validate_parms(parms)
        self$name <- name
        private$rg_op(body=parms, encode="json", http_verb="PUT")
    },

    validate_parms=function(parms)
    {
        required_names <- c("location", "name")
        optional_names <- c("id", "managedBy", "tags", "properties")
        validate_object_names(names(parms), required_names, optional_names)
    },

    rg_op=function(op="", ...)
    {
        op <- construct_path("resourcegroups", self$name, op)
        call_azure_rm(self$token, self$subscription, op, ...)
    }
))

