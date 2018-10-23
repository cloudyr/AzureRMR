## ---- eval=FALSE---------------------------------------------------------
#  az_storage <- R6::R6Class("az_storage", inherit=AzureRMR::az_resource,
#  
#  public=list(
#  
#      list_keys=function()
#      {
#          keys <- named_list(private$res_op("listKeys", http_verb="POST")$keys, "keyName")
#          sapply(keys, `[[`, "value")
#      },
#  
#      get_blob_endpoint=function(key=self$list_keys()[1], sas=NULL)
#      {
#          blob_endpoint(self$properties$primaryEndpoints$blob, key=key, sas=sas)
#      },
#  
#      get_file_endpoint=function(key=self$list_keys()[1], sas=NULL)
#      {
#          file_endpoint(self$properties$primaryEndpoints$file, key=key, sas=sas)
#      }
#  ))

## ---- eval=FALSE---------------------------------------------------------
#  az_vm_template <- R6::R6Class("az_vm_template", inherit=AzureRMR::az_template,
#  
#  public=list(
#      disks=NULL,
#      status=NULL,
#      ip_address=NULL,
#      dns_name=NULL,
#      clust_size=NULL,
#  
#      initialize=function(token, subscription, resource_group, name, ...)
#      {
#          super$initialize(token, subscription, resource_group, name, ...)
#  
#          # fill in fields that don't require querying the host
#          num_instances <- self$properties$outputs$numInstances
#          if(is_empty(num_instances))
#          {
#              self$clust_size <- 1
#              vmnames <- self$name
#          }
#          else
#          {
#              self$clust_size <- as.numeric(num_instances$value)
#              vmnames <- paste0(self$name, seq_len(self$clust_size) - 1)
#          }
#  
#          private$vm <- sapply(vmnames, function(name)
#          {
#              az_vm_resource$new(self$token, self$subscription, self$resource_group,
#                  type="Microsoft.Compute/virtualMachines", name=name)
#          }, simplify=FALSE)
#  
#          # get the hostname/IP address for the VM
#          outputs <- unlist(self$properties$outputResources)
#          ip_id <- grep("publicIPAddresses/.+$", outputs, ignore.case=TRUE, value=TRUE)
#          ip <- lapply(ip_id, function(id)
#              az_resource$new(self$token, self$subscription, id=id)$properties)
#  
#          self$ip_address <- sapply(ip, function(x) x$ipAddress)
#          self$dns_name <- sapply(ip, function(x) x$dnsSettings$fqdn)
#  
#          lapply(private$vm, function(obj) obj$sync_vm_status())
#          self$disks <- lapply(private$vm, "[[", "disks")
#          self$status <- lapply(private$vm, "[[", "status")
#  
#          NULL
#      }
#  
#      # ... other VM-specific methods ...
#  ),
#  
#  private=list(
#      # will store a list of VM objects after initialisation
#      vm=NULL
#  
#      # ... other private members ...
#  )
#  ))

## ---- eval=FALSE---------------------------------------------------------
#  res <- az_rm$new("tenant_id", "app_id", "secret") $
#      get_subscription("subscription_id") $
#      get_resource_group("resgroup") $
#      get_my_resource("myresource")

## ---- eval=FALSE---------------------------------------------------------
#  
#  # all methods adding methods to classes in external package must go in .onLoad
#  .onLoad <- function(libname, pkgname)
#  {
#      AzureRMR::az_resource_group$set("public", "create_storage_account", overwrite=TRUE,
#      function(name, location,
#               kind="Storage",
#               sku=list(name="Standard_LRS", tier="Standard"),
#               ...)
#      {
#          az_storage$new(self$token, self$subscription, self$name,
#                         type="Microsoft.Storage/storageAccounts", name=name, location=location,
#                         kind=kind, sku=sku, ...)
#      })
#  
#      AzureRMR::az_resource_group$set("public", "get_storage_account", overwrite=TRUE,
#      function(name)
#      {
#          az_storage$new(self$token, self$subscription, self$name,
#                         type="Microsoft.Storage/storageAccounts", name=name)
#      })
#  
#      AzureRMR::az_resource_group$set("public", "delete_storage_account", overwrite=TRUE,
#      function(name, confirm=TRUE, wait=FALSE)
#      {
#          self$get_storage_account(name)$delete(confirm=confirm, wait=wait)
#      })
#  
#      # ... other startup code ...
#  }

## ---- eval=FALSE---------------------------------------------------------
#  .onLoad <- function(libname, pkgname)
#  {
#      AzureRMR::az_resource_group$set("public", "create_vm_cluster", overwrite=TRUE,
#      function(name, location,
#               os=c("Windows", "Ubuntu"), size="Standard_DS3_v2",
#               username, passkey, userauth_type=c("password", "key"),
#               ext_file_uris=NULL, inst_command=NULL,
#               clust_size, template, parameters,
#               ..., wait=TRUE)
#      {
#          os <- match.arg(os)
#          userauth_type <- match.arg(userauth_type)
#  
#          if(missing(parameters) && (missing(username) || missing(passkey)))
#              stop("Must supply login username and password/private key", call.=FALSE)
#  
#          # find template given input args
#          if(missing(template))
#              template <- get_dsvm_template(os, userauth_type, clust_size,
#                                            ext_file_uris, inst_command)
#  
#          # convert input args into parameter list for template
#          if(missing(parameters))
#              parameters <- make_dsvm_param_list(name=name, size=size,
#                  username=username, userauth_type=userauth_type, passkey=passkey,
#                  ext_file_uris=ext_file_uris, inst_command=inst_command,
#                  clust_size=clust_size, template=template)
#  
#          az_vm_template$new(self$token, self$subscription, self$name, name,
#                             template=template, parameters=parameters, ..., wait=wait)
#      })
#  
#      # ... other startup code ...
#  }

## ---- eval=FALSE---------------------------------------------------------
#  #' Get existing Azure resource type 'foo'
#  #'
#  #' Methods for the [AzureRMR::az_resource_group] and [AzureRMR::az_subscription] classes.
#  #'
#  #' @rdname get_foo
#  #' @name get_foo
#  #' @aliases get_foo list_foos
#  #'
#  #' @section Usage:
#  #' ```
#  #' get_foo(name)
#  #' list_foos()
#  #' ```
#  #' @section Arguments:
#  #' - `name`: For `get_foo()`, the name of the resource.
#  #'
#  #' @section Details:
#  #' The `AzureRMR::az_resource_group` class has both `get_foo()` and `list_foos()` methods, while the `AzureRMR::az_subscription` class only has the latter.
#  #'
#  #' @section Value:
#  #' For `get_foo()`, an object of class `az_foo` representing the foo resource.
#  #'
#  #' For `list_foos()`, a list of such objects.
#  #'
#  #' @seealso
#  #' [create_foo], [delete_foo], [az_foo]
#  NULL

## ---- eval=FALSE---------------------------------------------------------
#  # blob endpoint for a storage account
#  blob_endpoint <- function(endpoint, key=NULL, sas=NULL, api_version=getOption("azure_storage_api_version"))
#  {
#      if(!is_endpoint_url(endpoint, "blob"))
#          stop("Not a blob endpoint", call.=FALSE)
#  
#      obj <- list(url=endpoint, key=key, sas=sas, api_version=api_version)
#      class(obj) <- c("blob_endpoint", "storage_endpoint")
#      obj
#  }
#  
#  
#  # S3 generic and methods to create an object representing a blob container within an endpoint
#  blob_container <- function(endpoint, ...)
#  {
#      UseMethod("blob_container")
#  }
#  
#  blob_container.character <- function(endpoint, key=NULL, sas=NULL,
#                                       api_version=getOption("azure_storage_api_version"))
#  {
#      do.call(blob_container, generate_endpoint_container(endpoint, key, sas, api_version))
#  }
#  
#  blob_container.blob_endpoint <- function(endpoint, name)
#  {
#      obj <- list(name=name, endpoint=endpoint)
#      class(obj) <- "blob_container"
#      obj
#  }
#  
#  
#  # download a file from a blob container
#  download_blob <- function(container, src, dest, overwrite=FALSE, lease=NULL)
#  {
#      headers <- list()
#      if(!is.null(lease))
#          headers[["x-ms-lease-id"]] <- as.character(lease)
#      do_container_op(container, src, headers=headers, config=httr::write_disk(dest, overwrite))
#  }

