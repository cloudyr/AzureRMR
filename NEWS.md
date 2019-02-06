# AzureRMR 1.0.0.9000

## Significant interface changes

* Allow authentication without having to create a service principal first, by leveraging the Azure CLI cross-platform app. It's still recommended to create your own SP for authentication, if possible.
* New `create_azure_login`, `get_azure_login` and `delete_azure_login` functions to handle ARM authentication. While directly calling `az_rm$new()` will still work, it's recommended to use `create_azure_login` and `get_azure_login` going forward. Login credentials will be saved and reused for subsequent sessions (see below).
* `get_azure_token` significantly revamped. It now supports four authentication methods for obtaining AAD tokens:
  - Client credentials (what you would use with a "web app" registered service principal)
  - Authorization code (for a "native" service principal)
  - Device code
  - With a username and password (resource owner grant)
* `get_azure_token` will now cache AAD tokens and refresh them for subsequent sessions. Tokens are cached in a user-specific configuration directory, using the rappdirs package (unlike httr, which saves them in a special file in the R working directory).
* By default, use the latest _stable_ API version when interacting with resources. `az_resource$set_api_version` gains a new argument `stable_only` which defaults to `TRUE`; set this to `FALSE` if you want the latest preview version.
* Token acquisition logic moved to a new package, [AzureAuth](https://github.com/cloudyr/AzureAuth).

## Other changes

* Don't print empty fields for ARM objects.
* Add optional `etag` field to resource object definition.
* Add `location` argument to `az_resource_group$create_resource` method, rather than hardcoding it to the resgroup location.
* Add `wait` argument when creating a new resource, similar to deploying a template, since some resources will return before provisioning is complete. Defaults to `FALSE` for backward compatibility.
* Export `is_azure_token`.
* Allow `az_resource_group$deploy_template()` to work without `parameters` arg (parameters folded into template itself).
* Fix a bug that kept `az_resource_group$delete_resource` from deleting the resource.
* New resource method `az_resource$get_api_version` to match `set_api_version`.

# AzureRMR 1.0.0

* Submitted to CRAN

# AzureRMR 0.9.0

* Moved to cloudyr organisation
