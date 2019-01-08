# AzureRMR 1.0.0.9000

## Significant interface changes

* New `create_azure_login`, `get_azure_login` and `delete_azure_login` functions to handle ARM authentication. These will persist the login object across sessions, removing the need to re-authenticate each time. While directly calling `az_rm$new()` will still work, it's recommended to use `create_azure_login` and `get_azure_login` going forward.

## Other changes

* Don't print empty fields for ARM objects.
* Add optional `etag` field to resource object definition.
* Fix `AzureToken` object to never have a `NULL` password field (important to allow devicecode refreshing).
* Add `location` argument to `az_resource_group$create_resource` method, rather than hardcoding it to the resgroup location.
* Add `wait` argument when creating a new resource, similar to deploying a template, since some resources will return before provisioning is complete. Defaults to `FALSE` for backward compatibility.
* Initialise AzureToken objects with an empty string as password instead of `NULL` when using device code flow; required by httr 1.4.0's stricter input checking.

# AzureRMR 1.0.0

* Submitted to CRAN

# AzureRMR 0.9.0

* Moved to cloudyr organisation
