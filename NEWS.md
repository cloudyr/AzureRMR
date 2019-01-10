# AzureRMR 1.0.0.9000

## Significant interface changes

* New `create_azure_login`, `get_azure_login` and `delete_azure_login` functions to handle ARM authentication. These will persist the login object across sessions, removing the need to re-authenticate each time. While directly calling `az_rm$new()` will still work, it's recommended to use `create_azure_login` and `get_azure_login` going forward.
* `get_azure_token` revamped, now supports the resource owner authentication method for obtaining an AAD token with a username and password.

## Other changes

* Don't print empty fields for ARM objects.
* Add optional `etag` field to resource object definition.
* Add `location` argument to `az_resource_group$create_resource` method, rather than hardcoding it to the resgroup location.
* Add `wait` argument when creating a new resource, similar to deploying a template, since some resources will return before provisioning is complete. Defaults to `FALSE` for backward compatibility.
* Export `is_azure_token`.

# AzureRMR 1.0.0

* Submitted to CRAN

# AzureRMR 0.9.0

* Moved to cloudyr organisation
