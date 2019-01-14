# AzureRMR 1.0.0.9000

## Significant interface changes

* New `get_azure_login` function to handle ARM authentication. While directly calling `az_rm$new()` will still work, it's recommended to use `get_azure_login` going forward. Login credentials will be saved and reused for subsequent sessions (see below).
* `get_azure_token` significantly revamped. It now supports four authentication methods for obtaining AAD tokens:
  - Client credentials (what you would use with a "web app" registered service principal)
  - Authorization code (for a "native" service principal)
  - Device code
  - With a username and password (resource owner grant)
* `get_azure_token` will now cache AAD tokens and refresh them for subsequent sessions. Tokens are cached in a user-specific configuration directory (unlike httr, which saves them in a special file in the R working directory).
* Token acquisition logic will shortly move to a new package, to allow it to be used by other packages independently of the Resource Manager interface.

## Other changes

* Don't print empty fields for ARM objects.
* Add optional `etag` field to resource object definition.
* Add `location` argument to `az_resource_group$create_resource` method, rather than hardcoding it to the resgroup location.
* Add `wait` argument when creating a new resource, similar to deploying a template, since some resources will return before provisioning is complete. Defaults to `FALSE` for backward compatibility.
* Export `is_azure_token`.
* Allow `az_resource_group$deploy_template()` to work without `parameters` arg (parameters folded into template itself).

# AzureRMR 1.0.0

* Submitted to CRAN

# AzureRMR 0.9.0

* Moved to cloudyr organisation
