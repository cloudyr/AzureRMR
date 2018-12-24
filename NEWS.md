# AzureRMR 1.0.0.9000

* Add optional `etag` field to resource object definition.
* Fix `AzureToken` object to never have a `NULL` password field (important to allow devicecode refreshing).
* Add `location` argument to `az_resource_group$create_resource` method, rather than hardcoding it to the resgroup location.
* Add `wait` argument when creating a new resource, similar to deploying a template, since some resources will return before provisioning is complete. Defaults to `FALSE` for backward compatibility.

# AzureRMR 1.0.0

* Submitted to CRAN

# AzureRMR 0.9.0

* Moved to cloudyr organisation
