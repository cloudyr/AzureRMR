# AzureRMR 1.1.0

* Revamped authentication process that mimics the flow in the Azure CLI. Now, you can use the `create_az_login()` function to create a persistent login object, which will be saved and reused in later sessions with `get_az_login()`. By default this authenticates using a custom AAD app created for AzureR, removing the need for the user to create a service principal.

- You can still authenticate with a direct call to `az_rm$new()` if so desired.

# AzureRMR 1.0.0

* Submitted to CRAN

# AzureRMR 0.9.0

* Moved to cloudyr organisation
