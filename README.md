# AzureRMR

AzureRMR is a package for interacting with Azure Resource Manager: authenticate, list subscriptions, manage resource groups, deploy and delete templates and resources. It calls the Resource Manager [REST API](https://docs.microsoft.com/en-us/rest/api/resources) directly, so you don't need to have PowerShell or Python installed.

You can install the development version from GitHub, via `devtools::install_github("cloudyr/AzureRMR")`.

## Authentication

AzureRMR uses a similar authentication process to the [Azure CLI](https://docs.microsoft.com/en-us/cli/azure/?view=azure-cli-latest). The first time you authenticate with a given Azure Active Directory tenant, you call `create_az_login("tenant_name")`. R will display a code and prompt you to visit the Microsoft login page in your browser. You then enter the code along with your Active Directory credentials, which completes the authentication process. The returned Resource Manager client object is also saved on your machine, and can be retrieved in subsequent R sessions with `get_az_login("tenant_name")`. AzureRMR will automatically handle details like refreshing your credentials.

This achieves two things: first, you only have to manually authenticate once; and second, it saves you from having to create a service principal (although you can also do that if so desired).

As a matter of convenience, you can also call `get_az_login("tenant_name")` on first login rather than `create_az_login`. In this case, AzureRMR will detect that you don't have a saved client object and create it for you.

## Sample workflow

```r
library(AzureRMR)

# authenticate with Azure AD:
# if this is the first time you're using the package, it will save a client object,
# otherwise it will reload the saved object and refresh your credentials
az <- get_az_login("myaadtenant.onmicrosoft.com")

# get a subscription and resource group
sub <- az$get_subscription("{subscription_id}")
rg <- sub$get_resource_group("rgname")

# get a resource (storage account)
stor <- rg$get_resource(type="Microsoft.Storage/storageAccounts", name="mystorage")

# method chaining works too
stor <- az$
    get_subscription("{subscription_id}")$
    get_resource_group("rgname")$
    get_resource(type="Microsoft.Storage/storageAccounts", name="mystorage")


# create a new resource group and resource
rg2 <- sub$create_resource_group("newrgname", location="westus")

stor2 <- rg2$create_resource(type="Microsoft.Storage/storageAccounts", name="mystorage2",
    kind="Storage", sku=list(name="Standard_LRS"))

# delete them
stor2$delete(confirm=FALSE)
rg2$delete(confirm=FALSE)
```

## Extending

AzureRMR is meant to be a generic mechanism for working with Resource Manager. You can extend it to provide support for service-specific features; examples of packages that do this include [AzureVM](https://github.com/cloudyr/AzureVM) for [virtual machines](https://azure.microsoft.com/en-us/services/virtual-machines/), and [AzureStor](https://github.com/cloudyr/AzureStor) for [storage accounts](https://azure.microsoft.com/en-us/services/storage/). For more information, see the ["Extending AzureRMR" vignette](vignettes/extend.Rmd).

---
[![cloudyr project logo](https://i.imgur.com/JHS98Y7.png)](https://github.com/cloudyr)
