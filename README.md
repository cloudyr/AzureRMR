# AzureRMR

AzureRMR is a package for interacting with Azure Resource Manager: authenticate, list subscriptions, manage resource groups, deploy and delete templates and resources. It calls the Resource Manager [REST API](https://docs.microsoft.com/en-us/rest/api/resources) directly, so you don't need to have PowerShell or Python installed.

AzureRMR is not yet available on CRAN. You can install it from GitHub, via `devtools::install_github("cloudyr/AzureRMR")`.

## Activating

To use AzureRMR, you must create and register a service principal with Azure Active Directory. This is a one-time task, and the easiest method is to use the Azure cloud shell.

- In the Azure Portal (https://portal.azure.com/), click on the Cloud Shell icon:

![](vignettes/images/cloudportal2.png)

- If you haven't used the shell before, there will be a dialog box to choose whether to use bash or PowerShell. Choose bash.
- In the shell, type `az ad sp create-for-rbac --name {app-name} --subscription "{your-subscription-name}" --years {N}`, substituting the desired name of your service principal (try to make it memorable to you, and unlikely to clash with other names), your subscription name, and the number of years you want the password to be valid.
- Wait until the app creation is complete. You should see a screen like this.

![](vignettes/images/cloudshell.png)

- Record your tenant ID, app ID, and password.

## Extending

AzureRMR is meant to be a generic mechanism for working with Resource Manager. You can extend it to provide support for service-specific features; examples of packages that do this include [AzureVM](https://github.com/cloudyr/AzureVM) for [virtual machines](https://azure.microsoft.com/en-us/services/virtual-machines/), and [AzureStor](https://github.com/cloudyr/AzureStor) for [storage accounts](https://azure.microsoft.com/en-us/services/storage/). For more information, see the ["Extending AzureRMR" vignette](vignettes/extend.Rmd).

---
[![cloudyr project logo](https://i.imgur.com/JHS98Y7.png)](https://github.com/cloudyr)
