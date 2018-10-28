---
title: "Registering a client app with Azure Active Directory"
Author: Hong Ooi, after Alan Weaver and Andrie de Vries
output: html_document
vignette: >
  %\VignetteIndexEntry{Azure Active Directory app registration}
  %\VignetteEngine{R.rsp::md}
  %\VignetteEncoding{utf8}
---

## Configuration instructions

To use the `AzureRMR` package, you must create an Azure Active Directory application with permisssions. This vignette contains instructions to do this.

You must collect at least two pieces of information to authenticate with `az_rm()`, possibly three:

* tenant ID (`tenant`)
* application ID (`app`)
* secret key (`secret`) optionally, if using client credentials: see later

## Create an Active Directory application

1. Login to the [Azure Portal](https://portal.azure.com).

1. On the left side of the screen, there should be a list of the different things you can create in Azure. Click on "Azure Active Directory".

1. The AAD blade should appear. Under "Manage", click on "Properties". Note the 'directory ID' entry on the right, which is, confusingly, your **tenant ID**.

1. Under "Manage", click on the "App registrations" entry.

1. Along the top menu, click "New application registration".

1. In the Create blade, enter the details for your new application. The name should be unique. It doesn't matter what sign-on URL you provide (it won't be used), but it must be a valid URL.

1. In the "Application type" box, choose "Web App/API" if you want to authenticate using a secret key, or "Native" if you want to authenticate with a code that the server sends to you at runtime. If you're not sure, choose "Native" as this provides more security. If you intend to use AzureRMR in a scripted setting (without user intervention), choose "Web App".

1. Click on "Create". After a few seconds, a new blade will appear containing a list of all registered AAD applications. Find your app by entering the name you chose into the search box.

1. When your app appears in the list, click on it. In the details, note the **application ID**.

1. The Settings blade for your app should also be on the screen. Click on the "Keys" entry.

1. If you chose "Web App/API" as the type of your app, you will need to create a new **secret key**. Enter a name for it, choose a 1 year duration (or 2) and click "Save" at the top of the blade. When the key is generated, copy it and save it somewhere. _You won't be able to see it again, so make sure you copy it now._

1. Return to your app settings by closing the Keys blade. Click the "Required permissions" entry.

1. In the permissions blade, click "Add". Click on "Select an API" and choose "Windows Azure Service Management API". Then click Select at the bottom of the blade.

1. This should bring up the Enable Access blade. Check the tick box next to "Delegated permissions" and click Select at the bottom of the blade.

1. Click Done at the bottom of the permissions blade.


## Access control

Azure lets you apply access controls at multiple levels: by subscription, by resource group, or by individual resource. It's recommended that you provide subscription-level access to an app meant for use with AzureSMR, as that allows maximum flexibility.


### At subscription Level

1. Click on Subscriptions on the left menu item in the portal.

1. Identify the Subscription you will associate with this application.

1. Choose the `Access Control (IAM)` menu item.

1. In the resulting scope click the `+ Add` button.

1. Choose the role as Owner and under the user search box enter the name of the App, e.g. `Azure R management app`.

2. Select the resulting list item for that App then click Select in that scope then OK in the "Add access" scope. The user will be added to the list.

### At resource group level

1. Click on Resource Groups menu item on the left in the portal.

16. Identify the resource group you will associate with this application.

17. Choose the `Access Control (IAM)` menu item from the Resource scope.

18. In the resulting scope click the `+ Add` button.

19. Choose the role as Owner and under the user search box enter the name of the App, e.g. `AzureSMR`.

20. Select the resulting list item for that App then click Select in that scope then OK in the `Add access` scope. The user will be added to the list.

## Conclusion

You can test this by trying:

```{r, eval = FALSE}
library(AzureRMR)
sc <- az_rm$new(tenant="{TID}", app="{CID}", secret="{KEY}")
sc
```

or using device authentication by trying:

```{r, eval = FALSE}
sc <- az_rm$new(tenant="{TID}", app="{CID}", auth_type="device")
# Manually authenticate using device code flow
sc
```

For more information see the tutorial [Use portal to create Active Directory application and service principal that can access resources](https://azure.microsoft.com/en-us/documentation/articles/resource-group-create-service-principal-portal/)
