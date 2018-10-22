context("Subscriptions")

tenant <- Sys.getenv("AZ_TENANT_ID")
app <- Sys.getenv("AZ_APP_ID")
secret <- Sys.getenv("AZ_SECRET")
subscription <- Sys.getenv("AZ_SUBSCRIPTION")

if(tenant == "" || app == "" || secret == "" || subscription == "")
    skip("Subscription method tests skipped: ARM credentials not set")

az <- az_rm$new(tenant=tenant, app=app, secret=secret)


test_that("Subscription methods work",
{
    sub <- az$get_subscription(subscription)
    expect_is(sub, "az_subscription")

    locs <- sub$list_locations()
    expect_is(locs, "data.frame")

    vers <- sub$get_provider_api_version()
    expect_true(is.list(vers))

    vers_comp <- sub$get_provider_api_version("Microsoft.Compute")
    expect_true(is.character(vers_comp))

    vers_vm <- sub$get_provider_api_version("Microsoft.Compute", "virtualMachines")
    expect_true(is.character(vers_vm) && length(vers_vm) == 1)

    # assume there are actually resource groups here
    rgs <- sub$list_resource_groups()
    expect_true(is.list(rgs) && all(sapply(rgs, is_resource_group)))
})

