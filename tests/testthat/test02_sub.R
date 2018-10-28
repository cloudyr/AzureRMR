context("Subscriptions")

tenant <- Sys.getenv("AZ_TEST_TENANT_ID")
app <- Sys.getenv("AZ_TEST_APP_ID")
password <- Sys.getenv("AZ_TEST_PASSWORD")
subscription <- Sys.getenv("AZ_TEST_SUBSCRIPTION")

if(tenant == "" || app == "" || password == "" || subscription == "")
    skip("Subscription method tests skipped: ARM credentials not set")

az <- az_rm$new(tenant=tenant, app=app, password=password)


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

    # assume there are actually resources
    res <- sub$list_resources()
    expect_true(is.list(res) && all(sapply(res, is_resource)))
})

