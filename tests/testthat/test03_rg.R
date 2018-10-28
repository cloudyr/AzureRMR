context("Resource groups")

tenant <- Sys.getenv("AZ_TEST_TENANT_ID")
app <- Sys.getenv("AZ_TEST_APP_ID")
password <- Sys.getenv("AZ_TEST_PASSWORD")
subscription <- Sys.getenv("AZ_TEST_SUBSCRIPTION")

if(tenant == "" || app == "" || password == "" || subscription == "")
    skip("Resource group method tests skipped: ARM credentials not set")

sub <- az_rm$new(tenant=tenant, app=app, password=password)$get_subscription(subscription)


test_that("Resource group methods work",
{
    rgname <- paste(sample(letters, 20, replace=TRUE), collapse="")

    rgnew <- sub$create_resource_group(rgname, location="westus")
    expect_is(rgnew, "az_resource_group")
    expect_equal(rgnew$name, rgname)

    rgnew2 <- sub$get_resource_group(rgname)
    expect_is(rgnew2, "az_resource_group")
    expect_equal(rgnew2$name, rgname)

    rgnew$delete(confirm=FALSE)
    expect_true({
        status <- tryCatch(sub$get_resource_group(rgname)$properties$provisioningState == "Deleting",
            error=function(e) TRUE)
    })
})
