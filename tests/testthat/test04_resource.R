context("Resources")

tenant <- Sys.getenv("AZ_TEST_TENANT_ID")
app <- Sys.getenv("AZ_TEST_APP_ID")
password <- Sys.getenv("AZ_TEST_PASSWORD")
subscription <- Sys.getenv("AZ_TEST_SUBSCRIPTION")

if(tenant == "" || app == "" || password == "" || subscription == "")
    skip("Resource group method tests skipped: ARM credentials not set")

rgname <- paste(sample(letters, 20, replace=TRUE), collapse="")
rg <- az_rm$
    new(tenant=tenant, app=app, password=password)$
    get_subscription(subscription)$
    create_resource_group(rgname, location="westus")


test_that("Resource methods work",
{
    # storage account resource
    resname <- paste(sample(letters, 20, replace=TRUE), collapse="")

    expect_false(rg$resource_exists(type="foo/bar", name="randomname"))
    expect_false(rg$resource_exists(type="Microsoft.Storage/storageAccounts", name=resname))

    res <- rg$create_resource(type="Microsoft.Storage/storageAccounts", name=resname,
        kind="Storage",
        sku=list(name="Standard_LRS", tier="Standard"))

    expect_true(rg$resource_exists(type="Microsoft.Storage/storageAccounts", name=resname))
    expect_is(res, "az_resource")
    expect_true(res$type == "Microsoft.Storage/storageAccounts" && res$name == resname)
    
    res2 <- rg$get_resource(type="Microsoft.Storage/storageAccounts", name=resname)
    expect_is(res2, "az_resource")
    expect_true(res2$type == "Microsoft.Storage/storageAccounts" && res2$name == resname)

    reslst <- rg$list_resources()
    expect_true(is.list(reslst) && all(sapply(reslst, is_resource)))

    expect_silent(res$sync_fields())

    res$set_api_version()
    expect_true(!is.null(res$.__enclos_env__$private$api_version))

    res$set_tags(tag1="value1")
    expect_true(!is.null(res$tags))
})

rg$delete(confirm=FALSE)
