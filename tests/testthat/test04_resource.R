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
    create_resource_group(rgname, location="eastus")


test_that("Resource methods work",
{
    expect_false(rg$resource_exists(type="Microsoft.Storage/storageAccounts", name="randomname"))

    # public key resource (no wait required)
    restype <- "Microsoft.Compute/sshPublicKeys"
    resname <- paste(sample(letters, 20, replace=TRUE), collapse="")

    expect_false(rg$resource_exists(type=restype, name=resname))
    res <- rg$create_resource(type=restype, name=resname)

    expect_true(rg$resource_exists(type=restype, name=resname))
    expect_is(res, "az_resource")
    expect_true(res$type == restype && res$name == resname)

    res1 <- rg$get_resource(type=restype, name=resname)
    expect_is(res1, "az_resource")
    expect_true(res1$type == restype && res1$name == resname)

    reslst <- rg$list_resources()
    expect_true(is.list(reslst) && all(sapply(reslst, is_resource)))

    expect_silent(res$sync_fields())

    res$set_api_version()
    expect_true(!is.null(res$.__enclos_env__$private$api_version))

    # tagging
    res$set_tags(tag1="value1")
    expect_identical(res$get_tags(), list(createdBy="AzureR/AzureRMR", tag1="value1"))
    res$set_tags(tag2)
    expect_identical(res$get_tags(), list(createdBy="AzureR/AzureRMR", tag1="value1", tag2=""))
    res$set_tags(tag2=NULL)
    expect_identical(res$get_tags(), list(createdBy="AzureR/AzureRMR", tag1="value1"))
    res$set_tags(keep_existing=FALSE)
    expect_true(is_empty(res$get_tags()))

    # locking
    expect_is(res$create_lock("newlock_res", level="cannotdelete"), "az_resource")
    expect_is(res$get_lock("newlock_res"), "az_resource")
    expect_true({
        locks <- res$list_locks()
        is.list(locks) && all(sapply(locks, is_resource))
    })
    expect_null(res$delete_lock("newlock_res"))
    expect_error(res$get_lock("newlock_res"))

    # wait arg
    resname2 <- paste(sample(letters, 20, replace=TRUE), collapse="")
    res2 <- rg$create_resource(type="Microsoft.Storage/storageAccounts", name=resname2,
        kind="StorageV2",
        sku=list(name="Standard_LRS", tier="Standard"),
        properties=list(isHnsEnabled=TRUE),
        wait=TRUE)
    expect_true(is(res2, "az_resource") && !is_empty(res2$properties))
})

test_that("Extended resource fields works",
{
    # managed disk resource
    restype <- "Microsoft.Compute/disks"
    resname <- paste(sample(letters, 20, replace=TRUE), collapse="")

    res <- rg$create_resource(type=restype, name=resname,
        properties=list(
            creationData=list(createOption="empty"),
            diskSizeGB=500,
            osType=""
        ),
        sku=list(name="Standard_LRS"),
        zones=list(1),
        wait=TRUE
    )

    Sys.sleep(30)  # let Azure catch up

    expect_true(rg$resource_exists(type=restype, name=resname))
    expect_is(res, "az_resource")

    expect_false(is_empty(res$ext))

    reslst <- rg$list_resources()
    expect_true(is.list(reslst) && all(sapply(reslst, is_resource)))
})

test_that("List filters work",
{
    reslst0 <- rg$list_resources()
    expect_identical(length(reslst0), 3L)

    reslst <- rg$list_resources(top=1)
    expect_true(is.list(reslst) && length(reslst) == 1)

    reslst <- rg$list_resources(filter="tagName eq 'createdBy' and tagValue eq 'AzureR/AzureRMR'")
    expect_true(is.list(reslst) && all(sapply(reslst, is_resource)))

    reslst <- rg$list_resources(filter="resourceType eq 'Microsoft.Storage/storageAccounts'", expand="createdTime")
    expect_true(is.list(reslst))
    expect_true(all(sapply(reslst,
        function(r) is_resource(r) && r$type == "Microsoft.Storage/storageAccounts" && !is_empty(r$ext$createdTime))))
})

test_that("Tag creation inside a function works",
{
    reslst <- rg$list_resources()
    expect_true(length(reslst) > 0)

    res <- reslst[[1]]
    expect_is(res, "az_resource")

    func <- function(obj, value)
    {
        obj$set_tags(test_tag=value)
    }
    expect_silent(func(res, "test value"))
    tags <- res$get_tags()
    expect_true("test_tag" %in% names(tags) && "test value" == tags$test_tag)
})

test_that("Getting a resource with only preview API version works",
{
    type <- "Microsoft.sqlvirtualmachine/sqlvirtualmachines"
    name <- "nonexistent"
    expect_error(expect_warning(rg$get_resource(type=type, name=name), "No stable API versions found"))
})

test_that("Resource deletion works",
{
    reslst <- rg$list_resources()
    restype <- reslst[[1]]$type
    resname <- reslst[[1]]$name

    expect_null(rg$delete_resource(type=restype, name=resname, confirm=FALSE, wait=TRUE))
    expect_false(rg$resource_exists(type=restype, name=resname))
})

rg$delete(confirm=FALSE)
