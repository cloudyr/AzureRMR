context("RBAC")

tenant <- Sys.getenv("AZ_TEST_TENANT_ID")
app <- Sys.getenv("AZ_TEST_APP_ID")
password <- Sys.getenv("AZ_TEST_PASSWORD")
subscription <- Sys.getenv("AZ_TEST_SUBSCRIPTION")
newsvc_id <- Sys.getenv("AZ_TEST_SVC_PRINCIPAL_ID")

if(tenant == "" || app == "" || password == "" || subscription == "" || newsvc_id == "")
    skip("RBAC method tests skipped: ARM credentials not set")

az <- az_rm$new(tenant, app, password)
sub <- az$get_subscription(subscription)
rgname <- paste(sample(letters, 20, replace=TRUE), collapse="")

test_that("Subscription RBAC works",
{
    defs <- sub$list_role_definitions()
    expect_is(defs, "data.frame")

    defs_lst <- sub$list_role_definitions(as_data_frame=FALSE)
    expect_is(defs_lst, "list")
    expect_true(all(sapply(defs_lst, is_role_definition)))

    asns <- sub$list_role_assignments()
    expect_is(asns, "data.frame")

    asns_lst <- sub$list_role_assignments(as_data_frame=FALSE)
    expect_is(asns_lst, "list")
    expect_true(all(sapply(asns_lst, is_role_assignment)))

    asn <- sub$add_role_assignment(newsvc_id, "reader")
    expect_true(is_role_assignment(asn))

    newasns <- sub$list_role_assignments()
    expect_true(newsvc_id %in% newasns$principal)

    expect_silent(sub$remove_role_assignment(asn$name, confirm=FALSE))
})

test_that("Resource group RBAC works",
{
    expect_false(sub$resource_group_exists(rgname))

    rg <- sub$create_resource_group(rgname, location="australiaeast")

    defs <- rg$list_role_definitions()
    expect_is(defs, "data.frame")

    defs_lst <- rg$list_role_definitions(as_data_frame=FALSE)
    expect_is(defs_lst, "list")
    expect_true(all(sapply(defs_lst, is_role_definition)))

    asns <- sub$list_role_assignments()
    expect_is(asns, "data.frame")

    asns_lst <- rg$list_role_assignments(as_data_frame=FALSE)
    expect_is(asns_lst, "list")
    expect_true(all(sapply(asns_lst, is_role_assignment)))

    asn <- rg$add_role_assignment(newsvc_id, "contributor")
    expect_true(is_role_assignment(asn))

    expect_silent(rg$remove_role_assignment(asn$name, confirm=FALSE))
})

test_that("Resource RBAC works",
{
    restype <- "Microsoft.Storage/storageAccounts"
    resname <- paste(sample(letters, 20, replace=TRUE), collapse="")

    rg <- sub$get_resource_group(rgname)
    res <- rg$create_resource(type=restype, name=resname,
        kind="StorageV2",
        sku=list(name="Standard_LRS", tier="Standard"),
        properties=list(
            accessTier="hot",
            supportsHttpsTrafficOnly=TRUE,
            isHnsEnabled=FALSE
        ),
        wait=TRUE)

    defs <- res$list_role_definitions()
    expect_is(defs, "data.frame")

    defs_lst <- res$list_role_definitions(as_data_frame=FALSE)
    expect_is(defs_lst, "list")
    expect_true(all(sapply(defs_lst, is_role_definition)))

    asns <- sub$list_role_assignments()
    expect_is(asns, "data.frame")

    asns_lst <- res$list_role_assignments(as_data_frame=FALSE)
    expect_is(asns_lst, "list")
    expect_true(all(sapply(asns_lst, is_role_assignment)))

    asn <- res$add_role_assignment(newsvc_id, "owner")
    expect_true(is_role_assignment(asn))

    expect_silent(res$remove_role_assignment(asn$name, confirm=FALSE))
})

sub$delete_resource_group(rgname, confirm=FALSE)

