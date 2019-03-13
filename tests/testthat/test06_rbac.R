context("Graph/RBAC")

tenant <- Sys.getenv("AZ_TEST_TENANT_ID")
app <- Sys.getenv("AZ_TEST_NATIVE_APP_ID")
subscription <- Sys.getenv("AZ_TEST_SUBSCRIPTION")

if(tenant == "" || app == "" || subscription == "")
    skip("Resource group method tests skipped: ARM credentials not set")

az <- get_azure_login(tenant=tenant, selection=app)
sub <- az$get_subscription(subscription)

test_that("App creation works",
{
    newapp_name <- paste0("AzureRtest_", paste0(sample(letters, 5, TRUE), collapse=""))
    newapp <- az$create_app(name=newapp_name, create_service_principal=FALSE)
    newsvc <- newapp$create_service_principal()
    expect_true(is_app(newapp))
    expect_true(is_service_principal(newsvc))

    newapp_id <- newapp$properties$appId
    expect_true(is_app(az$get_app(app_id=newapp_id)))
    expect_true(is_service_principal(az$get_service_principal(app_id=newapp_id)))
    expect_true(is_service_principal(newapp$get_service_principal()))

    Sys.setenv(AZ_TEST_NEWAPP_ID=newapp_id)
})

test_that("Subscription RBAC works",
{
    Sys.sleep(5)
    newapp_id <- Sys.getenv("AZ_TEST_NEWAPP_ID")

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

    newapp <- az$get_app(newapp_id)
    asn <- sub$add_role_assignment(newapp, "reader")
    expect_true(is_role_assignment(asn))

    newsvc <- az$get_service_principal(newapp_id)
    newasns <- sub$list_role_assignments()
    expect_true(newsvc$properties$objectId %in% newasns$principal)
})

test_that("Resource group RBAC works",
{
    newapp_id <- Sys.getenv("AZ_TEST_NEWAPP_ID")
    rgname <- paste(sample(letters, 20, replace=TRUE), collapse="")
    Sys.setenv(AZ_TEST_NEWRG=rgname)

    expect_false(sub$resource_group_exists(rgname))

    rg <- sub$create_resource_group(rgname, location="westus")

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

    newapp <- az$get_app(app_id=newapp_id)
    asn <- rg$add_role_assignment(newapp, "contributor")
    expect_true(is_role_assignment(asn))
})

test_that("App deletion works",
{
    newapp_id <- Sys.getenv("AZ_TEST_NEWAPP_ID")

    expect_silent(az$delete_service_principal(app_id=newapp_id, confirm=FALSE))
    expect_silent(az$delete_app(app_id=newapp_id, confirm=FALSE))

    Sys.sleep(2)
    expect_error(az$get_app(app_id=newapp_id))
})

sub$get_resource_group(Sys.getenv("AZ_TEST_NEWRG"))$delete(confirm=FALSE)
Sys.unsetenv("AZ_TEST_NEWAPP_ID")
Sys.unsetenv("AZ_TEST_NEWRG")
