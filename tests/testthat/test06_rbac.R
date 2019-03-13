context("RBAC")

tenant <- Sys.getenv("AZ_TEST_TENANT_ID")
app <- Sys.getenv("AZ_TEST_NATIVE_APP_ID")
subscription <- Sys.getenv("AZ_TEST_SUBSCRIPTION")

if(tenant == "" || app == "" || subscription == "")
    skip("Resource group method tests skipped: ARM credentials not set")

az <- create_azure_login(tenant=tenant, app=app)

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

    Sys.setenv(NEWAPP_ID=newapp_id)
})

test_that("Role assignments work",
{
    newapp_id <- Sys.getenv("NEWAPP_ID")
})

test_that("App deletion works",
{
    newapp_id <- Sys.getenv("NEWAPP_ID")

    expect_silent(az$delete_service_principal(app_id=newapp_id, confirm=FALSE))
    expect_silent(az$delete_app(app_id=newapp_id, confirm=FALSE))

    Sys.sleep(2)
    expect_error(az$get_app(app_id=newapp_id))
})

Sys.unsetenv("NEWAPP_ID")
