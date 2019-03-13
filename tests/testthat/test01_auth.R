context("Authentication")

tenant <- Sys.getenv("AZ_TEST_TENANT_ID")
app <- Sys.getenv("AZ_TEST_APP_ID")
password <- Sys.getenv("AZ_TEST_PASSWORD")
subscription <- Sys.getenv("AZ_TEST_SUBSCRIPTION")

if(tenant == "" || app == "" || password == "" || subscription == "")
    skip("Authentication tests skipped: ARM credentials not set")


test_that("ARM authentication works",
{
    az <- az_rm$new(tenant=tenant, app=app, password=password)
    expect_is(az, "az_rm")
    expect_true(is_azure_token(az$token))

    creds <- tempfile(fileext=".json")
    writeLines(jsonlite::toJSON(list(tenant=tenant, app=app, password=password)), creds)
                        
    az2 <- az_rm$new(config_file=creds)
    expect_is(az2, "az_rm")
    expect_true(is_azure_token(az2$token))
})

test_that("Graph authentication works",
{
    az <- az_graph$new(tenant=tenant, app=app, password=password)
    expect_is(az, "az_graph")
    expect_true(is_azure_token(az$token))

    creds <- tempfile(fileext=".json")
    writeLines(jsonlite::toJSON(list(tenant=tenant, app=app, password=password)), creds)
                        
    az2 <- az_graph$new(config_file=creds)
    expect_is(az2, "az_graph")
    expect_true(is_azure_token(az2$token))
})

test_that("Login interface works",
{
    expect_silent(delete_azure_login(tenant=tenant, confirm=FALSE))

    lst <- list_azure_logins()
    expect_true(is.list(lst) && is_empty(lst[[tenant]]))

    az3 <- create_azure_login(tenant=tenant, app=app, password=password)
    expect_is(az3, "az_client")

    creds <- tempfile(fileext=".json")
    writeLines(jsonlite::toJSON(list(tenant=tenant, app=app, password=password)), creds)

    az4 <- create_azure_login(config_file=creds)
    expect_is(az4, "az_client")

    az5 <- get_azure_login(tenant)
    expect_is(az5, "az_client")

})

