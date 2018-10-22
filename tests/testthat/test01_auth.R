context("Authentication")

tenant <- Sys.getenv("AZ_TENANT_ID")
app <- Sys.getenv("AZ_APP_ID")
secret <- Sys.getenv("AZ_SECRET")
subscription <- Sys.getenv("AZ_SUBSCRIPTION")

if(tenant == "" || app == "" || secret == "" || subscription == "")
    skip("Authentication tests skipped: ARM credentials not set")


test_that("Authentication works",
{
    az <- az_rm$new(tenant=tenant, app=app, secret=secret)
    expect_is(az, "az_rm")
    expect_true(is_azure_token(az$token))

    creds <- tempfile(fileext=".json")
    writeLines(jsonlite::toJSON(list(tenant=tenant, app=app, secret=secret)), creds)
                        
    az2 <- az_rm$new(config_file=creds)
    expect_is(az2, "az_rm")
    expect_true(is_azure_token(az2$token))

    token <- az2$token

    az3 <- az_rm$new(token=token)
    expect_is(az3, "az_rm")
    expect_true(is_azure_token(az3$token))
})
