context("AzureToken")

tenant <- Sys.getenv("AZ_TEST_TENANT_ID")
app <- Sys.getenv("AZ_TEST_APP_ID")
password <- Sys.getenv("AZ_TEST_PASSWORD")
subscription <- Sys.getenv("AZ_TEST_SUBSCRIPTION")

if(tenant == "" || app == "" || password == "" || subscription == "")
    skip("Authentication tests skipped: ARM credentials not set")


test_that("normalize_tenant, normalize_guid work",
{
    guid <- "abcdefab-1234-5678-9012-abcdefabcdef"
    expect_identical(normalize_guid(guid), guid)
    guid2 <- paste0("{", guid, "}")
    expect_identical(normalize_guid(guid2), guid)
    guid3 <- paste0("(", guid, ")")
    expect_identical(normalize_guid(guid3), guid)
    guid4 <- sub("-", "", guid, fixed=TRUE)
    expect_identical(normalize_guid(guid4), guid)

    # improperly formatted GUID will be treated as a name
    guid5 <- paste0("(", guid)
    expect_identical(normalize_tenant(guid5), paste0(guid5, ".onmicrosoft.com"))

    expect_identical(normalize_tenant("common"), "common")
    expect_identical(normalize_tenant("mytenant"), "mytenant.onmicrosoft.com")
    expect_identical(normalize_tenant("mytenant.com"), "mytenant.com")
})

test_that("Authentication works",
{
    suppressWarnings(file.remove(dir(AzureRMR:::config_dir(), full.names=TRUE)))

    token <- get_azure_token("https://management.azure.com/", tenant, app, password)
    expect_true(is_azure_token(token))

    toklist <- list_azure_tokens()
    hash <- AzureRMR:::token_hash(
        "https://management.azure.com/", tenant, app, password, username=NULL,
        auth_type="client_credentials",
        aad_host="https://login.microsoftonline.com/")
    expect_true(length(toklist) > 0)
    expect_true(hash %in% names(toklist))

    expect_true(is_azure_token(token$refresh()))
    expect_null(delete_azure_token("https://management.azure.com/", tenant, app, password, confirm=FALSE))

    token <- get_azure_token("https://management.azure.com/", tenant, app, password)
    expect_null(delete_azure_token(hash=hash, confirm=FALSE))
})
