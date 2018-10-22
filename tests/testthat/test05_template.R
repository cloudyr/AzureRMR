context("Templates")

tenant <- Sys.getenv("AZ_TENANT_ID")
app <- Sys.getenv("AZ_APP_ID")
secret <- Sys.getenv("AZ_SECRET")
subscription <- Sys.getenv("AZ_SUBSCRIPTION")

if(tenant == "" || app == "" || secret == "" || subscription == "")
    skip("Resource group method tests skipped: ARM credentials not set")

rgname <- paste(sample(letters, 20, replace=TRUE), collapse="")
rg <- az_rm$
    new(tenant=tenant, app=app, secret=secret)$
    get_subscription(subscription)$
    create_resource_group(rgname, location="australiaeast")


test_that("Template methods work",
{
    # precondition
    expect_true(is_empty(rg$list_resources()))

    tplname <- paste(sample(letters, 10, replace=TRUE), collapse="")

    # simple storage account template
    template <- "../resources/template.json"
    parameters <- jsonlite::toJSON(list(
        location=list(value="australiaeast"),
        name=list(value=tplname)
    ), auto_unbox=TRUE)

    tpl <- rg$deploy_template(tplname, template=template, parameters=parameters, wait=TRUE)
    tpl$check()
    expect_is(tpl, "az_template")
    expect_false(is_empty(rg$list_resources()))

    tpl$delete(confirm=FALSE, free_resources=TRUE)
    expect_true(is_empty(rg$list_resources()))

    tplname2 <- paste(sample(letters, 10, replace=TRUE), collapse="")
    tpl_parsed <- jsonlite::fromJSON(template, simplifyVector=FALSE)
    parm_parsed <- list(
        location="australiaeast",
        name=tplname2
    )

    tpl2 <- rg$deploy_template(tplname2, template=tpl_parsed, parameters=parm_parsed)
    tpl2$check()
    expect_is(tpl2, "az_template")
    expect_false(is_empty(rg$list_resources()))

    tpl2$delete(confirm=FALSE, free_resources=TRUE)
    expect_true(is_empty(rg$list_resources()))
})

rg$delete(confirm=FALSE)

#rg <- sub1$create_resource_group(paste(sample(letters, 20, replace=TRUE), collapse=""), location="australiaeast")

#parm_parsed <- list(
    #location="australiaeast",
    #name=paste(sample(letters, 10, replace=TRUE), collapse="")
#)

#tmp <- rg$deploy_template("tmpsa2",
    #template="tests/resources/template.json",
    #parameters=parm_parsed)

