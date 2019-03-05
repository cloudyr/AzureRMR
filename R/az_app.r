az_app <- R6::R6Class("az_app",

public=list(

    token=NULL,
    tenant=NULL,

    # app data from server
    odata.metadata=NULL,
    odata.type=NULL,
    objectType=NULL,
    objectId=NULL,
    deletionTimestamp=NULL,
    acceptMappedClaims=NULL,
    addIns=NULL,
    appId=NULL,
    appRoles=NULL,
    availableToOtherTenants=NULL,
    displayName=NULL,
    errorUrl=NULL,
    groupMembershipClaims=NULL,
    homepage=NULL,
    identifierUris=NULL,
    informationalUrls=NULL,
    isDeviceOnlyAuthSupported=NULL,
    keyCredentials=NULL,
    knownClientApplications=NULL,
    logoutUrl=NULL,
    `logo@odata.mediaEditLink`=NULL,
    `logo@odata.mediaContentType`=NULL,
    logoUrl=NULL,
    `mainLogo@odata.mediaEditLink`=NULL,
    oauth2AllowIdTokenImplicitFlow=NULL,
    oauth2AllowImplicitFlow=NULL,
    oauth2AllowUrlPathMatching=NULL,
    oauth2Permissions=NULL,
    oauth2RequirePostResponse=NULL,
    optionalClaims=NULL,
    orgRestrictions=NULL,
    parentalControlSettings=NULL,
    passwordCredentials=NULL,
    publicClient=NULL,
    publisherDomain=NULL,
    recordConsentConditions=NULL,
    replyUrls=NULL,
    requiredResourceAccess=NULL,
    samlMetadataUrl=NULL,
    signInAudience=NULL,
    tokenEncryptionKeyId=NULL,

    initialize=function(token, tenant=NULL, object_id=NULL, app_id=NULL, password=NULL, password_duration=1, ...,
                        deployed_properties=list())
    {
        self$token <- token
        self$tenant <- tenant

        parms <- if(!is_empty(list(...)))
            private$init_and_deploy(..., password=password, password_duration=password_duration)
        else if(!is_empty(deployed_properties))
            private$init_from_parms(deployed_properties)
        else private$init_from_host(object_id, app_id)

        # fill in values
        parm_names <- names(parms)
        obj_names <- names(self)
        mapply(function(name, value)
        {
            if(name %in% obj_names)
                self[[name]] <- value
        }, parm_names, parms)

        self
    },

    delete=function(confirm=TRUE)
    {},

    update=function()
    {},

    sync_fields=function()
    {},

    create_service_principal=function()
    {},

    get_service_principal=function()
    {},

    delete_service_principal=function()
    {}
),

private=list(
    
    password=NULL,

    init_and_deploy=function(..., password, password_duration)
    {
        properties <- list(...)
        if(is.null(password) || password != FALSE)
        {
            key <- "awBlAHkAMQA=" # base64/UTF-16LE encoded "key1"
            if(is.null(password))
                password <- paste0(sample(c(letters, LETTERS, 0:9), 50, replace=TRUE), collapse="")

            end_date <- if(is.finite(password_duration))
            {
                now <- as.POSIXlt(Sys.time())
                now$year <- exp_date$year + password_duration
                format(as.POSIXct(now), "%Y-%m-%dT%H:%M:%SZ", tz="GMT")
            }
            else "2299-12-30T13:00:00Z"

            private$password <- password
            properties <- modifyList(properties, list(passwordCredentials=list(
                customKeyIdentifier=key,
                endDate=end_date,
                value=password
            )))
        }

        call_azure_graph(token, self$tenant, "applications", body=properties, encode="json", http_verb="PUT")
    },

    init_from_parms=function(parms)
    {
        parms
    },

    init_from_host=function(object_id, app_id)
    {
        op <- if(is.null(object_id))
            file.path("applicationsByAppId", app_id)
        else file.path("applications", object_id)

        call_azure_graph(token, self$tenant, op)
    }
))
