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

    initialize=function(token, tenant=NULL, object_id=NULL, app_id=NULL, password=NULL, ...,
                        deployed_properties=list(), api_version=getOption("azure_graph_api_version"))
    {
        self$token <- token
        self$tenant <- tenant
        private$api_version <- api_version

        parms <- if(!is_empty(list(...)))
            private$init_and_deploy(..., password=password)
        else if(!is_empty(deployed_properties))
            private$init_from_parms(deployed_properties)
        else private$init_from_host(tenant, object_id, app_id)

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
    
    api_version=NULL,

    init_and_deploy=function(...)
    {},

    init_from_parms=function(parms)
    {
        parms
    },

    init_from_host=function(tenant, object_id, app_id)
    {}
))
