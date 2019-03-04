az_app <- R6::R6Class("az_app",

public=list(

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

    initialize=function()
    {},

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
))
