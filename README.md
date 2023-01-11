# Netatmo
Netatmo plugin for Vera & openLuup

Recent versions (since v23.1.6) use Oath2 token access authorization:

*    no need for you to create your own App credentials on the dev.netatmo.com site
*    one-off authorization via the Netatmo website to access your own weather station
*    no user credentials stored in the app

After installing, the above one-off authorization is required. 
You can access this via the Authorize button on either the AltUI device page or the openLuup console device page. 
You are redirected to the Netatmo site to login and approve the request. 
After successful acknowledgement, you should restart Vera/openLuup.

A number of previously-existing device variables (all in the Netatmo1 service) will be marked with the value 'nil'. These may be manually deleted:

*    ClientID
*    ClientSecret
*   Username
*    Password
*    TokenRefresh
*    AppMemoryUsed

