# Deploy the app for the first time
# rsconnect::deployApp(appDir = "./inst/app",
#                      server = "connect-apps.ceh.ac.uk",
#                      account = "zekmar",
#                      appName = "rmavis", 
#                      appTitle = "RMAVIS")

# Re-deploy the app
rsconnect::deployApp(appDir = "./inst/app",
                     server = "connect-apps.ceh.ac.uk",
                     account = "zekmar",
                     # appName = "rmavis", 
                     # appTitle = "RMAVIS",
                     appId = 321,
                     forceUpdate = TRUE)
