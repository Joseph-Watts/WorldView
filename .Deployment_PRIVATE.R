# Deploying app to shinyapps.io

#' Notes: was getting an error when using R 5.6, had to revert to an earlier
#' version to get working on shinyapps.io 

library(rsconnect)

rsconnect::setAccountInfo(name='josephwatts', 
                          token='CBED9DC30B048DD2836336945A88A4C7', 
                          secret='Jyc33GIo3bpBLf1t9/JUxpfVWlJQaJ/lBd/9Xb2o')

deployApp(appName = "worldview",
          forceUpdate = TRUE)
