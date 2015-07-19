# ga-dashboard-demo
A demo on how to build your own Google Analytics dashboard with R, Shiny and MySQL

A guide blogpost here: http://markedmondson.me/enhance-your-google-analytics-data-with-r-and-shiny-free-online-dashboard-template 

Live demo version here: https://mark.shinyapps.io/GA-dashboard-demo

## Intro

Whilst [shinyga()](https://github.com/MarkEdmondson1234/shinyga) lets you create Shiny dashboards that anyone can connect their own GA data with, the more common use case of creating a dashboard to use just your own data is better served by this example.  This template lets you clone and enter your GA id to quick start your own Shiny dashboard.

## Features

* Interactive trend graph.
* Auto-update of GA data for last 3 years.
* Zoomable heatmap for Day of week analysis.
* Year on Year, Month on Month and Last Month vs same Month Last Year.
* MySQL persistant storage for data blending your data with GA data.
* Upload option to update MySQL data stored.
* Analysis of impact of events on your GA data via Google's CausalImpact
* Detection of unusual timepoints using Twitter's AnomalyDetection

## Screenshots

![Trend](https://github.com/MarkEdmondson1234/ga-dashboard-demo/blob/master/ga-dashboard-demo1.png "Trend and heatmap")
![Upload to MySQL](https://github.com/MarkEdmondson1234/ga-dashboard-demo/blob/master/ga-dashboard-demo2.png "Upload to MySQL")
![Analysis of data](https://github.com/MarkEdmondson1234/ga-dashboard-demo/blob/master/ga-dashboard-demo3.png "Data Analysis")


## To Use

1. Clone this repository to your own RStudio project.
2. Get your MySQL setup with a user and IP location, and the GA View ID you want to pull data from. You will also probably need to whitelist the IP of your Shiny Server. Add your local IP for testing too. If you will use shinyapps.io their IPs are:
 - 54.204.29.251
 - 54.204.34.9
 - 54.204.36.75
 - 54.204.37.78
3. Create another file called secrets.r file in the same directory with the below content filled in with your details.  This file is called in functions.r

        # secrets.r
        options(mysql = list(
        "host" = "YOUR SQL IP",
        "port" = 3306,
        "user" = "YOUR SQL USER",
        "password" = "YOUR USER PW",
        "databaseName" = "onlinegashiny"),
        rga = list(
        "profile_id" = "YOUR GA ID",
        "daysBackToFetch" = 356*3
        ),
        shinyMulti = list(
        "max_plots" = 10
        ),
        myCausalImpact = list(
        'test_time' = 14,
        'season' = 7
        ),
        shiny.maxRequestSize = 0.5*1024^2 ## upload only 0.5 MB
        )
4. Install rga() if you need to, then run the below once locally in the same folder to have the app remember your GA OAuth2 settings.
    
        ## Run this locally first, to store the auth token.
        library(rga)
        rga::rga.open(where="token.rga")

5. Run locally with shiny::runApp() or upload to your shinyapps.io account or your own Shiny server. 
6. Customise your instance.
