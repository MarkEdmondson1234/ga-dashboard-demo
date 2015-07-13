# ga-dashboard-demo
A demo on how to build your own Google Analytics dashboard with R, Shiny and MySQL

Live version here: https://mark.shinyapps.io/GA-dashboard-demo

## Intro

Whilst shinyga() lets you create Shiny dashboards that anyone can connect their own GA data with, the more common use case of creating a dashboard to use just your own data is better served by this example.  This template lets you clone and enter your GA id to quick start your own Shiny dashboard.

## Features

* Interactive trend graph.
* Auto-update of GA data for last 3 years.
* Zoomable heatmap for Day of week analysis.
* Year on Year, Month on Month and Last Month vs same Month Last Year.
* MySQL persistant storage for data blending your data with GA data.
* Upload option to update MySQL data stored.
* Analysis of impact of events on your GA data.
