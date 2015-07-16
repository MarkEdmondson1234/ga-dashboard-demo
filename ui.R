
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(dygraphs)
library(d3heatmap)


dashboardPage(skin = "purple",
  dashboardHeader(title = "GA Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon=icon("dashboard")),
      menuItem("Upload Data", tabName = "join", icon=icon("upload")),
      menuItem("Analyse", tabName = "analyse", icon=icon("calculator")),
      menuItem("Github Source Code", href="https://github.com/MarkEdmondson1234", icon = icon("github")),
      menuItem("Say Hello :-", href="#", newtab=F),
      menuSubItem("@HoloMarkeD", href="http://twitter.com/HoloMarkeD", icon = icon("twitter")),
      menuSubItem("LinkedIn", href="http://dk.linkedin.com/in/markpeteredmondson", icon = icon("linkedin")),
      menuSubItem("Blog", href="http://markedmondson.me/?utm_source=shinyapps&utm_medium=referral&utm_content=sidebar&utm_campaign=ga-dash-demo", icon=icon("hand-o-right")),
      menuItem("Other Apps :-", href="#", newtab=F),
      menuSubItem("GA Effect", href="https://gallery.shinyapps.io/ga-effect/", icon = icon("line-chart")),
      menuSubItem("GA Rollup", href="https://mark.shinyapps.io/ga-rollup/", icon = icon("area-chart")),
      menuSubItem("GA Meta", href="https://mark.shinyapps.io/ga-meta/", icon = icon("sitemap"))
    )
    
    
  ),
  dashboardBody(
    tabItems(
      # first tab content
      tabItem(tabName = "dashboard",
          h2("Dashboard Plots"),
          fluidRow(  #row 1
                box(width=4,
                  selectInput("medium_select", 
                              "Select Plot Channel",
                              choices = c("All" = "total",
                                          "Direct" = "(none)",
                                          "Email" = "email",
                                          "SEO" = "organic",
                                          "Referral" = "referral",
                                          "Social" = "social"))
                ),
                box(width=4,
                  selectInput("agg_select", 
                              "Select Plot Date Type",
                              choices = c("Day" = "day",
                                          "Week" = "week",
                                          "Month" = "month",
                                          "Annual" = "year"),
                              selected = "week"
                              )
                )
          ),
          fluidRow(  #row 2
            box(width=12,
                textOutput("date_shown"),
                dygraphOutput("plot1"))
            ),
          fluidRow(
            valueBoxOutput("WoW"),
            valueBoxOutput("MoM"),
            valueBoxOutput("YoY")
          ),
          fluidRow( #row 3
            box(width=12,
                title="Day of Week Split By Week",
                textOutput("current_week"),
                d3heatmapOutput("heatmap", height="800px")
            )
          )
      ),
      # second tab content
      tabItem(tabName = "join",
         h2("Import Data to Join"),
         fluidRow(
              box(fileInput("eventUploadFile",
                            "Marketing Events",
                            accept = c("text/csv", ".csv")),
                  helpText("Upload a CSV file with two columns: date (YYYY-MM-DD) and eventname (string), to store it in the database. 0.5MB limit.")),
              box(width = 12,
                  title = "Current Data Loaded",
                DT::dataTableOutput("eventTable")                
              )

              
              )
      ),
      # third tab content
      tabItem(tabName = "analyse",
         h2("Analysis On Your Data"),
           tabBox("Select Analysis", width = 12,
             tabPanel("Event Effects",
               p("The estimated impact of the uploaded events on total traffic is shown below."),
               uiOutput("multiple_plots")
             ),
             tabPanel("Anomaly Detection",
                      p("What dates had unusual activity?"),
                      plotOutput("anomalyPlot"),
                      DT::dataTableOutput("anomalyTable")
                      
                      )
           )
      )
    )
    
  )
)