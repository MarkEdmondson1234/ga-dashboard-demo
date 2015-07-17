
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(dygraphs)
library(d3heatmap)


scriptHeaders <- function(inputId=NULL) {
  tagList(
    singleton(tags$head(tags$script(src = "js/gtm.js")))
  )
}



dashboardPage(skin = "purple",
              dashboardHeader(title = "GA Dashboard"),
              dashboardSidebar(
                sidebarMenu(
                  menuItem("Dashboard", tabName = "dashboard", icon=icon("dashboard")),
                  menuItem("Upload Data", tabName = "join", icon=icon("cloud-upload")),
                  menuItem("Analyse", tabName = "analyse", icon=icon("calculator")),
                  menuItem("Github Source Code", href="https://github.com/MarkEdmondson1234/ga-dashboard-demo", icon = icon("github")),
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
                          h1("Executive Summary"),
                          fluidRow(  #row 1
                            scriptHeaders(),
                            box(width=6, height="200px",title="Select Data", solidHeader = T, status = "primary",
                                selectInput("medium_select", 
                                            "Select Traffic Channel",
                                            choices = c("All" = "total",
                                                        "Direct" = "(none)",
                                                        "Email" = "email",
                                                        "SEO" = "organic",
                                                        "Referral" = "referral",
                                                        "Social" = "social")),
                                helpText("Select which channel to show in the plots below.  For demo purposes it loads sessions, but could easily be revenue, goals or other metrics and segments.")
                            ),
                            box(width=6, height="200px",title = "What is this?", solidHeader = T, status = "info",
                                p("This is a demo Google Analytics dashboard using R, Shiny and various other features."),
                                p("Check out the source code via the Github link to the left. ")
                            )
                          ),
                          h2("   Top Level Trends"),
                          fluidRow(
                            
                            valueBoxOutput("WoW"),
                            valueBoxOutput("MoM"),
                            valueBoxOutput("YoY")
                          ),
                          h2("Plots"),
                          fluidRow(
                            
                            tabBox(title = "", width=12,
                                   tabPanel(title=tagList(shiny::icon("line-chart"), "Trend"),
                                            helpText("Click and drag on the plot to zoom and select date ranges.  Events that have been uploaded to the MySQL backend are shown as dotted lines, and unusual time points are annotated below them."),
                                            selectInput("agg_select", 
                                                        "Select Trend Type",
                                                        choices = c("Day" = "day",
                                                                    "Week" = "week",
                                                                    "Month" = "month",
                                                                    "Annual" = "year"),
                                                        selected = "week"
                                            ),
                                            textOutput("date_shown"),
                                            dygraphOutput("plot1", height = "600px")
                                   ),
                                   tabPanel(title=tagList(shiny::icon("calendar"), "Day Of Week"),
                                            helpText("Zoom in by selecting with the mouse.  This heatmap representation of the data helps show if the day of the week holds any patterns."),
                                            textOutput("current_week"),
                                            d3heatmapOutput("heatmap", height="800px")
                                            
                                   )
                                   
                            )
                          )
                          # ,
                          #                           fluidRow(  #row 2
                          #                             
                          #                           )
                          
                  ),
                  # second tab content
                  tabItem(tabName = "join",
                          h1("Import Data to Join"),
                          fluidRow(
                            box(height="200px",
                                title = "Upload file",
                                solidHeader = T,
                                status ="primary",
                                fileInput("eventUploadFile",
                                          "Marketing Events",
                                          accept = c("text/csv", ".csv")),
                                helpText("Upload a CSV file with two columns: date (YYYY-MM-DD) and eventname (string), to store it in the database. 0.5MB limit.")),
                            box(height="200px",
                                title = "Public Upload", solidHeader=T, status="info",
                                p("For this demo, anyone can load up data into the MySQL database which will appear in the table below.  This data is persistent between Shiny app sessions."),
                                p("These events are then used in the trend graphs and in the Analyse section.  Other applications include uploading CRM data, offline transactions or campaign info to bind to your GA data.")
                            ),
                            box(width = 12, solidHeader = T, status="success",
                                title = "Current Data Loaded",
                                DT::dataTableOutput("eventTable")                
                            )
                            
                            
                          )
                  ),
                  # third tab content
                  tabItem(tabName = "analyse",
                          h2("Deepdive Analysis"),
                          helpText("This section demonstrates some R packages that work with time-series data.  This leverages the strength of using R for dashboarding. "),
                          tabBox("Select Analysis", width = 12,
                                 tabPanel(tagList(shiny::icon("thumbs-up"), "Event Effects"),
                                          helpText("The estimated impact of the uploaded events on total traffic is shown below.  It uses code similar to the GA Effect app linked in the menu to the left."),
                                          uiOutput("multiple_plots")
                                 ),
                                 tabPanel(tagList(shiny::icon("exclamation"), "Anomaly Detection"),
                                          helpText("What dates had unusual activity?  This uses Twitter's AnomalyDetection package to find unusual activity.  Fine tune the dates shown using the controls below."),
                                          selectInput("medium_select2", 
                                                      "Select Plot Channel",
                                                      choices = c("All" = "total",
                                                                  "Direct" = "(none)",
                                                                  "Email" = "email",
                                                                  "SEO" = "organic",
                                                                  "Referral" = "referral",
                                                                  "Social" = "social")
                                          ),
                                          selectInput("agg_select2", 
                                                      "Select Plot Date Type",
                                                      choices = c("Day" = "day",
                                                                  "Week" = "week",
                                                                  "Month" = "month"),
                                                      selected = "day"
                                          ),
                                          sliderInput("max_anoms",
                                                      "Sensitivity",
                                                      min=0.0,
                                                      max=0.4,
                                                      value = 0.1,
                                                      step = 0.05),
                                          plotOutput("anomalyPlot"),
                                          DT::dataTableOutput("anomalyTable")
                                          
                                 )
                          )
                  )
                )
                
              )
)