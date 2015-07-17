## functions.R
library(rga)
library(dygraphs)
library(zoo)
library(tidyr)
library(lubridate)
library(d3heatmap)
library(dplyr)
library(stringr)
library(DT)
library(RMySQL)
library(CausalImpact)
library(AnomalyDetection)

# I have the below in a file I source that is not on github
message("functions.R called from ", getwd())
source('secrets.R')
# secrets.R has this in it:
# options(mysql = list(
#   "host" = "xxxx",
#   "port" = 3306,
#   "user" = "shinyapps",
#   "password" = "YOUR USER PW",
#   "databaseName" = "onlinegashiny"),
#   rga = list(
#     "profile_id" = "xxxxxx",
#     "daysBackToFetch" = 356*3
#   ),
#   shinyMulti = list(
#     "max_plots" = 10
#   ),
#   myCausalImpact = list(
#     'test_time' = 14,
#     'season' = 7
#   ),
#   shiny.maxRequestSize = 0.5*1024^2 ## upload only 0.5 MB
# )

## Run this locally first, to store the auth token.
## this is then uploaded with the shiny app for future requests.
# rga.open(where="token.rga")

get_ga_data <- function(profileID, 
                        fetch_metrics, 
                        fetch_dimensions,
                        fetch_filter = ""){
  
  ## Run this locally first, to store the auth token.
  rga.open(where="token.rga")
  
  all_start <-  ga$getFirstDate(profileID)
  start <- today() - options()$rga$daysBackToFetch
  yesterday <- today() -1
  
  message("# Fetching GA data")
  ga_data <- ga$getData(ids = profileID,
                        start.date = start,
                        end.date = yesterday,
                        metrics = fetch_metrics,
                        dimensions = fetch_dimensions,
                        filters = fetch_filter,
                        batch = T)
  
  return(ga_data)
  
}

## Twitter's AnomalyDetection
## https://github.com/twitter/AnomalyDetection
anomalyDetect <- function(data, ...){
  message("Anomaly detection")
  if("date" != names(data)[1]){
    stop("'date' must be in first column of data")
  }
  
  if(ncol(data) > 2){
    warning("More than two columns detected in data, only first that isn't 'date' is used")
  }
  
  data <- data[,1:2]
  
  data$date <- as.POSIXct(data$date)
  data[is.na(data[,2]),2] <- 0
  
  a_result <- AnomalyDetectionTs(data, plot = T, ...)
  
}


aggregate_data <- function(data, agg_period){
  
  if("date" != names(data)[1]){
    stop("'date' must be in first column of data")
  }
  
  if(ncol(data) > 2){
    warning("More than two columns detected in data, only first that isn't 'date' is used")
  }
  
  agg_data <- data[,1:2]
  
  
  ## aggregate data if not agg == date
  if(agg_period %in% c('week', 'month', 'year')){
    old_names <- names(agg_data)
    names(agg_data) <- c("date","metric")
    agg_data <- tbl_df(agg_data)
    date_type_function <- period_function_generator(agg_period, pad=T)
    
    agg_data <-  agg_data %>% 
      mutate(period_type = paste0(year(date),
                                  "_",
                                  date_type_function(date))) %>%
      group_by(period_type) %>%
      dplyr::summarise(date = min(date),
                       metric = sum(metric))
    
    agg_data <- data.frame(agg_data)
    names(agg_data) <- c(agg_period, old_names)
    agg_data$date <- as.Date(agg_data$date)
  } else {
    
  }
  
  agg_data
  
}



## utility to create a time period finder on a date
## outputs a function you use on Date objects
period_function_generator <- function(period, pad=FALSE){
  
  if(!(period %in% c("month", "week", "year", "monthYear"))){
    stop("time_period must be one of 'week', 'month', 'monthYear', or 'year'")
  }
  
  if(period == "month" | period == "monthYear"){
    f <- month
  } else if (period == "week" | period == "weekYear"){
    f <- week
  } else if(period == "year"){
    f <- year
  }
  
  if(pad){
    
    function(x){
      gsub(" ","0", sprintf("%2d",f(x)))
    } 
    
  } else {
    f
  }    
  
}

lag_time <- function(period, amount=1L, data_date = Sys.Date()){
  
  if(period %in% c("month", "monthYear")){
    data_date %m-% months(amount)
  } else if (period == "week"){
    data_date - 7*amount
  } else if(period == "year"){
    data_date %m-% years(amount)
  } 
  
}

calcPeriodChange <- function (data, time_period) {
  
  if("date" != names(data)[1]){
    stop("'date' must be in first column of data")
  }
  
  if(ncol(data) > 2){
    warning("More than two columns detected in data, only first that isn't 'date' is used")
  }
  
  if(!(time_period %in% c("week", "month", "year", "monthYear"))){
    stop("time_period must be one of 'week', 'month', 'monthYear', or 'year'")
  }
  
  period_f <- period_function_generator(time_period)
  
  ## make the period column
  data$period    <- period_f(data$date)
  now_period     <- period_f(today())
  
  lag_one_period <- period_f(lag_time(time_period, 1))
  lag_one_total <- sum(data[data$period == lag_one_period &
                              year(data$date) == year(today()),2], 
                       na.rm=T)
  
  ## what periods to compare
  if(time_period %in% c("week", "month", "year")){
    lag_two_period <- period_f(lag_time(time_period, 2))
    ## calculate % difference for that period
    
    lag_two_total <- sum(data[data$period == lag_two_period &
                                year(data$date) == year(today()),2], 
                         na.rm=T)
    
  } else if(time_period == "monthYear") {
    lag_two_period <- period_f(lag_time(time_period, 13))
    lag_two_total <- sum(data[data$period == lag_two_period &
                                year(data$date) == year(lag_time(time_period, 13)),2], 
                         na.rm=T)
  }
  
  diff_period <- (lag_one_total - lag_two_total) / 
    (lag_one_total + lag_two_total)
}



valueBoxTimeOnTime <- function(data, time_period="month"){
  
  diff_period <- calcPeriodChange(data, time_period)
  
  wentUp <- ifelse(diff_period > 0, TRUE, FALSE)
  
  ## decide what to put in the box
  if(!is.na(wentUp)){
    diff_period <- paste(round(diff_period, 2)*100, "%")
    
    icon_vb <- ifelse(wentUp, "arrow-up", "arrow-down")
    color_vb <- ifelse(wentUp, "olive", "maroon")
  } else { ## period was invalid, default to nothing
    diff_period <- "-"
    icon_vb <- "exclamation"
    color_vb <- "teal"
  }
  
  if(time_period=="monthYear"){
    sub_title_name <- "Last Month vs Same Month Last Year"
  } else {
    sub_title_name <- str_to_title(paste(time_period,"on",time_period))
  }
  
  ## output a shinydashboard valueBox for use in ui.r
  valueBox(
    value = diff_period,
    subtitle = sub_title_name,
    icon = icon(icon_vb),
    color = color_vb
  )
  
  
}



#### MySQL functions

createTable <- function(table_name, data_for_table){
  require(RMySQL)
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  
  if(dbExistsTable(conn, table_name)){
    message("Table Exisits: ", table_name)
  } else {
    message("Creating Table: ", table_name)
    dbWriteTable(conn, table_name, value=as.data.frame(data_for_table))
    dbDisconnect(conn)
  }
  
}

overWriteTable <- function(table_name, data_for_table){
  require(RMySQL)
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  
  if(!dbExistsTable(conn, table_name)){
    message("Table Does Not Exisit, Creating: ", table_name)
    createTable(table_name, data_for_table)
  } else {
    message("Overwriting Table: ", table_name)
    dbRemoveTable(conn, table_name)
    createTable(table_name, data_for_table)
  }
  
}

loadData <- function(table_name) {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", table_name)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data[,-1]
}

is.error <- function(x) inherits(x, "try-error")

### CausalImpact Loop

getCausalImpactList <- function(ts_data, events){
  
  start <- min(zoo::index(ts_data))
  last  <- max(zoo::index(ts_data))
  
  ci_list <- apply(events, 1, function(row) {
    event_date <- as.Date(row['date'])
    event_label <- row['eventname']
    
    message("Modelling: ", event_label, as.character(event_date))
    
    if((event_date - start) > (options()$myCausalImpact$test_time * 5) ){
      start <- event_date - options()$myCausalImpact$test_time * 5
    }
    
    if((last - event_date) < options()$myCausalImpact$test_time ){
      end <- last
    } else {
      end   <- event_date + options()$myCausalImpact$test_time
    }
    
    pre.period <- c(start, event_date -1)
    post.period <- c(event_date, end)
    
    ts_data <- window(ts_data, start = start, end = end)
    
    ci <- CausalImpact(ts_data, 
                       pre.period, 
                       post.period,
                       model.args = list(nseasons = options()$myCausalImpact$season))
  })
  
  names(ci_list) <- events$eventname 
  
  ci_list
  
}     


