
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

source('functions.R')

## for the loop over plots
max_plots <- options()$shinyMulti$max_plots

shinyServer(function(input, output, session) {
  
  message(getwd())

  ga_data <- reactive({
    
    ## get your profile ID of your GA account to pull from
    gadata <- get_ga_data(profileID = options()$rga$profile_id, 
                          fetch_metrics = "ga:sessions",
                          fetch_dimensions = "ga:date,ga:medium",
                          fetch_filter="")
    
    # get one column per medium plus a total
    data <- tidyr::spread(gadata, medium, sessions)
    data$total <- rowSums(as.matrix(data[,-1]), na.rm=T)
    
    data
    
  })
  
  output$date_shown <- renderText({

    pdata <- plot_date_data()
    
    min_date <- as.character(min(pdata$date))
    max_date <- as.character(max(pdata$date))
    
    paste0(min_date, " to ", max_date)
    
  })
  
  plot_date_data <- reactive({
    data <- ga_data()
    if(is.null(data)){
      return(NULL)
    }
    
    choice <- input$medium_select
    plot1_dates <- input$plot1_date_window
    
    min_date <- plot1_dates[1]
    max_date <- plot1_dates[2]
    
    pdata <- data[data$date > min_date &
                  data$date < max_date,]
    
    pdata <- pdata[,colnames(pdata) %in% c("date", choice)]
    
    
  })
  
  output$heatmap <- renderD3heatmap({
    validate(
      need(is.null(plot_date_data()), "Plot data")
    )
    
    hm_data <- plot_date_data()
    
    week_pad <- period_function_generator("week", pad=T)
    
    hm_data$wday <- lubridate::wday(hm_data$date, label=T)
    
    hm_data$week <- paste0(year(hm_data$date), 
                           " W", 
                           week_pad(hm_data$date)
                           )
    
    names(hm_data) <- c("date", "sessions", "wday", "week")
    
    hm_data <- tbl_df(hm_data)
    
    hm_f <- tidyr::spread(hm_data[,c("sessions","wday","week")], 
                          wday, 
                          sessions)
    hm_m <- as.matrix(hm_f %>% dplyr::select(-week))
    row.names(hm_m) <- factor(hm_f$week)
    
    hm_m[is.na(hm_m)] <- 0
    
    d3heatmap(hm_m, 
              colors = "Blues",
              Rowv = FALSE,
              Colv = FALSE,
              labRow = row.names(hm_m),
              labCol = colnames(hm_m)
              )
    
    
    
  })
  
  output$current_week <- renderText({
    
    paste("Currently Week", week(today()))
    
  })
  
  output$plot1 <- renderDygraph({

    data   <- ga_data()
    choice <- input$medium_select
    agg    <- input$agg_select
    events <- eventData()
    
    agg_data <- data[,c('date', choice)]
    names(agg_data) <- c('date', 'metric')
    
    ## aggregate data if not agg == date
    if(agg %in% c('week', 'month', 'year')){
      agg_data <- tbl_df(agg_data)
      date_type_function <- period_function_generator(agg, pad=T)
      
      agg_data <-  agg_data %>% 
        mutate(period_type = paste0(year(date),
                                    "_",
                                   date_type_function(date))) %>%
        group_by(period_type) %>%
        summarise(date = min(date),
                  metric = sum(metric))
      
      agg_data <- data.frame(agg_data)
    }
    
    ## dygraph needs a time-series, zoo makes it easier
    ts_data <- zoo(agg_data[,'metric'], 
                   order.by = agg_data[,'date'])
    
    start <- Sys.Date() - 450
    end <- Sys.Date() - 1
      
    d <- dygraph(ts_data, main=str_to_title(paste(choice, "Sessions "))) %>%
      dyRangeSelector(dateWindow = c(start, end)) %>%
      dySeries("V1", color = "#8a48a4", label = str_to_title(choice) )
    
    ## a dreaded for loop, but it makes sense for this?
    if(!is.null(events)){
      for(i in 1:length(events$date)){
        d <- d %>% dyEvent(events$date[i], label = events$eventname[i]) 
      }
    }

    return(d)
    
  })
  
  output$WoW <- renderValueBox({
    
    data <- ga_data()
    choice <- input$medium_select
    
    wow_data <- data[,c('date', choice)]
    
    valueBoxTimeOnTime(wow_data, "week")
  
  })
  
  output$MoM <- renderValueBox({
    
    data <- ga_data()
    choice <- input$medium_select
    
    mom_data <- data[,c('date', choice)]
    
    valueBoxTimeOnTime(mom_data, "month")

    
  })
  
  output$YoY <- renderValueBox({
    
    data <- ga_data()
    choice <- input$medium_select
    
    yoy_data <- data[,c('date', choice)]
    
    valueBoxTimeOnTime(yoy_data, "monthYear")
    
    
  })
  
#   eventData <- reactive({
#     eventUploaded <- input$eventUploadFile
#     
#     if(is.null(eventUploaded)){
#       return(NULL)
#     }
#     
#     read.csv(eventUploaded$datapath)
#     
#     
#   })
  
  eventData <- reactive({
    eventUploaded <- input$eventUploadFile
    
    if(is.null(eventUploaded)){
      ## get it from the SQL database instead
      uploaded_csv <- try(loadData("onlineGAshiny_events"))
      
      if(!is.error(uploaded_csv)){
        return(uploaded_csv)        
      } else {
        message("No event data found in SQL")
        return(NULL)
      }

      
    } else {
      uploaded_csv <- try(read.csv(eventUploaded$datapath, stringsAsFactors = F))
      
      if(!is.error(uploaded_csv)){
        ## check uploaded_csv
        
        if(all(names(uploaded_csv) %in% c('date', 'eventname'))){
          
          uploaded_csv <- uploaded_csv[complete.cases(uploaded_csv),]
          
          ## convert dates
          dates_guessed <- as.Date(uploaded_csv$date,
                                   guess_formats(uploaded_csv$date, 
                                                 c("Y-m-d", "m-d-Y")))
          message("Dates: ", dates_guessed)
          uploaded_csv$date <- dates_guessed
          
          uploadBool <- overWriteTable("onlineGAshiny_events", uploaded_csv)
          
          if(uploadBool) message("File uploaded successfully.")
          
          return(uploaded_csv)          
          
        } else {
          stop("File did not include 'date' and 'eventname' columns.  Found: ", names(uploaded_csv))
        }       
        
      } else {
        message("Problem uploading file.")
        return(NULL)
      }
    }
    
  })
  
  output$eventTable <- DT::renderDataTable({
    
    eventData()
    
  })
  
  casualImpactData <- reactive({
    
    ci       <- ga_data()
    events   <- eventData()
    
    ## dygraph needs a time-series, zoo makes it easier
    ts_data <- zoo(ci[,'total'], 
                   order.by = ci[,'date'])
    
    ci_list <- getCausalImpactList(ts_data, events)
    
    ## data via lapply(ci_list, function(x) summary(x[[1]]))
 
  })
  
  ## loop over possible events
  output$multiple_plots <- renderUI({
    events <- eventData()
    
    if(!is.null(events)){
      ci_sig_output_list <- lapply(1:nrow(events), function(i){
        ci_sig_name <- paste("ci_sig", i, sep="")
        valueBoxOutput(ci_sig_name)
      })
      
      ci_rel_output_list <- lapply(1:nrow(events), function(i){
        ci_sig_name <- paste("ci_rel", i, sep="")
        valueBoxOutput(ci_sig_name)
      })
      
      null_plot_list <- lapply(1:nrow(events), function(i){
        null_plot_name <- paste("null_plot", i, sep="")
        box(width = 8, dygraphOutput(null_plot_name, height = "300px"))
      })
      
      for(i in 1:nrow(events)){
        add <- c(null_plot_list[i], ci_sig_output_list[i],ci_rel_output_list[i] )
        if(i>1){
          output_list <- c(output_list, add)        
        } else {
          output_list <- add
        }
        
      }
      
      all_the_plots <- do.call(tagList, output_list)
      
      return(fluidRow(all_the_plots))     
      
    } else {
      
    return(NULL)
      
    }
    
 
  })
  

  ### loops over events
  for(i in 1:max_plots){
    #local needed to have reevaluation of i
    local({
      my_i <- i

      ## the dynamic output for this Box     
      ci_sig_name <- paste("ci_sig", i, sep="")
      output[[ci_sig_name]] <- renderValueBox({
        ci_l <- casualImpactData()
        events <- eventData()
        ## accessing the CI list summary for this event
        ci <- ci_l[[events$eventname[my_i]]]$summary
        
        if(!is.null(ci)){
          
          if(ci$p[1] < 0.05){
            valueBox(
              "Significant", 
              "The probability of obtaining this effect by chance alone is small",
              icon = icon("thumbs-o-up"),
              color = "green"
            )
          } else {
            valueBox(
              "Not Significant", 
              "Its likely that any effect observed is just by chance",
              icon = icon("thumbs-o-down"),
              color = "red"
            )
          }
        } else {
          valueBox(
            "No data", 
            ""
          )
          
        }
      })
      
      ## dynamic output for Box 2
      ci_rel_name <- paste("ci_rel", i, sep="")
      output[[ci_rel_name]] <- renderValueBox({
        ci_l <- casualImpactData()
        events <- eventData()
        ## accessing the CI list summary for this event
        ci <- ci_l[[events$eventname[my_i]]]$summary
        
        if(!is.null(ci)){
          relEffect <- round(ci$RelEffect[2], 2) * 100
          
          valueBox(
            paste(as.character(relEffect), "%"), 
            paste("Estimated Average % Change"),
            icon = icon("pie-chart"),
            color = "blue"
          )
        }else {
          valueBox(
            "No data", 
            ""
          )
          
        }
      })
      
      ## dynamic output for plot result
      null_plot_name <- paste("null_plot", i, sep="")
      output[[null_plot_name]] <- renderDygraph({
        
        ci_l <- casualImpactData()
        events <- eventData()
        ci <- ci_l[[events$eventname[my_i]]]$series
        
        event_date <- as.Date(events$date[my_i])
        
        start <- event_date - options()$myCausalImpact$test_time * 4
        enddd <- event_date + options()$myCausalImpact$test_time
        
        dygraph(data=ci[,c('response', 'point.pred', 'point.pred.lower', 'point.pred.upper')], 
                main=paste("Impact of Event:", str_to_title(events$eventname[my_i]))) %>%
          dyEvent(date = event_date, events$eventname[my_i]) %>%
          dySeries(c('point.pred.lower', 'point.pred','point.pred.upper'), label='Expected') %>%
          dySeries('response', label="Observed")
      })
      
      
    }) # end local
  } # end loop over plots
  
  



})
