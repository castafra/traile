
na.lomf <- function(x) {
  
  na.lomf.0 <- function(x) {
    non.na.idx <- which(!is.na(x))
    if (is.na(x[1L])) {
      non.na.idx <- c(1L, non.na.idx)
    }
    rep.int(x[non.na.idx], diff(c(non.na.idx, length(x) + 1L)))
  }
  
  dim.len <- length(dim(x))
  
  if (dim.len == 0L) {
    na.lomf.0(x)
  } else {
    apply(x, dim.len, na.lomf.0)
  }
}




map_ui <- function(id, label = "rideMapMod") {
  ns <- NS(id)
  fluidPage(
  fluidRow(
    column(width = 2, offset = 0,
           box(width = 12,
               selectizeInput(ns("user"), label = "Localize User", selected = 'All', choices = c("All", Tracker_fleet$username)),
               tags$head(
                 tags$style(HTML('#rideMapMod-track{background-color: #0E6655; 
                                 color: #B8C7CE}')), 
                 tags$style(HTML('#button_1{background-color: #FF8B8B; 
                                 color: #000000}')),
                 tags$style(HTML('#button_resc_1{background-color: #7AF8FF; 
                                 color: #000000}'))
               ),
               actionButton(ns("track"), label = "Refresh")
           )
    ),
    column(width = 10, offset = 0,
           box(width = 12,
               leafletOutput(ns("track_map"))
           )
    )
    
  ),
  fluidRow(
    column(width = 12, offset = 0,
           dataTableOutput(ns('runner_focus'))
    )
  ),
  fluidRow(
    column(width = 12, offset = 5,
           tags$head(
             tags$style(
               tags$style(HTML('#rideMapMod-urgency{background-color: #FF8B8B !important; 
                                 color: #000000}'))
             )
           ),
               actionButton(ns("urgency"), label = "Send Alert To Everyone")
    )
  )
  )
}

map_server <- function(input, output, session) {
  ns = session$ns
  
  shinyInput <- function(FUN, len, id, ns, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  user_information_reactive = reactiveValues()
  user_information_reactive$state = vector(mode="character", length=nrow(Tracker_fleet))
  user_information_reactive$pace = vector(mode="character", length=nrow(Tracker_fleet))
  user_information_reactive$heart_rate = vector(mode="character", length=nrow(Tracker_fleet))
  user_information_reactive$username = Tracker_fleet$username
  user_information_reactive$longitude = vector(mode="numeric", length=nrow(Tracker_fleet))
  user_information_reactive$latitude = vector(mode="numeric", length=nrow(Tracker_fleet))
  user_information_reactive$altitude = vector(mode="numeric", length=nrow(Tracker_fleet))
  user_information_reactive$old_longitude = vector(mode="numeric", length=nrow(Tracker_fleet))
  user_information_reactive$old_latitude = vector(mode="numeric", length=nrow(Tracker_fleet))
  
  
  observeEvent({input$track
    input$user}, {
    for(i in 1:nrow(Tracker_fleet)){
      TSdata_raw_i = jsonlite::fromJSON(paste0("https://api.thingspeak.com/channels/",Tracker_fleet$channel_id[i],"/feeds.json?api_key=",Tracker_fleet$read_key[i]))
      TSdata_i = TSdata_raw_i$feeds
      TSdata_i = data.frame(na.lomf(TSdata_i))
      TSdata_i = TSdata_i[nrow(TSdata_i),]
      
      Tracker_fleet$longitude[i] = as.numeric(TSdata_i$field1)
      Tracker_fleet$latitude[i] = as.numeric(TSdata_i$field2)
      Tracker_fleet$pace[i] = as.numeric(TSdata_i$field3)
      Tracker_fleet$heart_rate[i] = as.numeric(TSdata_i$field4)
      Tracker_fleet$in_danger[i] = c("Ok", "In Danger")[as.integer(TSdata_i$field5) + 1]
      user_information_reactive$state[i] = Tracker_fleet$in_danger[i]
      user_information_reactive$pace[i] = Tracker_fleet$pace[i]
      user_information_reactive$heart_rate[i] = Tracker_fleet$heart_rate[i]
      user_information_reactive$old_longitude[i] = user_information_reactive$longitude[i]
      user_information_reactive$old_latitude[i] = user_information_reactive$longitude[i]
      user_information_reactive$longitude[i] = Tracker_fleet$longitude[i]
      user_information_reactive$longitude[i] = Tracker_fleet$latitude[i]
      Tracker_fleet$old_longitude[i] = user_information_reactive$old_longitude[i]
      Tracker_fleet$old_latitude[i] = user_information_reactive$old_latitude[i]
      
      Tracker_fleet_to_show = Tracker_fleet
      print(Tracker_fleet_to_show)
      if(input$user != 'All'){
        Tracker_fleet_to_show = Tracker_fleet[Tracker_fleet == input$user,]
      }
      
      Tracker_fleet_to_show=filter(Tracker_fleet_to_show, latitude != "NA") # removing NA values
      
      # new column for the popup label
      
      Tracker_fleet_to_show <- mutate(Tracker_fleet_to_show, cntnt=paste0('<strong>Name: </strong>',name,
                                                          '<br><strong>Surname:</strong> ', surname,
                                                          '<br><strong>Sex:</strong> ', sex,
                                                          '<br><strong>Age:</strong> ', age,
                                                          '<br><strong>Pace:</strong> ',pace), cntnt_hover = paste0(name,' ',surname)) 
      
      # create a color paletter for category type in the data file
      
      pal <- colorFactor(pal = c("#FF2D00", "#0025FF" ), domain = c("Ok", "In Danger"))
      
      #print(Tracker_fleet_to_show)
      #Tracker_fleet$Latitude <-  as.numeric(Tracker_fleet$Latitude)
      #Tracker_fleet$Longitude <-  as.numeric(Tracker_fleet$Longitude)
      
      # create the leaflet map  
      output$track_map <- renderLeaflet({
        leaflet(Tracker_fleet_to_show) %>% 
          addCircles(lng = ~longitude, lat = ~latitude) %>% 
          addTiles() %>%
          addCircleMarkers(data = Tracker_fleet_to_show, lat =  ~latitude, lng =~longitude, 
                           radius = 5, label = ~as.character(cntnt_hover), layerId = ~username,  
                           color = ~pal(in_danger),
                           stroke = FALSE, fillOpacity = 0.8)%>%
          addLegend(pal=pal, values=Tracker_fleet_to_show$in_danger,opacity=1, na.label = "Not Available")%>%
          addEasyButton(easyButton(
            icon="fa-crosshairs", title="ME",
            onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
      })
      
      output$runner_focus <- NULL
    }
    
  })
  
  
  
  
  observeEvent(input$track_map_marker_click, { 
    clicked <- input$track_map_marker_click
    state = user_information_reactive$state[user_information_reactive$username == clicked$id]
    if(state == "Ok") {
      user_data_table <- reactive({
        tibble::tibble(
          Name = Tracker_fleet$name[Tracker_fleet$username == clicked$id],
          Surname = Tracker_fleet$surname[Tracker_fleet$username == clicked$id],
          Pace = user_information_reactive$pace[user_information_reactive$username == clicked$id],
          Heart_rate = user_information_reactive$heart_rate[user_information_reactive$username == clicked$id],
          Action = shinyInput(
            actionButton,
            1,
            'button_',
            label = "Send Alert",
            class = 'alert_button',
            onclick = paste0(
              'Shiny.onInputChange(\"' ,
              ns("send_user_alert"),
              '\", this.id)'
            )
          )
        )
      })
    }
    if(state== "In Danger"){
      user_data_table <- reactive({
        tibble::tibble(
          Name = Tracker_fleet$name[Tracker_fleet$username == clicked$id],
          Surname = Tracker_fleet$surname[Tracker_fleet$username == clicked$id],
          Pace = user_information_reactive$pace[user_information_reactive$username == clicked$id],
          Heart_rate = user_information_reactive$heart_rate[user_information_reactive$username == clicked$id],
          Action = shinyInput(
            actionButton,
            1,
            'button_resc_',
            label = "Rescue",
            class = 'rescue_button',
            onclick = paste0(
              'Shiny.onInputChange(\"' ,
              ns("send_user_rescue"),
              '\", this.id)'
            )
          )
        )
      })
    }
    
    output$runner_focus <- DT::renderDataTable({
      return(user_data_table())
    }, escape = FALSE)
  })
  
  observeEvent(input$send_user_alert, {
    user_to_alert = input$track_map_marker_click$id
    key = Tracker_fleet[Tracker_fleet$username == user_to_alert,"write_key"]
    for (k in 1:20) { 
      print(user_to_alert)
      print(key)
      GET(paste0('https://api.thingspeak.com/update?api_key=',key,'&field6=1'))
      Sys.sleep(1)
    }
    
  })
  
  observeEvent(input$send_user_rescue, {
    user_to_alert = input$track_map_marker_click$id
    key = Tracker_fleet[Tracker_fleet$username == user_to_alert,"write_key"]
    for (k in 1:20) { 
      print(paste0('rescue',user_to_alert))
      print(key)
      GET(paste0('https://api.thingspeak.com/update?api_key=',key,'&field6=2'))
      Sys.sleep(1)
    }
    #???print(paste0('https://api.thingspeak.com/update?api_key=',key,'&field6=2'))
    #selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    #myValue$check <<- paste('click on ',my_data_table()[selectedRow,1])
  })
  
  observeEvent(input$urgency, {
    for(i in 1:nrow(Tracker_fleet)){
      key_i = Tracker_fleet$write_key[i]
      
      for (k in 1:20) { 
        print(key_i)
        GET(paste0('https://api.thingspeak.com/update?api_key=',key_i,'&field6=1'))
        Sys.sleep(1)
      }
      
    }
  })
  
  #create a data object to display data
  
  output$data <-DT::renderDataTable(datatable(
    Tracker_fleet, rownames = FALSE, escape = FALSE, options = list(searching = FALSE, paging = FALSE, dom = 't'), selection = 'none'
   
  ))
}

