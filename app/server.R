library(shiny)




shinyServer(function(input, output, session) {

  

  


  observe({
    # We'll use the input$controller variable multiple times, so save it as x
    # for convenience.
  })
  output$Information <- renderMenu(dropdownMenu(type = "messages",badgeStatus = NULL,icon = icon("question"), headerText = "Information about this page",
                                                messageItem(
                                                  from = NULL,
                                                  message = "This is a message",
                                                  time = NULL,
                                                  icon = icon("info")
                                                )
  ))

  output$logout <- renderMenu(dropdownMenu(type = "messages",badgeStatus = NULL,icon = icon("power-off"),headerText =  "CentraleSupelec",
                                           messageItem(
                                             from = NULL,
                                             message = "Sign Out",
                                             time = NULL,
                                             icon = icon("sign-out-alt")
                                             , href = paste0(session$clientData$url_protocol,"//",session$clientData$url_hostname,':',session$clientData$url_port,"/logout")
                                           )
  ))


  openTab <- reactiveVal()
   observe({ openTab(input$tabs) }) 

  callModule(map_server, "rideMapMod")
  


  observeEvent(openTab(), {
    updateTabItems(session, "tabs", openTab())
  })



})
