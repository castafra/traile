## ui.R

source("map_module.R")





sidebar <- dashboardSidebar(

    sidebarMenu(
      id = "tabs",

      menuItem("Live Race", tabName = "ride", icon = icon("stopwatch")
              ,menuSubItem("Map", tabName = "ridemap", icon = icon("map-marked"))
              ,menuSubItem("Profile", tabName = "rideprofile", icon = icon("mountain"))
      ),

      
      
      
      HTML("<li id='participants_dash'>"),
      a(href = "#client", icon("walking"), span("Participants"), selectizeInput("global_select_client", NULL, choices = paste(Tracker_fleet$name,Tracker_fleet$surname), multiple = TRUE, options = list( placeholder = "Select",maxItems = 1))),
      HTML("</li>")
    ),
    tags$footer(title="credits",
                div(
                    img(src="logo_white.png",width = 160, height = 113),br(),br(),align = "left"

                ),
                div(
                  HTML(paste("&#9400; 2020 - Version ","1.0")), align = "center"

                ),
                align = "right",
                style = "
                    font-family : Helvetica;
                    font-size: 0.7em ;
                    position:absolute;
                    margin-left: 10px;
                    margin-bottom: 75px;
                    bottom:25px;
                    width:100%;
                    height:50px; /* Height of the footer */
                    color: white;"
    )
)

loadingLogo <- function(href, src, loadingsrc, height = NULL, width = NULL, heightgif = NULL, widthgif = NULL, alt = NULL) {
  tagList(
    tags$head(
      tags$script(
        "setInterval(function(){
                     if ($('html').attr('class')=='shiny-busy') {
                     $('div.busy').show();
                     $('div.notbusy').hide();
                     } else {
                     $('div.busy').hide();
                     $('div.notbusy').show();
           }
         },100)")
    ),
    tags$a(
           div(class = "busy",
               img(src=loadingsrc,height = heightgif, width = widthgif, alt = alt)

               ),
           div(class = 'notbusy',
               img(src = src, height = height, width = width, alt = alt)

               )
    )
  )
}



body <- dashboardBody(
  tags$style(HTML('button#rideMapMod-track .btn .btn-default .action-button .shiny-bound-input  {
            background-color: #0E6655 !important;
}')),
  tags$style(HTML('a.dropdown-toggle {
                                color: #0E6655 !important;
                                }')),
  tags$style(HTML('body {
                                height: 100% !important;
                                }')),
  tags$style(HTML('span.logo {
                                background-color: #FFFFFF !important;
                                }')),
  tags$style(HTML('a.sidebar-toggle {
                                color: #0E6655 !important;
                                }')),
  tags$style(HTML('span.irs-from {
                                background-color: #0E6655 !important;
                                }')),
  tags$style(HTML('span.irs-bar {
                                background-color: #0E6655 !important;
                                }')),
  tags$style(HTML('span.irs-single {
                                background-color: #0E6655 !important;
                                }')),
  tags$style(HTML('span.irs-bar-edge {
                                background-color: #0E6655 !important;
                                }')),
  tags$style(HTML('span.irs-to {
                                background-color: #0E6655 !important;
                                }')),
  tags$style(HTML('div#titlesdash.shiny-text-output.shiny-bound-output {color: #0E6655 !important;}')),

      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      #tags$script(HTML("$('body').addClass('fixed');")),

      tags$style(HTML("#big-heading{
                      color: #0E6655;
                      }")),
      tags$style(HTML("#big-title{
                      font-weight: bold;
                      font-size : 1.5em;
                      margin-top : -1em;
                      marging-left : 5em;
                      }")),
      tags$style(HTML("#presentation{
                      color: #0E6655;
                      padding-bottom : 1em;
                      padding-left : 1em;
                      }")),



      tags$head(tags$style(HTML('

.content {
  padding-top: 60px;
}

.main-header {
  position: fixed;
  width:100%;
}

.arrow-icon {
        width: 14px;
        height: 14px;
    }

.arrow-icon > div {
            margin-left: -1px;
            margin-top: -3px;
            transform-origin: center center;
            font: 12px/1.5 "Helvetica Neue", Arial, Helvetica, sans-serif;
        }

.skin-black .main-sidebar {
                                background-color: #0E6655;
                                }

.skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #094B3E;
                              color: #FFC300;
         }

.skin-black .main-sidebar .sidebar .sidebar-menu .active a{
                              color: #FFC300;
                              border-color: #ffffff;
        }

.skin-black .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #0E6655;

                              color: :#0E6655;
        }
.skin-black .main-sidebar .sidebar {
                              background-color: #0E6655;
}

                            '))),




    tabItems(
      

      tabItem(tabName = "ridemap", map_ui("rideMapMod")
              
      )
)
)

  # Put them together into a dashboardPage
  dashboardPage(skin = "black",

                dashboardHeader(title =  loadingLogo(src ="gif3.png",
                                                     loadingsrc = "gif_logo.gif", height = 40, width = 30, heightgif = 40, widthgif = 30)

                                ,tags$li(
                                  class = "dropdown",
                                  tags$h4(class = "titre",textOutput("titlesdash")),
                                  style = "width: 50vw !important;
                                          font-size: 25px;
                                            font-weight:bold !important;
                                            color : #0E6655;
                                            font-family : Helvetica Neue !important;"
                                )
                                ,dropdownMenuOutput("Information")
                                ,dropdownMenuOutput("logout")

                )

    ,
    sidebar,
    body
  )



