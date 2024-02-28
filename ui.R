shinyUI(

  fluidPage(
    
    tags$style(HTML("
    .tabbable > .nav > li > a {background-color: #005753;  color:white}
    .tabbable > .nav > li.active > a {background-color: #91268F; color:white}
  ")),
    
    id = "Ferries",
    tags$style("@import url(https://use.fontawesome.com/releases/v6.3.0/css/all.css);"),
    title = "Ferry Use in the PSRC Region",
    
    theme = "styles.css",
    banner_ui('ferryBanner'),
    
    sidebarLayout(
      sidebarPanel(left_panel_ui('leftMain')),
      
      mainPanel(
        tabPanel("Urban Areas",
                 fluidRow(column(12, style='padding-left:25px; padding-right:50px;',
                                 ferry_overview_ui('ferryOverview'),
                                 tabsetPanel(type = "pills",
                                             tabPanel("Urban Areas", ferry_ua_ui('UAferry')))
                                 )) # end of fluid row for Urban Areas tab
                 ), # end of tabpanel for Urban Areas
      ) # End of mainPanel
    ),
    tags$footer(footer_ui('psrcfooter'))
  ) # End of fluid page
) # end of shiny app
