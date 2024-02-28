# Overview ----------------------------------------------------------------------------
ferry_overview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("ferryoverview"))
  )
}

ferry_overview_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Text
    output$ferry_overview_text <- renderText({page_information(tbl=page_text, page_name="Ferry", page_section = "Overview", page_info = "description")})
    
    # Overview UI
    output$ferryoverview <- renderUI({
      tagList(
        textOutput(ns("ferry_overview_text")),
        br()
      )
    })
  })  # end moduleServer
}

# Urban Area Context --------------------------------------------------------------------------------
ferry_ua_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("ferryua"))
  )
}

ferry_ua_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Text
    output$ferry_ua_text <- renderText({page_information(tbl=page_text, page_name="Ferry", page_section = "Urban-Area", page_info = "description")})
    
    # Charts & Maps
    output$ferry_ua_map <- renderLeaflet({create_urban_area_map(yr="2023")})
    
    # Tab layout
    output$ferryua <- renderUI({
      tagList(
        h1("Ferry Usage in Urban Areas"),
        textOutput(ns("ferry_ua_text")) |> withSpinner(color=load_clr),
        br(),
        fluidRow(column(12,leafletOutput(ns("ferry_ua_map")))),
        tags$div(class="chart_source","Source: USDOT Federal Transit Administration (FTA) National Transit Database (NTD)"),
        hr(style = "border-top: 1px solid #000000;")
      )
    }) 
  })  # end moduleServer
}

