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
    output$ferry_ua_map <- renderLeaflet({create_urban_area_map(yr=latest_yr)})
    output$ferry_ua_chart <- renderEcharts4r({echart_bar_chart(df = urban_area_data |> 
                                                                 st_drop_geometry() |> 
                                                                 select("geography", "metric", all_of(latest_yr)) |>
                                                                 rename(estimate = all_of(latest_yr)) |>
                                                                 filter(estimate > 0) |>
                                                                 arrange(estimate),
                                                               x="geography", y="estimate", 
                                                               tog="metric", esttype = "number", dec=0, color="jewel", title="")})
    
    # Tab layout
    output$ferryua <- renderUI({
      tagList(
        h1("Ferry Usage in Urban Areas"),
        textOutput(ns("ferry_ua_text")) |> withSpinner(color=load_clr),
        br(),
        strong(tags$div(class="chart_title","Annual Ferry Boardings by Urban Area")),
        fluidRow(column(12,leafletOutput(ns("ferry_ua_map")))),
        tags$div(class="chart_source","Source: USDOT Federal Transit Administration (FTA) National Transit Database (NTD)"),
        hr(style = "border-top: 1px solid #000000;"),
        
        strong(tags$div(class="chart_title","Annual Ferry Boardings, Hours and Miles by Urban Area")),
        #fluidRow(column(12,echarts4rOutput(ns("ferry_ua_chart"), height = "600px"))),
        tags$div(class="chart_source","Source: USDOT Federal Transit Administration (FTA) National Transit Database (NTD)"),
        hr(style = "border-top: 1px solid #000000;"),
        
      )
    }) 
  })  # end moduleServer
}

# Demographics ------------------------------------------------------------
ferry_demographics_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("ferrydemographics"))
  )
}

ferry_demographics_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Text
    output$ferry_demographics_poc_text <- renderText({page_information(tbl=page_text, page_name="Ferry", page_section = "Demographics-Race", page_info = "description")})
    
    # Charts & Maps
    output$ferry_demographics_poc_chart <- renderEcharts4r({create_bar_chart(df = demographic_data |> filter(metric=="People of Color" & grouping =="Person of Color") |> mutate(year=as.character(year)),
                                                                             x="year", y="share", fill="variable", 
                                                                             esttype="percent", color=c("#00A7A0", "#F05A28"),
                                                                             left_align = '10%', title = "% People of Color")})
    
    output$ferry_demographics_dis_chart <- renderEcharts4r({create_bar_chart(df = demographic_data |> filter(metric=="People with a Disability" & grouping =="With a disability") |> mutate(year=as.character(year)),
                                                                             x="year", y="share", fill="variable", 
                                                                             esttype="percent", color=c("#00A7A0", "#F05A28"),
                                                                             left_align = '10%', title = "% People with a Disability")})
    
    output$ferry_demographics_edu_chart <- renderEcharts4r({create_bar_chart(df = demographic_data |> filter(metric=="People with a College Degree" & grouping =="College Degree") |> mutate(year=as.character(year)),
                                                                             x="year", y="share", fill="variable", 
                                                                             esttype="percent", color=c("#00A7A0", "#F05A28"),
                                                                             left_align = '10%', title = "% People with a College Degree")})
    
    output$ferry_demographics_own_chart <- renderEcharts4r({create_bar_chart(df = demographic_data |> filter(metric=="People who Own their Home" & grouping =="Owned") |> mutate(year=as.character(year)),
                                                                             x="year", y="share", fill="variable", 
                                                                             esttype="percent", color=c("#00A7A0", "#F05A28"),
                                                                             left_align = '10%', title = "% People who own heir home")})
    
    # Tab layout
    output$ferrydemographics <- renderUI({
      tagList(
        h1("Demographics of Ferry Users"),
        textOutput(ns("ferry_demographics_poc_text")) |> withSpinner(color=load_clr),
        br(),
        
        # Title
        fluidRow(column(6, strong(tags$div(class="chart_title","People of Color"))),
                 column(6, strong(tags$div(class="chart_title","People with a Disability")))),
        # Charts
        fluidRow(column(6,echarts4rOutput(ns("ferry_demographics_poc_chart"))),
                 column(6,echarts4rOutput(ns("ferry_demographics_dis_chart")))),
        
        br(),
        
        # Title
        fluidRow(column(6, strong(tags$div(class="chart_title","College Degree"))),
                 column(6, strong(tags$div(class="chart_title","Home Owners")))),
        # Charts
        fluidRow(column(6,echarts4rOutput(ns("ferry_demographics_edu_chart"))),
                 column(6,echarts4rOutput(ns("ferry_demographics_own_chart")))),
        
        br(),
        tags$div(class="chart_source","Source: US Census Bureau Public Use Microdata Sample (PUMS)"),
        hr(style = "border-top: 1px solid #000000;")
        
      )
    }) 
  })  # end moduleServer
}
