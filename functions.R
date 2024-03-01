echarts4r::e_common(font_family = "Poppins")

# General Information -------------------------------------------------------------
page_information <- function(tbl, page_name, page_section=NULL, page_info) {
  
  if(is.null(page_section)) {
    
    t <- tbl |>
      filter(page == page_name) |>
      select(all_of(page_info)) |>
      pull()
    
  } else {
    
    t <- tbl |>
      filter(page == page_name & section == page_section) |>
      select(all_of(page_info)) |>
      pull()
    
  }
  
  
  if(is.na(t)) {f <- ""} else {f <- t}
  
  return(f)
  
}

# Maps -------------------------------------------------------------
create_urban_area_map<- function(lyr=urban_area_data, variable="Boardings", yr, dec=-2) {
  
  # Trim Layer to Variable of Interest and Year
  lyr <- lyr |> 
    filter(metric %in% variable) |>
    select("geography", all_of(yr)) |>
    rename(estimate = all_of(yr)) |>
    filter(estimate > 0)
  
  # Determine Bins
  bins <- c(0, 100000, 1000000, 2000000, 5000000, 50000000)
  pal <- colorBin(c("#4C4C4C", "#00A7A0", "#8CC63E", "#F05A28", "#91268F"), domain = lyr$estimate, bins = bins)
  
  labels <- paste0("<b>", paste0("Urban Area: "),"</b>", lyr$geography,"<br/>", "<b>",paste0(variable,": "), "</b>", prettyNum(round(lyr$estimate, dec), big.mark = ",")) |> lapply(htmltools::HTML)
  
  working_map <- leaflet(data = lyr) |>
    
    addProviderTiles(providers$CartoDB.Positron) |>
    
    addLayersControl(baseGroups = c("Base Map"),
                     overlayGroups = c(variable),
                     options = layersControlOptions(collapsed = TRUE)) |>
    
    addCircleMarkers(data = lyr, 
                     fillColor = pal(lyr$estimate),
                     fillOpacity = 1,
                     opacity = 0,
                     label = labels,
                     group = variable) |>
    
    addLegend(colors=c("#4C4C4C", "#00A7A0", "#8CC63E", "#F05A28", "#91268F"),
              labels=c("less than 100,000", "100,000 to 1 million", "1 to 2 million", "2 to 5 million", "more than 5 million"),
              group = variable,
              position = "bottomleft",
              title = paste0("Annual ", variable))
  
  
  
  return(working_map)
  
}

# Charts ------------------------------------------------------------------
tooltip_js <- "
      function(params, ticket, callback) {
      var fmt = new Intl.NumberFormat('en',
      {\"style\":\"percent\",\"minimumFractionDigits\":0,\"maximumFractionDigits\":0,\"currency\":\"USD\"});\n
      var idx = 0;\n
      if (params.name == params.value[0]) {\n
      idx = 1;\n        }\n
      return(params.seriesName + '<br>' + 
      params.marker + ' ' +\n
      params.name + ': ' + fmt.format(parseFloat(params.value[idx]))
      )
      }"

format_opts <- function(e, esttype, dec, title) {
  if(esttype == "number") {
    e <- e |> e_tooltip(trigger = "item")
    
  } else {
    
    if(esttype == "currency") {
      curr <- "USD"
    } else {
      curr <- NULL
    }
    
    e <- e |>
      e_y_axis(name = title, 
               nameLocation = "middle", 
               nameGap = 50,
               nameTextStyle = list(fontSize=14),
               axisLabel=list(margin=10),
               formatter = e_axis_formatter(esttype, digits = dec)) |>
      e_tooltip(trigger = "item",
                formatter =  e_tooltip_item_formatter(style = esttype, digits = 0, currency = curr)) |>
      e_tooltip(formatter =  htmlwidgets::JS(tooltip_js))
  }
  return(e)
}

e_basics <- function(e, top_padding, bottom_padding, legend, left_align) {
  e <- e |>
    e_grid(left = left_align, top = top_padding, bottom = bottom_padding) |>
    e_x_axis(axisTick=list(show = FALSE)) |>
    e_show_loading()
  
  e <- e |> e_legend(show = legend, bottom=0)

  return(e)
}

create_bar_chart <- function(df, x, y, fill, esttype="number", dec=0, color, bar_column="column", legend=TRUE, left_align='20%', top_padding=25, bottom_padding=75, title=NULL) {
  
  # Determine the number of Series to Plot
  chart_fill <- df |> select(all_of(fill)) |> distinct() |> pull() |> unique() |> as.character()
  
  # If the value is a percentage, round differently
  ifelse(esttype == "percent", num_dec <- 4, num_dec <- dec)
  
  # Format the tibble so that each series is in its own column - necessary to assigning colors in echarts via a palette
  chart_df <- df |>
    filter(.data[[fill]] %in% chart_fill) |>
    mutate(!!y:= round(.data[[y]], num_dec)) |>
    select(all_of(fill), all_of(x), all_of(y)) |>
    pivot_wider(names_from = all_of(fill), values_from = all_of(y))
  
  # Create the most basic chart
  c <- chart_df |>
    e_charts_(x, timeline = FALSE) |>
    e_toolbox_feature("dataView") |>
    e_toolbox_feature("saveAsImage")
  
  # Add a bar for each series
  for (fill_items in chart_fill) {
    c <- c |> e_bar_(fill_items, name = fill_items)
  }
  
  # Set series colors and set the basics for padding and leged
  c <- c |> e_color(color) |> e_basics(top_padding, bottom_padding, legend = legend, left_align = left_align)
  
  # Format the Axis and Hover Text
  c <- format_opts(c, esttype, dec, title)
  
  # Rotate for bar chart
  if (bar_column == "bar") {
    c <- c |> e_flip_coords() |> e_legend(show = legend, top=0)
  }
  
  return(c)
  
}
