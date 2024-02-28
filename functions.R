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
                     label = labels) |>
    
    addLegend(colors=c("#4C4C4C", "#00A7A0", "#8CC63E", "#F05A28", "#91268F"),
              labels=c("less than 100,000", "100,000 to 1 million", "1 to 2 million", "2 to 5 million", "more than 5 million"),
              group = variable,
              position = "bottomleft",
              title = paste0("Annual ", variable))
  
  
  
  return(working_map)
  
}