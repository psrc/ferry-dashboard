library(tidyverse)
library(psrcplot)
library(psrccensus)
library(openxlsx)
library(sf)

# Process NTD Data --------------------------------------------------------

# Silence the dplyr summarize message
options(dplyr.summarise.inform = FALSE)

# Location of the most recently downloaded NTD file
file_dir <- "X:/DSA/rtp-dashboard/NTD/"
code_dir <- "C:/coding/ferries/"
setwd(file_dir)

# Choose NTD file
data_file <- file.choose()

ntd_tabs <- c("UPT", "VRM", "VRH")

processed <- NULL
for (areas in ntd_tabs) {
  print(str_glue("Working on {areas} data processing and cleanup."))
  
  # Open file and filter data to only include Ferry modes
  t <- as_tibble(read.xlsx(data_file, sheet = areas, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE)) |>
    mutate(NTD.ID = str_pad(string=NTD.ID, width=5, pad="0", side=c("left"))) |>
    filter(Mode == "FB") |> 
    select(-"Legacy.NTD.ID", -"Status", -"Reporter.Type", -"TOS", -"3.Mode") |> 
    pivot_longer(cols = 6:last_col(), names_to = "date", values_to = "estimate", values_drop_na = TRUE) |> 
    mutate(date = my(date))
  
  # Add Detailed Mode Names & Aggregate  
  t <- t |> 
    mutate(variable="Ferry") |>
    rename(uza="UZA.Name", fips="UACE.CD", geography="Agency", ntd_id="NTD.ID") |> 
    select(-"Mode") |>
    group_by(ntd_id, geography, uza, fips, date, variable) |>
    summarise(estimate=sum(estimate)) |>
    as_tibble()
  
  # Only include full year data
  max_yr <- t |> select("date") |> distinct() |> pull() |> max() |> year()
  max_mo <- t |> select("date") |> distinct() |> pull() |> max() |> month()
  
  if (max_mo <12) {
    yr <- max_yr-1
  } else {
    yr <- max_yr
  }
  
  # Trim Data so it only includes full year data and combine
  t <- t |>
    filter(year(date)<=yr) |>
    mutate(year = year(date)) |>
    group_by(year, ntd_id, geography, uza, fips, variable) |>
    summarise(estimate=sum(estimate)) |>
    as_tibble() |>
    mutate(metric=areas) |>
    mutate(metric = case_when(
      metric == "UPT" ~ "Boardings",
      metric == "VRM" ~ "Revenue-Miles",
      metric == "VRH" ~ "Revenue-Hours")) |>
    mutate(uza = str_replace_all(uza, "Bremerton, WA", "Seattle--Tacoma, WA")) |>
    mutate(fips = str_replace_all(fips, "09946", "80389"))
  
  # Ferry Results by Operator
  o <- t |> mutate(geography_type = "Ferry Operator")
  
  # Ferry Results by Urban Area
  u <- t |>
    group_by(year, uza, fips, variable, metric) |>
    summarise(estimate=sum(estimate)) |>
    as_tibble() |>
    mutate(geography_type = "Urban Area", geography = uza, ntd_id=fips)
  
  # Combine NTD results
  ifelse(is.null(processed), processed <- bind_rows(list(o, u)), processed <- bind_rows(list(processed, o, u)))
  rm(t, o, u)

}

# Urban Area Data ---------------------------------------------------------
setwd(code_dir)
ua_lyr <- st_read("data/tl_2023_us_uac20.shp")

# Ferry Boardings by Urban Area
b <- processed |> 
  filter(geography_type == "Urban Area" & metric == "Boardings") |>
  select("year", "fips", "geography", "estimate", "metric") |>
  pivot_wider(names_from = year, values_from = estimate) 
  
h <- processed |> 
  filter(geography_type == "Urban Area" & metric == "Revenue-Hours") |>
  select("year", "fips", "geography", "estimate", "metric") |>
  pivot_wider(names_from = year, values_from = estimate) 

m <- processed |> 
  filter(geography_type == "Urban Area" & metric == "Revenue-Miles") |>
  select("year", "fips", "geography", "estimate", "metric") |>
  pivot_wider(names_from = year, values_from = estimate) 

ua_data <- bind_rows(b,h,m)

ua_ferry_data <- left_join(ua_data, ua_lyr, by=c("fips"="UACE20")) |>
  select(-"fips", -"GEOID20", -"GEOIDFQ20", -"NAME20", -"NAMELSAD20", -"LSAD20", -"MTFCC20", -"FUNCSTAT20", -"ALAND20", -"AWATER20") |>
  rename(lat="INTPTLAT20", lon="INTPTLON20") |>
  select(-geometry) |>
  st_as_sf(coords = c(x="lon", y="lat"))

saveRDS(ua_ferry_data,"data/urban_area_ferry_data.rds")

rm(b,h,m, ua_data, ua_lyr)

# Ferry Operator Data -----------------------------------------------------
operator <- processed |> filter(geography_type == "Ferry Operator")
saveRDS(operator,"data/operator_ferry_data.rds")
