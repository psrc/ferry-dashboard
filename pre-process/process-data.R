library(tidyverse)
library(psrcplot)
library(psrccensus)
library(openxlsx)
library(sf)

# Inputs ------------------------------------------------------------------
# Non-overlapping PUMS 5 year data years
census_yr <- c(2012, 2017, 2022) 

# Some variables differ in prior PUMS years
pums_vars_pre_2019 <- c("JWTR", "PRACE", "DIS", "SCHL", "WAGP", "ENG", "JWMNP", "OWN_RENT", "HINCP")
pums_vars_post_2019 <- c("JWTRNS", "PRACE", "DIS", "SCHL", "WAGP", "ENG", "JWMNP", "OWN_RENT", "HINCP")

# Silence the dplyr summarize message
options(dplyr.summarise.inform = FALSE)

# Location of the most recently downloaded NTD file
file_dir <- "X:/DSA/rtp-dashboard/NTD/"
code_dir <- "C:/coding/ferry-dashboard/"

# Process NTD Data --------------------------------------------------------
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

# Clean up Urban Area Names
processed <- processed |>
  mutate(geography = str_replace_all(geography, "Seattle--Tacoma, WA", "Seattle, WA")) |>
  mutate(geography = str_replace_all(geography, "New York--Jersey City--Newark, NY--NJ", "New York City, NY--NJ")) |>
  mutate(geography = str_replace_all(geography, "Atlantic City--Ocean City--Villas, NJ", "Atlantic City, NJ")) |>
  mutate(geography = str_replace_all(geography, "San Francisco--Oakland, CA", "San Francisco, CA")) |>
  mutate(geography = str_replace_all(geography, "Miami--Fort Lauderdale, FL", "Miami, FL")) |>
  mutate(geography = str_replace_all(geography, "Virginia Beach--Norfolk, VA", "Norfolk, VA"))

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

# Census Data for Ferry Commuters: Person Level -----------------------------------------
pums_ferry_data <- NULL
for (y in census_yr) {
  print(str_glue("Working on {y} PUMS 5-year data download"))
  
  # Download the relevant person variables by census year
  pums_person <- get_psrc_pums(span = 5, dyear = y, level = "p", vars = if(y < 2019) pums_vars_pre_2019 else pums_vars_post_2019)
  
  # Clean up some of the attributes for simplification of analysis
  p <- pums_person |> 
    rename(mode = starts_with("JWTR"), race = ends_with("RACE")) |>
    mutate(ferry = factor(case_when(is.na(mode) ~ NA_character_,
                                    mode == "Car, truck, or van" ~ "Other",
                                    mode == "Bus" ~ "Other",
                                    mode == "Bus or trolley bus" ~ "Other",
                                    mode == "Ferryboat" ~ "Ferry",
                                    mode == "Streetcar or trolley car (carro publico in Puerto Rico)" ~ "Other",
                                    mode == "Light rail, streetcar, or trolley" ~ "Other",
                                    mode == "Railroad" ~ "Other",
                                    mode == "Long-distance train or commuter rail" ~ "Other",
                                    mode == "Other method" ~ "Other",
                                    mode == "Taxicab" ~ "Other",
                                    mode == "Motorcycle" ~ "Other",
                                    mode == "Subway or elevated" ~ "Other",
                                    mode == "Subway or elevated rail" ~ "Other",
                                    mode == "Worked at home" ~ "Other",
                                    mode == "Worked from home" ~ "Other",
                                    mode == "Walked" ~ "Other",
                                    mode == "Bicycle" ~ "Other",
                                    TRUE ~ as.character(.data$mode)))) |>
    mutate(college = factor(case_when(
      SCHL %in% c("Bachelor's degree", "Master's degree", "Professional degree beyond a bachelor's degree", "Doctorate degree") ~ "College Degree",
      !(SCHL %in% c("Bachelor's degree", "Master's degree", "Professional degree beyond a bachelor's degree", "Doctorate degree")) ~ "No Degree"))) |>
    mutate(poc = factor(case_when(
      race %in% c("White alone") ~ "White",
      !(race %in% c("White alone")) ~ "Person of Color"))) |>
    mutate(lep = factor(case_when(
      ENG %in% c("Not well", "Not at all") ~ "Limited English Proficiency",
      !(ENG %in% c("Not well", "Not at all")) ~ "English proficient")))
  
  # Calculate various counts, shares, medians and means for ferry related variables
  print(str_glue("Summarizing {y} PUMS 5-year data for ferry users"))
  race <- psrc_pums_count(p, group_vars = c("ferry", "poc"), incl_na = FALSE) |> select(year="DATA_YEAR", variable="ferry", grouping="poc", estimate="count", share="share") |> mutate(metric="People of Color")
  disability <- psrc_pums_count(p, group_vars = c("ferry", "DIS"), incl_na = FALSE) |> select(year="DATA_YEAR", variable="ferry", grouping="DIS", estimate="count", share="share") |> mutate(metric="People with a Disability")
  education <- psrc_pums_count(p, group_vars = c("ferry", "college"), incl_na = FALSE) |> select(year="DATA_YEAR", variable="ferry", grouping="college", estimate="count", share="share") |> mutate(metric="People with a College Degree")
  limited_english <- psrc_pums_count(p, group_vars = c("ferry", "lep"), incl_na = FALSE) |> select(year="DATA_YEAR", variable="ferry", grouping="lep", estimate="count", share="share") |> mutate(metric="People with Limited English Proficiency")
  housing_tenure <- psrc_pums_count(p, group_vars = c("ferry", "OWN_RENT"), incl_na = FALSE) |> select(year="DATA_YEAR", variable="ferry", grouping="OWN_RENT", estimate="count", share="share") |> mutate(metric="People who Own their Home")
  median_wage <- psrc_pums_median(p, stat_var="WAGP", group_vars = c("ferry"), incl_na = FALSE) |> select(year="DATA_YEAR", variable="ferry", estimate="WAGP_median") |> mutate(metric="Wage of Worker", grouping="All")
  median_income <- psrc_pums_median(p, stat_var="HINCP", group_vars = c("ferry"), incl_na = FALSE) |> select(year="DATA_YEAR", variable="ferry", estimate="HINCP_median") |> mutate(metric="Household Income of Worker", grouping="All")
  mean_time <- psrc_pums_mean(p, stat_var="JWMNP", group_vars = c("ferry"), incl_na = FALSE) |> select(year="DATA_YEAR", variable="ferry", estimate="JWMNP_mean") |> mutate(metric="Travel Time to Work", grouping="All")
  
  # Join data into final tibble and remove interim data
  if (is.null(pums_ferry_data)) {
    
    pums_ferry_data <- bind_rows(race, disability, education, limited_english, housing_tenure, median_wage, median_income, mean_time)
    
  } else {
    
    pums_ferry_data <- bind_rows(pums_ferry_data, race, disability, education, limited_english, housing_tenure, median_wage, median_income, mean_time)
    
  }
  
  rm(race, disability, education, limited_english, housing_tenure, median_wage, median_income, mean_time, pums_person, p)

}

saveRDS(pums_ferry_data,"data/pums_ferry_data.rds")




