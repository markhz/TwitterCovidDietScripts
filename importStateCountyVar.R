
rm(list=ls())

# set working directory to script
wd <- "/projectnb2/sph795/markhz/TwitterScripts"
setwd(wd)


# import packages and functions
library(tidyverse)
library(tictoc)
library(tidycensus)
source("twitter_food_functions.R")

# Add census api key
# census_api_key("b25f910867b5786e08dec5cba9253be2e6d376f6", install = TRUE)


startDate <- as.Date("2020-05-15")
endDate <- as.Date("2021-01-31")

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# import data
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------



# import google mobility data (all)
# -------------------------------------------------------------------------
df_google <- read.csv("Data/ImportedData/2020_US_Region_Mobility_Report.csv") %>%
  bind_rows( read.csv("Data/ImportedData/2021_US_Region_Mobility_Report.csv")  ) %>%
  select( c("iso_3166_2_code", "census_fips_code", "date", 
            "retail_and_recreation_percent_change_from_baseline",
            "residential_percent_change_from_baseline",
            "grocery_and_pharmacy_percent_change_from_baseline") ) %>%
  rename("pctchange_retail_rec" = "retail_and_recreation_percent_change_from_baseline",
         "pctchange_grocery" = "grocery_and_pharmacy_percent_change_from_baseline",
         "pctchange_residential" = "residential_percent_change_from_baseline") %>%
  rename("state_code" = "iso_3166_2_code")  %>%
  rename("FIPS" = "census_fips_code") %>%
  mutate(StateAbbr = substr(state_code, nchar(state_code)-2+1, nchar(state_code)),
         date = as.Date(date) ) %>%
  filter(date >= startDate & date <= endDate) 




# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# generate county data frame
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------



# get google mobility county data
# -------------------------------------------------------------------------

# summarize mobility data by county
df_google_county <- df_google %>%
  filter(!is.na(FIPS)) %>%
  group_by(FIPS) %>%
  summarize( pctchange_retail_rec = mean(pctchange_retail_rec, na.rm = TRUE),
             pctchange_grocery = mean(pctchange_grocery, na.rm = TRUE),
             pctchange_residential = mean(pctchange_residential, na.rm = TRUE)) %>%
  filter(!is.na(pctchange_retail_rec) &
           !is.na(pctchange_grocery) &
           !is.na(pctchange_residential) ) %>%
  mutate(FIPS = sprintf("%05d", FIPS) )





# import ACS sociodemographic variables (county)
# -------------------------------------------------------------------------
# load county shapefile with population estimates
df_pop_county <- get_estimates(geography = "county",
                               variables = "POP",
                               year = 2019) %>%
  filter(variable == "POP") %>%
  rename(ESTPOP2019 = value,
         FIPS = GEOID) %>%
  select("FIPS", "ESTPOP2019") 


df_acs_county <- getAcsClean(year_in = 2019, 
                             geography_in = "county") %>%
  calc_ICE_metrics() %>%
  mutate(FIPS_ST = as.numeric(substr(FIPS,  start = 1, stop = 2)) ) %>% 
  select(c("FIPS", "FIPS_ST", "YEAR", 
           "pctOld", "pctYoung",
           "pctLowIncome", "pctHighIncome", 
           "pctBlack", "pctWhite", "pctHisp"))


# import Community Business Patterns (CBP)  variables (county)
# -------------------------------------------------------------------------
df_cbp_county <- read.csv("Data/ImportedData/cbp_2018.csv") %>%
  mutate(NAICS2017_LABEL = factor(NAICS2017_LABEL,
                                  levels = c("Grocery stores", 
                                             "Supermarkets and other grocery (except convenience) stores",
                                             "Beer, wine, and liquor stores"  ,
                                             "Food services and drinking places"  ,
                                             "Drinking places (alcoholic beverages)" ) ,
                                  labels = c("grocAll", "groc", "liq", "rest", "bars") ) )%>%
  select(-c("NAICS2017")) %>%
  pivot_wider(names_from = NAICS2017_LABEL, values_from = n) %>%
  mutate(FIPS = sprintf("%05d", FIPS) ) %>%
  replace(is.na(.), 0) %>%
  left_join(df_pop_county, by = "FIPS") %>%
  mutate(groc_per10k = groc / ESTPOP2019 * 10000,
         grocAll_per10k = grocAll / ESTPOP2019 * 10000,
         liq_per10k = liq / ESTPOP2019 * 10000,
         bars_per10k = bars / ESTPOP2019 * 10000,
         rest_per10k = rest / ESTPOP2019 * 10000) %>%
  select(-c("ESTPOP2019") )


# join county covariates (ACS, CDC, Google, CBP, CUSP)
# -------------------------------------------------------------------------

df_county_join <- df_pop_county %>%
  left_join( df_acs_county , by = "FIPS") %>%
  left_join( df_google_county , by = "FIPS") %>%
  left_join( df_cbp_county , by = "FIPS")


write.csv(df_county_join, "Data/ProcessedData/County_covariates.csv", 
          row.names = FALSE)


