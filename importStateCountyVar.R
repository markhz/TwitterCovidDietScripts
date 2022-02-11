
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
# generate state data frame
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------




# get state policy data (state)
# -------------------------------------------------------------------------

# import policy data (state)
# -------------------------------------------------------------------------
df_cusp <- read.csv("CUSP_data_20210310.csv") %>%
  filter(!is.na(FIPS)) %>%
  rename("StateAbbr" = "POSTCODE")


cusp_vars <- c("CLREST", "ENDREST", "CLRST2", "ENDREST2", "CLRST3","END_CLRST3",
               "CLOSEBAR", "END_BRS", "CLBAR2", "END_BRS2")

startDate_all <- as.Date("2020-03-14")
endDate_all <- as.Date("2021-01-31")
days_all <- as.numeric(endDate_all - startDate_all)

# create data frame with state policy data
df_cusp_state <- df_cusp %>%
  select(c("FIPS", "StateAbbr", cusp_vars)) %>%
  mutate_at(cusp_vars, as.Date, format = "%m/%d/%Y") %>%
  rowwise() %>%
  mutate(days_closed_bar1 = calcDaysClosed(CLOSEBAR, END_BRS, startDate = startDate_all,  endDate = endDate_all ),
         days_closed_bar2 = calcDaysClosed(CLBAR2, END_BRS2, startDate = startDate_all,  endDate = endDate_all ),
         days_closed_rst1 = calcDaysClosed(CLREST, ENDREST, startDate = startDate_all,  endDate = endDate_all ),
         days_closed_rst2 = calcDaysClosed(CLRST2, ENDREST2, startDate = startDate_all,  endDate = endDate_all ),
         days_closed_rst3 = calcDaysClosed(CLRST3, END_CLRST3, startDate = startDate_all,  endDate = endDate_all ),
         days_closed_bar = days_closed_bar1 + days_closed_bar2,
         days_closed_rst = days_closed_rst1 + days_closed_rst2 + days_closed_rst3,
         pct_closed_bar = days_closed_bar / days_all * 100,
         pct_closed_rst = days_closed_rst / days_all * 100,
         weeks_closed_bar = days_closed_bar / 7,
         weeks_closed_rst = days_closed_rst / 7
  )  %>%
  
  ungroup() %>%
  select( c("FIPS", "StateAbbr", "weeks_closed_bar","pct_closed_bar", "weeks_closed_rst",  "pct_closed_rst") )



# get google mobility data (state)
# -------------------------------------------------------------------------

# summarize mobility data by state
df_google_state <- df_google %>%
  filter(state_code != "") %>%
  group_by(StateAbbr) %>%
  summarize( pctchange_retail_rec = mean(pctchange_retail_rec, na.rm = TRUE),
             pctchange_grocery = mean(pctchange_grocery, na.rm = TRUE),
             pctchange_residential = mean(pctchange_residential, na.rm = TRUE)) %>%
  filter(!is.na(pctchange_retail_rec) &
           !is.na(pctchange_grocery) & 
           !is.na(pctchange_residential) )

# get google mobility data (state)
# -------------------------------------------------------------------------



df_acs_state<- getAcsClean(year_in = 2019, 
                             geography_in = "state") %>%
  calc_ICE_metrics() %>%
  mutate(FIPS = as.numeric(substr(FIPS,  start = 1, stop = 2)) ) %>% 
  select(c("FIPS",  "YEAR", 
           "pctOld", "pctYoung",
           "pctLowIncome", "pctHighIncome", 
           "pctBlack", "pctWhite", "pctHisp"))

# join state covariates
# -------------------------------------------------------------------------


df_state_join <- df_cusp_state %>%
  left_join(df_google_state, by = "StateAbbr") %>%
  left_join(df_acs_state, by = "FIPS")
  


write.csv(df_state_join, "Data/ProcessedData/State_covariates.csv", 
          row.names = FALSE)


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




# import CDC PLACES data (county)
# -------------------------------------------------------------------------
df_cdc_county <- read.csv("Data/ImportedData/PLACES__Local_Data_for_Better_Health__County_Data_2020_release.csv") %>%
  filter(DataValueTypeID == "CrdPrv",
         LocationName != "",
         MeasureId == "OBESITY" | MeasureId == "BINGE") %>%
  rename("FIPS" = "LocationID") %>%
  select(c("FIPS", "MeasureId", "Data_Value")) %>%
  pivot_wider(names_from = "MeasureId", values_from = "Data_Value") %>%
  mutate(FIPS = sprintf("%05d", FIPS) ) %>%
  rename("pct_obese" = "OBESITY",
         "pct_bingedrink" = "BINGE") 
#"Low_Confidence_Limit", "High_Confidence_Limit"





# import ACS sociodemographic variables (county)
# -------------------------------------------------------------------------
# load state shapefile with population estimates
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
  left_join( df_cdc_county , by = "FIPS") %>%
  left_join( df_google_county , by = "FIPS") %>%
  left_join( df_cbp_county , by = "FIPS") %>%
  left_join( df_cusp_state, by = c("FIPS_ST" = "FIPS"))


write.csv(df_county_join, "Data/ProcessedData/County_covariates.csv", 
          row.names = FALSE)


