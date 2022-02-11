
rm(list=ls())

# set working directory to script
wd <- "/projectnb2/sph795/markhz/TwitterScripts"
setwd(wd)

# import packages and functions
library(tidyverse)
library(tictoc)
library(zoo) # date time variables
library(lubridate) # date time variables
# devtools::install_github("r-spatial/sf")
library(sf)
library(tidycensus)
library(broom) # for tidy
library(RColorBrewer)

library(caTools)
library(randomForest)

source("twitter_food_functions.R")

# Add census api key
# census_api_key("b25f910867b5786e08dec5cba9253be2e6d376f6", install = TRUE)



# set path to input data
dataPath <- "/project/sph795/Data/"

filePath2019 <- file.path(dataPath, 
                          "Twitter_food_2019",
                          "food_tweets_feb_2019_jan_2020.csv")

filePath2020 <- file.path(dataPath, 
                          "Twitter_food_2020",
                          "food_tweets_feb_2020_jan_2021.csv")

filePath_labeled <- file.path("/projectnb/sph795/markhz/TwitterScripts/Data/ProcessedData/prediction_full.csv")


# filePathTweetClassifier <- file.path("Classifiers/TweetClassifier.RData")  

filePathHexShp <- "us_states_hexgrid/us_states_hexgrid.shp"


# load classifier
# load(filePathTweetClassifier)

# load csv with 2019 Twitter food data
tic()
df19 <- read.csv(filePath2019, fileEncoding="latin1") %>%
  mutate(datasetYear = 2019)
toc()

# load csv with 2020 Twitter food data
tic()
df20 <- read.csv(filePath2020, fileEncoding="latin1") %>%
  mutate(datasetYear = 2020)
toc()

df_in <- df19 %>%
  bind_rows(df20)

rm(df19)
rm(df20)


# read classifier output
df_classifier_out <- read.csv(filePath_labeled)

df_in <- df_in %>%
  mutate(label = df_classifier_out$x)


# -------------------------------------------------------------------------
# load state and county shapefiles from census ----------------------------
# -------------------------------------------------------------------------



# load state shapefile with population estimates
shp_state_pop <- get_estimates(geography = "state",
                               variables = "POP",
                               year = 2019,
                               geometry = TRUE) %>%
  filter(variable == "POP") %>%
  rename(ESTPOP2019 = value,
         STATE = NAME)


# load state shapefile with population estimates
shp_county_pop <- get_estimates(geography = "county", 
                                variables = "POP", 
                                year = 2019,
                                geometry = TRUE) %>%
  filter(variable == "POP") %>%
  rename(ESTPOP2019 = value) 


# -------------------------------------------------------------------------
# construct variables -----------------------------------------------------
# -------------------------------------------------------------------------




#  list of alcohol terms
alcohol_terms <- c("beer", "wine", "alcohol", "liquor", "tequila", "ipa", "beers", 
                   "whiskey", "vodka", "champagne", "mimosas", "gin", "booze", "rum", "mimosa", 
                   "martini", "brews", "blood mary", "daiquiri", "chardonnay", "eggnog",
                   "merlot", "pinot", "pinot noir", "pina colada", "riesling",
                   "pinot grigio","sauvignon blanc","cabernet sauvignon","zinfandel",
                   "syrah","tequila sunrise", "sangiovese", "muscat", "claret",
                   "cabernet franc","gamay", "pinot gris","chenin blanc","petite sirah",
                   "gewurztraminer", "lychee martini", "cocktail mix", "rice wine",
                   "pinot blanc","malt beverage","malt beverages","maraschino", 
                   "fume blanc", "rice sake", "tavern", "taverns")


# convert timestamp variable to datetime and year-mon variable
# convert variables to appropriate type
df_in <- df_in %>%
  clean_convert_variables() %>%
  convert_timestamp() %>%
  mutate(alcohol = foodterm1 %in% alcohol_terms | 
           foodterm2 %in% alcohol_terms | 
           foodterm3 %in% alcohol_terms,
         healthy = healthyfoodsum > 0,
         fastfood = fastfoodsum > 0) %>%
  mutate(place_state = toupper(place_state) )




# -------------------------------------------------------------------------
# filter by period, food term validity, and state validity ----------------
# -------------------------------------------------------------------------

stateAbbr_valid <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI",
                     "ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN",
                     "MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH",
                     "OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA",
                     "WV","WI","WY")


# filter tweets within time period (May 15 - Jan 31, 2019 and 2020)
df_include <- df_in %>%
  filter(period_include == TRUE) %>%
  filter(!invalid_foodterm) %>%
  filter(place_state %in% stateAbbr_valid)
 
nrow(df_include)
nrow(df_include) / nrow(df_in) * 100 
table(df_include$period_covid)


# -------------------------------------------------------------------------
# filter by food consumption tweet classifier -----------------------------
# -------------------------------------------------------------------------



# 
df_consumed <- df_include %>%
  filter(label == 1)

nrow(df_consumed)
nrow(df_consumed) / nrow(df_include) * 100 
table(df_consumed$period_covid)






# -------------------------------------------------------------------------
# filter by geotagged tweets with valid county  ---------------------------
# -------------------------------------------------------------------------



df_geo <- df_consumed %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = "NAD83") %>%
  st_join(select(shp_state_pop, c("STATE", "geometry") )) %>%
  st_join( select(shp_county_pop, c("GEOID", "NAME", "geometry") ) ) %>%
  st_drop_geometry() %>%
  filter(!is.na(GEOID))
  

nrow(df_geo)
nrow(df_geo) / nrow(df_consumed) * 100 
table(df_geo$period_covid)


# -------------------------------------------------------------------------
# export data frame for modeling geotagged tweets
# -------------------------------------------------------------------------




df_geo_out <- df_geo %>%
  rename(StateAbbr = place_state) %>%
  filter(period_covid == "during") %>%
  select( c("tweet_id", "GEOID", "STATE", "StateAbbr","period_covid", "healthy", "fastfood", "alcohol") ) 

write.csv(df_geo_out, "Data/ProcessedData/df_filt_geotagged_geoid.csv",
          row.names = FALSE)



# -------------------------------------------------------------------------
# clean data format
# -------------------------------------------------------------------------


# df_freq <- get_food_term_counts(df)
df_freq <- get_food_term_counts(df_geo)

write.csv(df_freq, "Data/ProcessedData/df_filt_geo_topfoodterms.csv",
          row.names = FALSE)

# -------------------------------------------------------------------------



# compare geotagged tweets pre-covid vs during covid by county (spatial join)
df_covidcompare_geocounty <- df_geo %>%
  group_by(GEOID, NAME, STATE, period_covid) %>%
  get_summ_stats()%>%
  get_summ_diff() %>%
  arrange(GEOID)

sum(df_covidcompare_geocounty$N_before<10, na.rm=T)
sum(df_covidcompare_geocounty$N_during<10, na.rm=T)

write.csv(df_covidcompare_geocounty, "Data/ProcessedData/df_filt_geo_Difference_BeforeAfter_County.csv",
          row.names = FALSE)



# -------------------------------------------------------------------------
# by state
# -------------------------------------------------------------------------

shp_state_hex <- st_read(filePathHexShp) %>%
  rename(StateAbbr = iso3166_2) %>%
  mutate(STATE = gsub(" \\(United States\\)", "", google_nam))

uniqueStateAbrr <- unique(shp_state_hex$StateAbbr)
uniqueState <- unique(shp_state_hex$STATE)


# -------------------------------------------------------------------------


var_output_plot <- c("place_state", 
                     "p_healthy_p_change", 
                     "p_fastfood_p_change",
                     "p_eatout_p_change",
                     "p_alcohol_p_change",
                     "p_healthy_diff_a", 
                     "p_fastfood_diff_a",
                     "p_eatout_diff_a",
                     "p_alcohol_diff_a") 



# -------------------------------------------------------------------------


# compare tweets pre-covid vs during covid in the U.S.
df_covidcompare <- df_geo %>%
  group_by(period_covid) %>%
  get_summ_stats() %>%
  get_summ_diff()
  
write.csv(df_covidcompare, "Data/ProcessedData/df_filt_geo_Difference_BeforeAfter_All.csv",
          row.names = FALSE)


# formatted table for before-after analysis




# -------------------------------------------------------------------------
# plot pre-post comparison  -----------------------------------------------
# -------------------------------------------------------------------------



df_covidcompare_long <- df_covidcompare %>%
  select(c("p_healthy_before", "p_healthy_during",
           "p_fastfood_before", "p_fastfood_during",
           "p_alcohol_before", "p_alcohol_during") ) %>%
  pivot_longer(p_healthy_before:p_alcohol_during,
               names_prefix = "p_") %>%
  separate(name, c("Outcome", "period")) %>%
  mutate(Outcome = factor(Outcome,
                          levels = c("healthy", "fastfood", "alcohol"),
                          labels = c("Healthy Food", "Fast Food", "Alcohol")) ) %>%
  mutate(period = factor(period,
                          levels = c("before", "during"),
                          labels = c("Before Pandemic", "During Pandemic")) )

width_bar <- 0.4

fig1 <- ggplot(df_covidcompare_long) +
  geom_bar(aes(x = Outcome, y = value, fill = period), 
           col = "black",
           stat = "identity",
           width = width_bar,
           position = position_dodge( width= width_bar+.05 )) +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw()+
  theme(text = element_text(size = 12),
        legend.title = element_blank(),
        legend.background = element_rect(fill=alpha('white', 0)),
        legend.position = c(0.2, 0.85),
        legend.box.background = element_rect(colour = "black") ,
        panel.grid.major.x = element_blank() )+
  
  ylim(0, 30)+
  xlab("")+
  ylab("Food tweets referencing food category, %") 

fig1

ggsave( file.path("Output_Figures","FIGURE1_ChangeFoodTweets.tiff"),
        plot = fig1,
        height = 4, 
        width = 5, 
        device = "tiff")


# -------------------------------------------------------------------------
# save table with pre-post comparison -------------------------------------
# -------------------------------------------------------------------------


df_covidcompare_out <- df_covidcompare %>% select(p_healthy_before , p_healthy_during , p_healthy_diff_a, p_healthy_p_change,
                           p_fastfood_before , p_fastfood_during , p_fastfood_diff_a, p_fastfood_p_change,
                           p_alcohol_before , p_alcohol_during , p_alcohol_diff_a, p_alcohol_p_change) %>% 
  round(digits = 1) %>%
  
  t()
 
  

write.csv(df_covidcompare_out, "Data/ProcessedData/TABLE1_BeforeAfterComparison.csv",
          row.names = TRUE)

# -------------------------------------------------------------------------





# compare geotagged tweets pre-covid vs during covid by state (spatial join)
df_covidcompare_geostate <- df_geo %>%
  # filter(!invalid_foodterm & period_include & STATE %in% uniqueState) %>%
  group_by(STATE, period_covid) %>%
  get_summ_stats() %>%
  get_summ_diff() %>%
  arrange(STATE) %>%
  mutate_at(vars(contains('p_') & !ends_with("sd_lab")), function(x){ format(round(x, digits = 1), nsmall = 1) } )  %>%
  rename(State = STATE)


write.csv(df_covidcompare_geostate, "Data/ProcessedData/df_filt_geo_Difference_BeforeAfter_State.csv",
          row.names = FALSE)


df_covidcompare_geostate_healthy <- df_covidcompare_geostate %>%
  select(State, N_before, N_during, 
         p_healthy_before, p_healthy_during, p_healthy_diff_a, p_healthy_p_change) 

df_covidcompare_geostate_fastfood <- df_covidcompare_geostate %>%
  select(State, N_before, N_during, 
         p_fastfood_before, p_fastfood_during, p_fastfood_diff_a, p_fastfood_p_change) 

df_covidcompare_geostate_alcohol <- df_covidcompare_geostate %>%
  select(State, N_before, N_during, 
         p_alcohol_before, p_alcohol_during, p_alcohol_diff_a, p_alcohol_p_change) 

write.table(df_covidcompare_geostate_healthy, "Data/ProcessedData/eTable3_state_table_healthyfood.csv",
          row.names = FALSE, sep = ",", col.names = c("State", 
                                                      "# Food Tweets During\nPre-Pandemic Period",
                                                      "# Food Tweets During\nPandemic Period",
                                                      "Share of Healthy Food\nTweets During\nPre-Pandemic Period,\n%",
                                                      "Share of Healthy Food\nTweets During\nPandemic Period,\n%",
                                                      "Relative Percent Change\nin Share of Healthy Food\nTweets, %",
                                                      "Absolute Point Change\nin Share of Healthy Food\nTweets, %"))


write.table(df_covidcompare_geostate_fastfood, "Data/ProcessedData/eTable3_state_table_fastfood.csv",
            row.names = FALSE, sep = ",", col.names = c("State", 
                                                        "# Food Tweets During\nPre-Pandemic Period",
                                                        "# Food Tweets During\nPandemic Period",
                                                        "Share of Fast Food\nTweets During\nPre-Pandemic Period,\n%",
                                                        "Share of Fast Food\nTweets During\nPandemic Period,\n%",
                                                        "Relative Percent Change\nin Share of Fast Food\nTweets, %",
                                                        "Absolute Point Change\nin Share of Fast Food\nTweets, %"))

write.table(df_covidcompare_geostate_alcohol, "Data/ProcessedData/eTable3_state_table_alcohol.csv",
            row.names = FALSE, sep = ",", col.names = c("State", 
                                                        "# Food Tweets During\nPre-Pandemic Period",
                                                        "# Food Tweets During\nPandemic Period",
                                                        "Share of Alcohol\nTweets During\nPre-Pandemic Period,\n%",
                                                        "Share of Alcohol\nTweets During\nPandemic Period,\n%",
                                                        "Relative Percent Change\nin Share of Alcohol\nTweets, %",
                                                        "Absolute Point Change\nin Share of Alcohol\nTweets, %"))
