


rm(list=ls())

# set working directory to script
wd <- "/projectnb2/sph795/markhz/TwitterScripts"
setwd(wd)


# import packages and functions
library(tidyverse)
library(tictoc)
library(sf)
library(tidycensus)
library(RColorBrewer)
library(ggpubr)
source("twitter_food_functions.R")


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# import data
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


filePathHexShp <- "us_states_hexgrid/us_states_hexgrid.shp"

# load state tweet trends and join with state covariates
df_state <- read.csv("Data/ProcessedData/df_filt_geo_Difference_BeforeAfter_State.csv") %>%
  # rename("StateAbbr" = "STATE") %>%
  refactorSdLabels()

# load hex shape file for mapping and join to main data frame
shp_state <- st_read(filePathHexShp) %>%
  rename(StateAbbr = iso3166_2) %>%
  mutate(STATE = gsub(" \\(United States\\)", "", google_nam)) %>%
  left_join(df_state, by = c("STATE"="State")) %>%
  left_join(read.csv("Data/ProcessedData/State_covariates.csv"), by = "StateAbbr") 

# shp_hex_geostate <- shp_state_hex %>%
#   left_join(df_covidcompare_geostate, by = "STATE" )



# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# plot food tweet trends by state
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# healthy plots
# -------------------------------------------------------------------------


# healthy food absolute difference
map_healthy <- shp_state %>%
  plotHexStateMap(outcomeStr = "p_healthy_diff_sd_lab", id_label = "StateAbbr") +
  # scale_fill_manual(values = green_pal) +
  guides(fill=guide_legend(title="Difference in share of food tweets\nrelated to healthy food\n(During vs. Before Pandemic)")) +
  theme(legend.position = "none")

# ggsave( file.path("Output_Figures/df_filt_geo_HexStateMap_p_healthy_diff_sd.png"),
#         height = 6,
#         width = 8,
#         device = "png")


# fast food plots
# -------------------------------------------------------------------------

# fast food absolute difference
map_fastfood <- shp_state %>% 
  plotHexStateMap(outcomeStr = "p_fastfood_diff_sd_lab", id_label = "StateAbbr") +
  guides(fill=guide_legend(title="Difference in share of food tweets\nrelated to fast food\n(During vs. Before Pandemic)"))+
  theme(legend.position = "none")
map_fastfood
# ggsave( file.path("Output_Figures/df_filt_geo_HexStateMap_p_fastfood_diff_sd.png"),
#         height = 6,
#         width = 8,
#         device = "png")



# alcohol plots
# -------------------------------------------------------------------------

# alcohol absolute difference
map_alcohol <- shp_state %>%
  plotHexStateMap(outcomeStr = "p_alcohol_diff_sd_lab", id_label = "StateAbbr") +
  guides(fill=guide_legend(title="Difference in share of food tweets\n(During vs. Before Pandemic)")) +
  theme(legend.box.background = element_rect(colour = "black") )
  # theme(legend.position = "none")
map_alcohol
# ggsave( file.path("Output_Figures/df_filt_geo_HexStateMap_p_alcohol_diff_sd.png"),
#         height = 6,
#         width = 8,
#         device = "png")



# combine maps
# -------------------------------------------------------------------------

map_combined <- ggarrange(map_healthy, map_fastfood, map_alcohol, 
          labels = c("Healthy Food", "Fast Food", "Alcohol"),
          widths = c(1,1,1.5),
          ncol = 3, nrow = 1,
          hjust = -0,
          vjust = 10)

map_combined

ggsave( file.path("Output_Figures/FIGURE2_StateChangeFoodTweets.tiff"),
        plot = map_combined,
        height = 6,
        width = 16,
        device = "tiff")



# raw tweet counts - state
# -------------------------------------------------------------------------

# plot hexbins

green_pal <- brewer.pal(name="Greens",n=7)[1:5]
blue_pal <- brewer.pal(name="Blues",n=7)[1:5]
purple_pal <- brewer.pal(name="Purples",n=7)[1:5]
orange_pal <- brewer.pal(name="Oranges",n=7)[1:5]

quantile(shp_state$N, seq(0,1,by=0.2))
shp_state %>%
  mutate(n_quant = cut(N, breaks = quantile(N, seq(0,1,by=0.2)) , include.lowest = TRUE,
                       labels = c("1.1K - 4.9K",
                                  "5.0K - 9.7K",
                                  "9.8K - 19.4K",
                                  "19.5K - 32.0K",
                                  "32.1K - 189K") ) ) %>%
  ggplot() + 
  geom_sf(aes(fill = n_quant )) +
  # geom_sf(aes(fill = cut_number(!!sym(outcomeStr), n = 5) ) ) +
  geom_sf_text(aes(label = StateAbbr), size = 3) +
  theme_void() +
  theme( plot.background = element_rect(fill = "#f5f5f2", color = NA) )  +
  scale_fill_manual(values = purple_pal) +
  # theme(legend.title = element_blank()) +
  guides(fill=guide_legend(title="Total Number of\nGeotagged Food Tweets")) +
  theme(legend.box.background = element_rect(colour = "black") )

ggsave( file.path("Output_Figures/FIGURE_State_FoodTweetCounts.jpg"),
        height = 6,
        width = 8,
        device = "jpg")



