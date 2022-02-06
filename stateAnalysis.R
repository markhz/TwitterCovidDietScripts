


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




# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# plot clusters
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# 
# 
# # changes in trips to retail/recreational places vs food tweet trends
# plotStateClusters(df_state, "pctchange_retail_rec", "p_fastfood_diff_sd", n_clusters = 4) +
#   ggtitle(paste0("Correlation coef: ", round(cor(df_state$pctchange_retail_rec_sd, df_state$p_fastfood_diff_sd), 3 ) ) )
# 
# plotStateClusters(df_state, "pctchange_retail_rec", "p_healthy_diff_sd", n_clusters = 4) +
#   ggtitle(paste0("Correlation coef: ", round(cor(df_state$pctchange_retail_rec_sd, df_state$p_healthy_diff_sd), 3 ) ) )
# 
# plotStateClusters(df_state, "pctchange_retail_rec", "p_alcohol_diff_sd", n_clusters = 4) +
#   ggtitle(paste0("Correlation coef: ", round(cor(df_state$pctchange_retail_rec_sd, df_state$p_alcohol_diff_sd), 3 ) ) )
# 
# # changes in trips to grocery vs food tweet trends
# plotStateClusters(df_state,  "pctchange_grocery_sd", "p_healthy_diff_sd", n_clusters = 4) +
#   ggtitle(paste0("Correlation coef: ", round(cor(df_state$pctchange_grocery_sd, df_state$p_healthy_diff_sd), 3 ) ) )
# ggsave( file.path("Output_Figures/State_Scatter_Healthy_Grocery.png"),
#         height = 6,
#         width = 8,
#         device = "png")
# 
# # changes in trips to grocery vs food tweet trends
# plotStateClusters(df_state,  "days_closed_rst_sd", "p_healthy_diff_sd", n_clusters = 4) +
#   ggtitle(paste0("Correlation coef: ", round(cor(df_state$days_closed_rst_sd, df_state$p_healthy_diff_sd), 3 ) ) )
# ggsave( file.path("Output_Figures/State_Scatter_Healthy_RestClose.png"),
#         height = 6,
#         width = 8,
#         device = "png")
# 
# 
# 
# cor(df_state$pctchange_grocery_sd, df_state$p_healthy_diff_sd)
# plotStateClusters(df_state, "p_fastfood_diff_sd", "pctchange_grocery", n_clusters = 4)
# cor(df_state$pctchange_grocery_sd, df_state$p_fastfood_diff_sd)
# plotStateClusters(df_state, "p_alcohol_diff_sd", "pctchange_grocery", n_clusters = 4)
# cor(df_state$pctchange_grocery_sd, df_state$p_alcohol_diff_sd)
# 
# plotStateClusters(df_state, "days_closed_rst", "pctchange_retail_rec", n_clusters = 4)
# plotStateClusters(df_state, "days_closed_bar", "pctchange_retail_rec", n_clusters = 4)
# 
# cor(df_state$days_closed_rst_sd, df_state$pctchange_retail_rec_sd)
# cor(df_state$days_closed_bar_sd, df_state$pctchange_retail_rec_sd)
# 



# -------------------------------------------------------------------------
# plot correlations between place state and geotagged state spatial join
# -------------------------------------------------------------------------
# 
# 
# # compare metrics -- full versus geotagged sample (correlation matrix)
# varPivot <- c("p_healthy_before", "p_healthy_during",
#               "p_fastfood_before", "p_fastfood_during",
#               "p_eatout_before", "p_eatout_during",
#               "p_alcohol_before", "p_alcohol_during")
# 
# 
# df_placestate_long <- shp_state %>%
#   #drop geometry
#   st_drop_geometry(  ) %>%
#   # select
#   select( c("StateAbbr", varPivot) ) %>%
#   pivot_longer(varPivot , 
#                names_to = c("Placeholder","Outcome", "Period"), 
#                values_to = "p_placestate",
#                names_sep = "_" ) %>%
#   select(-c("Placeholder"))
# 
# df_geostate_long <- shp_hex_geostate %>%
#   #drop geometry
#   st_drop_geometry(  ) %>%
#   # select
#   select( c("StateAbbr", varPivot) ) %>%
#   pivot_longer(varPivot , 
#                names_to = c("Placeholder","Outcome", "Period"), 
#                values_to = "p_geostate",
#                names_sep = "_" ) %>%
#   select(-c("Placeholder"))
# 
# df_cor_comp <- df_placestate_long %>% 
#   left_join(df_geostate_long, by = c("StateAbbr","Outcome","Period"))
# 
# # limit_axis <- c(0,0.7)
# limit_axis <- c(.05,.35)
# df_cor_comp%>%ggplot() +
#   geom_text(aes(x = p_placestate, y = p_geostate, label = StateAbbr) , size = 4) +
#   facet_grid(rows = vars(Outcome), cols = vars(Period) ) +
#   geom_abline(slope=1,intercept=0) +
#   ylim(limit_axis)+
#   xlim(limit_axis) +
#   theme(text = element_text(size = 18))
# 


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# # fast food percent change
# shp_state %>%
# plotHexStateMap(outcomeStr = "p_fastfood_p_change", id_label = "StateAbbr") +
#   scale_fill_manual(values = blue_pal) +
#   guides(fill=guide_legend(title="Percent change in share of\nfast food tweets (%)"))
# 
# ggsave( file.path("Output_Figures/HexStateMap_p_fastfood_p_change.png"),
#         height = 6,
#         width = 8)
# 
# 
# 
# # fast food absolute difference (geotagged)
# shp_hex_geostate %>% 
#   plotHexStateMap(outcomeStr = "p_fastfood_diff_sd_lab", id_label = "StateAbbr") +
#   guides(fill=guide_legend(title="Difference in share of food tweets\nrelated to fast food\n(During vs. Before Pandemic)"))
# 
# ggsave( file.path("Output_Figures/HexStateMap_geotagged_p_fastfood_diff_sd.png"),
#         height = 6,
#         width = 8,
#         device = "png")


# -------------------------------------------------------------------------


# # healthy food percent change
# shp_state %>%
#   plotHexStateMap(outcomeStr = "p_healthy_p_change", id_label = "StateAbbr") +
#   scale_fill_manual(values = green_pal) +
#   guides(fill=guide_legend(title="Percent change in share of\nhealthy food tweets (%)"))
# 
# ggsave( file.path("Output_Figures/HexStateMap_p_healthy_p_change.png"),
#         height = 6,
#         width = 8)
# 
# # healthy food absolute difference (geotagged)
# shp_hex_geostate %>%
#   plotHexStateMap(outcomeStr = "p_healthy_diff_sd_lab", id_label = "StateAbbr") +
#   # scale_fill_manual(values = green_pal) +
#   guides(fill=guide_legend(title="Difference in share of food tweets\nrelated to healthy food\n(During vs. Before Pandemic)"))
# 
# ggsave( file.path("Output_Figures/HexStateMap_geotagged_p_healthy_diff_sd.png"),
#         height = 6,
#         width = 8,
#         device = "png")


# -------------------------------------------------------------------------



# # alcohol percent change
# shp_state %>%
#   plotHexStateMap(outcomeStr = "p_alcohol_p_change", id_label = "StateAbbr") +
#   scale_fill_manual(values = orange_pal) +
#   guides(fill=guide_legend(title="Percent change in share of\nalcohol tweets (%)"))
# 
# ggsave( file.path("Output_Figures/HexStateMap_p_alcohol_p_change.png"),
#         height = 6,
#         width = 8)
# 
# 
# # write.csv(df_state, file = file.path(wd, "tweetStateFreq.csv"), row.names = FALSE)
# 
# 
# # alcohol absolute difference (geotagged)
# shp_hex_geostate %>%
#   plotHexStateMap(outcomeStr = "p_alcohol_diff_sd_lab", id_label = "StateAbbr") +
#   # scale_fill_manual(values = orange_pal) +
#   guides(fill=guide_legend(title="Difference in share of food tweets\nrelated to alcohol\n(During vs. Before Pandemic)"))
# 
# ggsave( file.path("Output_Figures/HexStateMap_geotagged_p_alcohol_diff_sd.png"),
#         height = 6,
#         width = 8,
#         device = "png")
# 
# 


# -------------------------------------------------------------------------
# eat out plots
# -------------------------------------------------------------------------

# # eating out absolute difference
# shp_state %>%
#   plotHexStateMap(outcomeStr = "p_eatout_diff_sd_lab", id_label = "StateAbbr") +
#   guides(fill=guide_legend(title="Difference in share of food tweets\nrelated to eating out\n(During vs. Before Pandemic)"))
# 
# ggsave( file.path("Output_Figures/HexStateMap_p_eatout_diff_sd.png"),
#         height = 6,
#         width = 8,
#         device = "png")
# 
# # eating out percent change
# shp_state %>%
#   plotHexStateMap(outcomeStr = "p_eatout_p_change", id_label = "StateAbbr") +
#   scale_fill_manual(values = purple_pal) +
#   guides(fill=guide_legend(title="Percent change in share of\neating out tweets (%)"))
# 
# ggsave( file.path("Output_Figures/HexStateMap_p_eatout_p_change.png"),
#         height = 6,
#         width = 8)
# 
# # eating out absolute difference (geotagged)
# shp_hex_geostate %>%
#   plotHexStateMap(outcomeStr = "p_eatout_diff_sd_lab", id_label = "StateAbbr") +
#   guides(fill=guide_legend(title="Difference in share of food tweets\nrelated to eating out\n(During vs. Before Pandemic)"))
# 
# ggsave( file.path("Output_Figures/HexStateMap_geotagged_p_eatout_diff_sd.png"),
#         height = 6,
#         width = 8,
#         device = "png")

# -------------------------------------------------------------------------



# # summarize tweets by state (full time period)
# df_state_all <- df %>%
#   filter(!invalid_foodterm) %>%
#   group_by(place_state) %>%
#   get_summ_stats()
# 
# # summarize tweets by state (by dataset year)
# df_state_by_year <- df %>%
#   filter(!invalid_foodterm) %>%
#   group_by(place_state) %>%
#   get_summ_stats()


