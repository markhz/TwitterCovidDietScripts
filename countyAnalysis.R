


rm(list=ls())

# set working directory to script
wd <- "/projectnb2/sph795/markhz/TwitterScripts"
setwd(wd)


# import packages and functions
library(tidyverse)
library(broom)
library(pls) 
library(tictoc)
library(RColorBrewer)
library(usmap)
library(ggpubr)
library("gridExtra")
source("twitter_food_functions.R")


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# import data
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


dataOutPath <- file.path("Data", "ProcessedData")
figOutPath <- "Output_Figures"

filePathHexShp <- "us_states_hexgrid/us_states_hexgrid.shp"

df_geo <- read.csv(file.path(dataOutPath, "df_filt_geotagged_geoid.csv")) %>%
  rename(FIPS = GEOID)
  


# load state tweet trends and join with state covariates
df_county_in <- read.csv(file.path(dataOutPath, "County_covariates.csv")) %>%
  left_join( read.csv(file.path(dataOutPath, "df_filt_geo_Difference_BeforeAfter_County.csv")) %>% rename("FIPS" = "GEOID"), 
            by = "FIPS") %>%
  mutate(fips = sprintf("%05d", FIPS)) %>%
  refactorSdLabels() 



fileOut_suffix <- "_FINAL_edit"
denominator_thresh <- 10

# get counties with sufficient data
df_county <- df_county_in %>%
  # filter denominator less than threshold per county
  filter(N_before >= denominator_thresh  & N_during >= denominator_thresh) %>%
  filter(!is.na(pctchange_residential )) %>%
  mutate(p_healthy_before = p_healthy_before * 100,
         p_fastfood_before = p_fastfood_before * 100,
         p_alcohol_before = p_alcohol_before * 100)


nrow(df_county_in)
nrow(df_county)



# numerator_thresh <- 5
# 
# # get counties with sufficient data
# df_county <- df_county_in %>%
#   # filter numerators less than 5 per county
#   filter(n_fastfood_before + n_fastfood_during > numerator_thresh &
#            n_healthy_before + n_healthy_during > numerator_thresh &
#            n_alcohol_before + n_alcohol_during > numerator_thresh ) %>%
#   filter(!is.na(pctchange_grocery ))






# -------------------------------------------------------------------------
# regression models
# -------------------------------------------------------------------------



# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

dem_covariates <- c("pctYoung","pctOld","pctLowIncome","pctHighIncome",
                    "pctBlack", "pctHisp", "pctWhite")


baseline_cov <-  c("p_healthy_before", "p_fastfood_before","p_alcohol_before")

# outcome <- "p_healthy_diff_a"
other_covariates <- c(
                "rest_per10k", "groc_per10k", "liq_per10k", "bars_per10k",
                "pct_obese", "pct_bingedrink",
                "weeks_closed_rst", "weeks_closed_bar",
                "pctchange_grocery","pctchange_retail_rec", "pctchange_residential")

# "pctchange_grocery", "pctchange_retail_rec",

# allVar <- c(outcome, covariates, baseline_cov)


# join with select covariates
df_mdl_tmp <- df_geo %>% 
  select("FIPS","StateAbbr", "healthy", "fastfood", "alcohol") %>%
  left_join( df_county %>% select(c("FIPS", dem_covariates, other_covariates, baseline_cov)) ,
             by = "FIPS") 




# drop excluded counties
df_mdl <- df_mdl_tmp %>%
  drop_na()


nrow(df_mdl)

nrow(df_mdl) / nrow(df_mdl_tmp)

# corMat<-cor(df_mdl, use = "complete.obs" )
# corMat_high <- ifelse(abs(corMat) > 0.5, corMat, NA)




# prcomp(df_mdl%>% select(-c("FIPS", "healthy", "fastfood", "alcohol") ) %>% drop_na(), scale=TRUE)
# -------------------------------------------------------------------------
# regression healthy food
# -------------------------------------------------------------------------

height_fig <- 3
width_fig <- 5.5


healthy_food_cov <- c("p_healthy_before",
                      dem_covariates,
                      "groc_per10k",
                      "rest_per10k",
                      # "weeks_closed_rst",
                      # "pctchange_grocery",
                      "pctchange_residential",
                      "StateAbbr")


healthy_food_all_var <- c("healthy", healthy_food_cov) 

df_mdl_healthy <- df_mdl %>%
  select(healthy_food_all_var) 

mdl_healthy <- glm( healthy ~ .,
                  data = df_mdl_healthy,
                  na.action=na.exclude,
                  family = "binomial")
summary(mdl_healthy)
AIC(mdl_healthy)

mdl_healthy_df <- gen_mdl_df(mdl_healthy)
mdl_healthy_df$Variable <- recode_factor(mdl_healthy_df$Variable, 
                                          "pctLowIncome" = "% Low Income",
                                          "pctHighIncome" = "% High Income",
                                          "pctBlack" = "% Non-Hispanic Black",
                                          "pctWhite" = "% Non-Hispanic White",
                                          "pctHisp" = "% Hispanic",
                                          "pctOld" = "% Age 65+ years",
                                          "pctYoung" = "% Age 10-24 years",
                                          "groc_per10k" = "Grocery Stores per 10,000",
                                         "rest_per10k" = "Restaurants per 10,000",
                                          "weeks_closed_rst" = "State Policy\nRestaurant Closures (weeks)",
                                         "pctchange_grocery" = "Change in Visits to\nGrocery Stores/Pharmacies",
                                          "pctchange_residential" = "% Change in Time Spent in\nPlaces of Residence")



fig_healthy <- displayModelResults(mdl_healthy_df %>% 
                                     filter( Variable %in% c("% Change in Time Spent in\nPlaces of Residence",
                                                             "Grocery Stores per 10,000",
                                                             "Restaurants per 10,000")  ) ,
                                   CALC_PROB = FALSE)+
                                     # filter(Variable != "p_healthy_before" & 
                                              # !startsWith(as.character(Variable), "StateAbbr") ) )+
  # ylim(y_lim) +
  ylab("OR (95% CI)") +
  scale_y_continuous(breaks = seq(0.92, 1.08, by = 0.02),
                     # limits = y_lim,
                     # limits = c(0.98, 1.02)
                     )

fig_healthy

ggsave(file.path(figOutPath, paste0("df_filt_geo_LogRegression_coef_healthy", fileOut_suffix, ".png")) , 
       plot = fig_healthy,
       device = "png",
       height = height_fig,
       width = width_fig)

write.csv(mdl_healthy_df, 
          file.path(dataOutPath, paste0("df_filt_geo_LogRegression_coef_healthy", fileOut_suffix, ".csv")) , 
          row.names = FALSE)

# -------------------------------------------------------------------------
# regression fast food
# -------------------------------------------------------------------------



fastfood_food_cov <- c("p_fastfood_before",
                      dem_covariates,
                      "groc_per10k",
                      "rest_per10k",
                      # "weeks_closed_rst",
                      "pctchange_residential",
                      "StateAbbr")

fast_food_all_var <- c("fastfood", fastfood_food_cov) 

df_mdl_fastfood <- df_mdl %>%
  select(fast_food_all_var) 
  # select(-c("FIPS",
  #           "healthy", "alcohol", "liq_per10k", "bars_per10k",
  #           "p_healthy_before","p_alcohol_before",
  #           # baseline_cov,
  #           "pct_bingedrink", "weeks_closed_bar"))




mdl_fastfood <- glm( fastfood ~ .,
                    data = df_mdl_fastfood,
                    na.action=na.exclude,
                    family = "binomial")
summary(mdl_fastfood)
AIC(mdl_fastfood)

mdl_fastfood_df <- gen_mdl_df(mdl_fastfood)
mdl_fastfood_df$Variable <- recode_factor(mdl_fastfood_df$Variable, 
                                         "pctLowIncome" = "% Low Income",
                                         "pctHighIncome" = "% High Income",
                                         "pctBlack" = "% Non-Hispanic Black",
                                         "pctWhite" = "% Non-Hispanic White",
                                         "pctHisp" = "% Hispanic",
                                         "pctOld" = "% Age 65+ years",
                                         "pctYoung" = "% Age 10-24 years",
                                         "groc_per10k" = "Grocery Stores per 10,000",
                                         "rest_per10k" = "Restaurants per 10,000",
                                         "weeks_closed_rst" = "State Policy\nRestaurant Closures (weeks)",
                                         "pctchange_residential" = "% Change in Time Spent in\nPlaces of Residence")


fig_fastfood <- displayModelResults(mdl_fastfood_df %>% 
                                      filter( Variable %in% c("% Change in Time Spent in\nPlaces of Residence",
                                                              "Grocery Stores per 10,000",
                                                              "Restaurants per 10,000")  ) )+
                                      # filter(Variable != "p_fastfood_before" & !startsWith(as.character(Variable), "StateAbbr") )  ) +
  # ylim(0.94, 1.06) +
  # ylim(y_lim) +
  ylab("Odds of Food Tweet Referencing Fast Food\nOR (95% CI)") +
  scale_y_continuous(breaks = seq(0.92, 1.08, by = 0.02),
                     # limits = y_lim,
                     # limits = c(0.94, 1.06)
                     )
                    # %>% filter(Variable != "p_fastfood_before") )
fig_fastfood

ggsave(file.path(figOutPath, paste0("df_filt_geo_LogRegression_coef_fastfood", fileOut_suffix, ".png")) , 
       plot = fig_fastfood,
       device = "png",
       height = height_fig,
       width = width_fig)

write.csv(mdl_fastfood_df, 
          file.path(dataOutPath, paste0("df_filt_geo_LogRegression_coef_fastfood", fileOut_suffix, ".csv")) , 
          row.names = FALSE)



# -------------------------------------------------------------------------
# regression alcohol
# -------------------------------------------------------------------------
alcohol_food_cov <- c("p_alcohol_before",
                      dem_covariates,
                      "bars_per10k", 
                      "liq_per10k",
                      # "weeks_closed_bar",
                      "pctchange_residential",
                      "StateAbbr")


# alcohol_food_cov <- c("p_alcohol_before",
#                       "liq_per10k",
#                       "StateAbbr")


alcohol_all_var <- c("alcohol", alcohol_food_cov) 


df_mdl_alcohol <- df_mdl %>%
  select(alcohol_all_var)
  # select(-c("FIPS",
  #           "healthy", "fastfood", "rest_per10k", "groc_per10k", 
  #           "p_healthy_before","p_fastfood_before",
  #           # baseline_cov,
  #           "pct_obese", "weeks_closed_rst"))

mdl_alcohol <- glm( alcohol ~ .,
                     data = df_mdl_alcohol,
                     na.action=na.exclude,
                     family = "binomial")
summary(mdl_alcohol)
AIC(mdl_alcohol)


mdl_alcohol_df <- gen_mdl_df(mdl_alcohol) %>%
  mutate(Variable = factor(Variable))

mdl_alcohol_df$Variable <- recode_factor(mdl_alcohol_df$Variable, 
              "pctLowIncome" = "% Low Income",
              "pctHighIncome" = "% High Income",
              "pctBlack" = "% Non-Hispanic Black",
              "pctWhite" = "% Non-Hispanic White",
              "pctHisp" = "% Hispanic",
              "pctOld" = "% Age 65+ years",
              "pctYoung" = "% Age 10-24 years",
              "bars_per10k" = "Bars per 10,000",
              "liq_per10k" = "Liquor Stores per 10,000",
              "weeks_closed_bar" = "State Policy\n            Bar Closures (weeks)",
              "pctchange_residential" = "% Change in Time Spent in\nPlaces of Residence")





fig_alcohol <- displayModelResults(mdl_alcohol_df %>%
                                     filter( Variable %in% c("% Change in Time Spent in\nPlaces of Residence",
                                                             "Liquor Stores per 10,000",
                                                             "Bars per 10,000")  ) )+
                                     # filter(Variable != "p_alcohol_before" & !startsWith(as.character(Variable), "StateAbbr") ) ) +
  # ylim(0.97, 1.03)+
  # ylim(y_lim) +
  ylab("Odds of Food Tweet Referencing Alcohol\nOR (95% CI)") +
  scale_y_continuous(breaks = seq(0.92, 1.08, by = 0.02),
                     # limits = y_lim,
                     # limits = c(0.97, 1.03)
                     )
                    # %>% filter(Variable != "p_alcohol_before") )
fig_alcohol

ggsave(file.path(figOutPath, paste0("df_filt_geo_LogRegression_coef_alcohol", fileOut_suffix, ".png")) , 
      plot = fig_alcohol,
      device = "png",
      height = height_fig,
      width = width_fig)

write.csv(mdl_alcohol_df, 
          file.path(dataOutPath, paste0("df_filt_geo_LogRegression_coef_alcohol", fileOut_suffix, ".csv")) , 
          row.names = FALSE)





# 
# 
# 
# 
# 
y_lim <- c(0.91, 1.09)
x_lim <- c(0.9, 3.1)
y_breaks <- seq(0.92, 1.08, by = 0.02)


# -------------------------------------------------------------------------


gen_table_plot <- function (fig_in, mdl){
  table_base <- ggplot(mdl, aes(y=Variable)) +
    ylab(NULL) + xlab("  ") + 
    theme(plot.title = element_text(hjust = 0.5, size=12), 
          axis.text.x = element_blank(),
          # axis.text.x = element_text(color="white", hjust = -3, size = 25), ## This is used to help with alignment
          axis.line = element_blank(),
          axis.text.y = element_blank(), 
          axis.ticks = element_blank(),
          axis.title.y = element_blank(), 
          legend.position = "none",
          panel.background = element_rect(fill = "white"),
          panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          plot.background = element_rect(fill = "white"))+
          # plot.background = element_blank()) +
          
    ylim(x_lim[1]-.15 , x_lim[2])
  
  ## Variable Name
  tab0 <- table_base +
    labs(title = "space") +
    geom_text(aes(y = Index, x = 1, label = Variable, size = 2)) + ## decimal places
    ggtitle(expression(underline("Variable"))) + 
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  ## OR point estimate table
  tab1 <- table_base +
    labs(title = "space") +
    geom_text(aes(y = Index, x = 1, label = OR_CI_str, size = 2)) + ## decimal places
    ggtitle(expression(underline("    OR (95% CI)    "))) + 
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  # lay <-  matrix(c(1,2,3,3), nrow = 1)
  # grid.arrange(tab0, tab1, fig_in,  layout_matrix = lay)
  lay <-  matrix(c(1,1,1,2), nrow = 1)
  grid.arrange( fig_in, tab1, layout_matrix = lay)
  
  # return(fig_out)
}


# # -------------------------------------------------------------------------


mdl_healthy_df_idx <- mdl_healthy_df %>%
  filter( Variable %in% c("% Change in Time Spent in\nPlaces of Residence",
                          "Grocery Stores per 10,000",
                          "Restaurants per 10,000")  ) %>% 
  mutate(Index = 1:3) 


fig_healthy_format <- displayModelResults(mdl_healthy_df_idx)+
  ylab("OR (95% CI)") +
  scale_y_continuous(breaks = y_breaks,
                     limits = y_lim) +
  ggtitle("Odds of Healthy Food Tweet")+
  scale_x_continuous(name = "", 
                     breaks = sort(mdl_healthy_df_idx$Index), 
                     minor_breaks = sort(mdl_healthy_df_idx$Index), 
                     labels = mdl_healthy_df_idx$Variable,
                     limits = x_lim)
  
  # xlim()


fig_healthy_format

fig_healthy_out <- gen_table_plot(fig_healthy_format, mdl_healthy_df_idx)



# -------------------------------------------------------------------------

mdl_fastfood_df_idx <- mdl_fastfood_df %>%
  filter( Variable %in% c("% Change in Time Spent in\nPlaces of Residence",
                          "Grocery Stores per 10,000",
                          "Restaurants per 10,000")  ) %>% 
  mutate(Index = 1:3) 


fig_fastfood_format <- displayModelResults(mdl_fastfood_df_idx)+
  ylab("OR (95% CI)") +
  scale_y_continuous(breaks = y_breaks,
                     limits = y_lim  ) +
  ggtitle("Odds of Fast Food Tweet")+
  scale_x_continuous(name = "", 
                     breaks = sort(mdl_fastfood_df_idx$Index), 
                     minor_breaks = sort(mdl_fastfood_df_idx$Index), 
                     labels = mdl_fastfood_df_idx$Variable,
                     limits = x_lim) 

fig_fastfood_format

fig_fastfood_out <- gen_table_plot(fig_fastfood_format, mdl_fastfood_df_idx)



# -------------------------------------------------------------------------

mdl_alcohol_df_idx <- mdl_alcohol_df %>%
  filter( Variable %in% c("% Change in Time Spent in\nPlaces of Residence",
                          "Liquor Stores per 10,000",
                          "Bars per 10,000")  ) %>%
  mutate(Index = 1:3) 


fig_alcohol_format <- displayModelResults(mdl_alcohol_df_idx)+
  ylab("OR (95% CI)") +
  scale_y_continuous(breaks = y_breaks,
                     limits = y_lim  ) +
  ggtitle("Odds of Alcohol Tweet")+
  scale_x_continuous(name = "", 
                     breaks = sort(mdl_alcohol_df_idx$Index), 
                     minor_breaks = sort(mdl_alcohol_df_idx$Index), 
                     labels = mdl_alcohol_df_idx$Variable,
                     limits = x_lim)

fig_alcohol_format

fig_alcohol_out <- gen_table_plot(fig_alcohol_format, mdl_alcohol_df_idx)



fig_out <- ggarrange(fig_healthy_out, fig_fastfood_out, fig_alcohol_out, ncol = 1)


ggsave(file.path(figOutPath, paste0("FIGURE3_OR_CI.tiff")) , 
       plot = fig_out,
       device = "tiff",
       height = 10,
       width = 10)

# 
# 
# # -------------------------------------------------------------------------
# 
# 
# 
# # -------------------------------------------------------------------------
# 
# # join with select variables
# df_mdl_healthy <- df_geo %>% select(c("FIPS", outcomeVar) ) %>%
#   left_join( df_county %>% select(c("FIPS", covariates, baseline_cov)) ,
#              by = "FIPS") %>%
#   select(-c("FIPS"))
# 
# 
# mdl_healthy <- glm( healthy ~ .,
#                     data = df_mdl_healthy,
#                     na.action=na.exclude,
#                     family = "binomial")
# summary(mdl_healthy)
# 
# 
# # -------------------------------------------------------------------------
# 
# 
# 
# 
# 
# 
# 
# mdl_healthy_agg <- lm(p_healthy_during~ 
#                     rest_per + groc_per + 
#             pctchange_grocery + pctchange_retail_rec +
#             OBESITY +
#             days_closed_rst +
#             ICE_I + ICE_H +
#             p_healthy_before ,
#           data = df_county,
#           na.action=na.exclude)
# summary(mdl_healthy_agg )
# 
# mdl <- lm(p_fastfood_during~ rest_per + groc_per + 
#             pctchange_grocery + pctchange_retail_rec +
#             OBESITY +
#             days_closed_rst +
#             ICE_I + ICE_H +
#             p_fastfood_before ,
#            data = df_county,
#           na.action=na.exclude)
# summary(mdl)
# 
# 
# 
# # alcohol model
# mdl_alc <- lm(p_alcohol_during~ rest_per + groc_per + 
#             pctchange_retail_rec +
#             BINGE +
#             days_closed_bar + days_closed_rst +
#             ICE_I + ICE_H + ICE_B +
#             p_alcohol_before ,
#           data = df_county,
#           na.action=na.exclude)
# summary(mdl_alc)
# # -------------------------------------------------------------------------
# # plots
# # -------------------------------------------------------------------------
# 
# 
# 
# 
# ggplot(df_county) + geom_point(aes(x = OBESITY, y = p_fastfood_during), alpha = 0.5 )
# ggplot(df_county) + geom_point(aes(x = OBESITY, y = p_healthy_during), alpha = 0.5 )
# ggplot(df_county) + geom_point(aes(x = BINGE, y = p_alcohol_during), alpha = 0.5 )
# 
# ggplot(df_county) + geom_point(aes(x = ICE_I, y = OBESITY), alpha = 0.5 )
# 
# ggplot(df_county) + geom_point(aes(x = E_NHBlk/E_POP_r, y = p_healthy_during), alpha = 0.5 )
# 
# ggplot(df_county) + geom_point(aes(x = ICE_B, y = p_fastfood_before), alpha = 0.5 )
# ggplot(df_county) + geom_point(aes(x = ICE_B, y = p_fastfood_diff_sd), alpha = 0.5 )
# 
# nrow(df_county_in)
# nrow(df_county)
# 
# sum(df_county$N_before<50, na.rm = TRUE)
# sum(df_county$N_during<50, na.rm = TRUE)
# 
# sum(df_county$n_fastfood_before<5, na.rm = TRUE)
# 
# 
# 
# # counties_sf <- get_urbn_map("counties", sf = TRUE)
# 




# -------------------------------------------------------------------------

# 
# # load state shapefile with population estimates
# shp_county <- get_estimates(geography = "county",
#                                 variables = "POP",
#                                 year = 2019,
#                                 geometry = TRUE) %>%
#   filter(variable == "POP") %>%
#   rename("POP2019" = "value",
#          "fips" = "GEOID") %>%
#   select(c("fips", "POP2019")) %>%
#   left_join(df_county, by = "fips")
# 
# 
# # df_county_plot <- df_county %>% select("FIPS", "p_healthy_diff_sd_lab") %>%
# #   rename("fips" = "FIPS",
# #          "values" = "p_healthy_diff_sd_lab")
# 
# 
# 
# 
# # # healthy food difference
# shp_county %>%
#   plotCountyMap(outcomeStr = "p_healthy_diff_sd_lab") +
#   guides(fill=guide_legend(title="Difference in share of food tweets\nrelated to healthy food\n(During vs. Before Pandemic)"))
# 
# ggsave( file.path("Output_Figures",paste0("_df_filt_CountyMap_p_healthy_diff_sd_lab_final.png")),
#         height = 6, width = 9, device = "png")
# 
# 
# # # fast food difference
# shp_county %>%
#   plotCountyMap(outcomeStr = "p_fastfood_diff_sd_lab") +
#   guides(fill=guide_legend(title="Difference in share of food tweets\nrelated to fast food\n(During vs. Before Pandemic)"))
# 
# ggsave( file.path("Output_Figures",paste0("_df_filt_CountyMap_p_fastfood_diff_sd_lab_final.png")),
#         height = 6, width = 9, device = "png")
# 
# 
# 
# # # alcohol difference
# shp_county %>%
#   plotCountyMap(outcomeStr = "p_alcohol_diff_sd_lab") +
#   guides(fill=guide_legend(title="Difference in share of food tweets\nrelated to alcohol\n(During vs. Before Pandemic)"))
# 
# ggsave( file.path("Output_Figures",paste0("_df_filt_CountyMap_p_alcohol_diff_sd_lab_final.png")),
#         height = 6, width = 9, device = "png")
