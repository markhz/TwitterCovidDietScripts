

# function convert_timestamp
#   converts timestamp variable from string to year-mon variable 
#
# Input:
convert_timestamp <- function(df){
  df_out <- df %>%
    mutate(timestamp = as.Date(timestamp, "%a %b %d %H:%M:%S %z %Y"),
           yearmon = as.yearmon(timestamp),
           year = year(timestamp) ) %>%
    mutate(period_covid = as.factor(ifelse(between(timestamp, as.Date("2019-05-15"), as.Date("2020-01-31")) , "before",
                                 ifelse(between(timestamp, as.Date("2020-05-15"), as.Date("2021-01-31")), "during", 
                                        "exclude")) ),
      # precovid = between(timestamp, as.Date("2019-05-15"), as.Date("2020-01-31")),
           # duringcovid = between(timestamp, as.Date("2020-05-15"), as.Date("2021-01-31")),
           period_include = period_covid %in% c("before", "during")  )  
  
  return(df_out)
}


# -------------------------------------------------------------------------

clean_convert_variables <- function(df) {
  df_out <- df %>%
    # convert to numerics
    mutate(caloriesum = as.numeric(caloriesum),
           healthyfoodsum = as.numeric(healthyfoodsum),
           fastfoodsum = as.numeric(fastfoodsum),
           eatoutsum = as.numeric(eatoutsum),
           following = as.numeric(following),
           followers = as.numeric(followers) ) %>%
    # remove outliers that do not make sense
    mutate(invalid_foodterm = is.na(healthyfoodsum) | is.na(fastfoodsum) | is.na(eatoutsum))
    
  
  return(df_out)
}


# -------------------------------------------------------------------------

create_timeseries <- function(df) {
  df_out <- df %>%
    group_by(timestamp) %>%
    count() %>%
    ungroup() %>%
    complete(timestamp = seq.Date(min(timestamp), max(timestamp), by="day") ) %>%
    mutate(n = replace_na(n, 0),
           n_mean = rollmean(n, k = 7, na.pad = TRUE) )
  
  return(df_out)
}


# -------------------------------------------------------------------------


# get food term counts from three separate food term variables
get_food_term_counts <- function(df){
  
  #  aggregating counts for each foodterm variable
  df_freq_1 <- df %>%
    group_by(foodterm1) %>%
    count()
  df_freq_2 <- df %>%
    group_by(foodterm2) %>%
    count()
  df_freq_3 <- df %>%
    group_by(foodterm3) %>%
    count()
  
  # join all food term tables
  # add across the three variables for each food term
  df_freq <- df_freq_1 %>%
    full_join(df_freq_2, by = c("foodterm1" = "foodterm2") ) %>%
    full_join(df_freq_3, by = c("foodterm1" = "foodterm3")  ) %>%
    mutate(n.x = replace_na(n.x, 0),
           n.y = replace_na(n.y, 0),
           n = replace_na(n, 0)) %>%
    rowwise() %>%
    mutate(freq = n.x + n.y + n) %>%
    rename(foodterm = foodterm1) %>%
    select(foodterm, freq) %>%
    filter(foodterm != "") %>%
    arrange(desc(freq)) %>%
    mutate( foodterm = factor(foodterm, levels=unique(foodterm)) ) 
  
  return(df_freq)
}



get_summ_stats <- function(df_in){
  # calculate counts and proportions of food category references
  
  
  df_out <- df_in %>%
    summarize( N = n(),
               n_healthy = sum(healthyfoodsum > 0),
               n_fastfood = sum(fastfoodsum > 0),
               n_eatout = sum(eatoutsum > 0),
               n_alcohol = sum(alcohol > 0),
               p_healthy = n_healthy / N * 100,
               p_fastfood = n_fastfood / N * 100,
               p_eatout = n_eatout / N * 100,
               p_alcohol = n_alcohol / N * 100) %>%
    arrange( desc(N))
  
  return(df_out)
}

get_summ_diff <- function(df_in) {
  # calculate differences in food category references between before/during covid
  
  stat_var <- c("N", "n_healthy", "n_fastfood", "n_eatout", "n_alcohol",
                "p_healthy", "p_fastfood", "p_eatout", "p_alcohol")
  
  sd_breaks <- c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf)
  sd_labels <- c("\u2264 -1.5 SD", "(-1.5 SD, -0.5 SD]", "(-0.5 SD, 0.5 SD]", "(0.5 SD, 1.5 SD]", "> 1.5 SD")
    
  df_out <- df_in %>%
    pivot_wider(names_from = "period_covid", values_from = stat_var) %>%
    mutate( p_healthy_diff_r = p_healthy_during / p_healthy_before,
            p_healthy_diff_a = (p_healthy_during - p_healthy_before),
            p_healthy_diff_sd = (p_healthy_during - p_healthy_before) / sd(.$p_healthy_before, na.rm = TRUE),
            p_healthy_diff_sd_lab = cut(p_healthy_diff_sd, breaks = sd_breaks, labels = sd_labels),
            p_healthy_p_change = (p_healthy_diff_r - 1) * 100,
            
            p_fastfood_diff_r = p_fastfood_during / p_fastfood_before,
            p_fastfood_diff_a = (p_fastfood_during - p_fastfood_before),
            p_fastfood_diff_sd = (p_fastfood_during - p_fastfood_before) / sd(.$p_fastfood_before, na.rm = TRUE),
            p_fastfood_diff_sd_lab = cut(p_fastfood_diff_sd, breaks = sd_breaks, labels = sd_labels),
            p_fastfood_p_change = (p_fastfood_diff_r - 1) * 100,
            
            p_eatout_diff_r = p_eatout_during / p_eatout_before,
            p_eatout_diff_a = (p_eatout_during - p_eatout_before),
            p_eatout_diff_sd = (p_eatout_during - p_eatout_before) / sd(.$p_eatout_before, na.rm = TRUE),
            p_eatout_diff_sd_lab = cut(p_eatout_diff_sd, breaks = sd_breaks, labels = sd_labels),
            p_eatout_p_change = (p_eatout_diff_r - 1) * 100,
            
            p_alcohol_diff_r = p_alcohol_during / p_alcohol_before,
            p_alcohol_diff_a = (p_alcohol_during - p_alcohol_before),
            p_alcohol_diff_sd = (p_alcohol_during - p_alcohol_before) / sd(.$p_alcohol_before, na.rm = TRUE),
            p_alcohol_diff_sd_lab = cut(p_alcohol_diff_sd, breaks = sd_breaks, labels = sd_labels),
            p_alcohol_p_change = (p_alcohol_diff_r - 1) * 100)  %>%
    mutate(N = N_before + N_during) 
  
  return(df_out)
}

refactorSdLabels <- function(df){
  
  varNames <- names(df)
  
  sd_labels <- c("\u2264 -1.5 SD", "(-1.5 SD, -0.5 SD]", "(-0.5 SD, 0.5 SD]", "(0.5 SD, 1.5 SD]", "> 1.5 SD")
  
  if("p_healthy_diff_sd_lab" %in% varNames){
    df$p_healthy_diff_sd_lab <- factor(df$p_healthy_diff_sd_lab, levels = sd_labels)
  }
  
  if("p_fastfood_diff_sd_lab" %in% varNames){
    df$p_fastfood_diff_sd_lab <- factor(df$p_fastfood_diff_sd_lab, levels = sd_labels)
  }
  
  if("p_alcohol_diff_sd_lab" %in% varNames){
    df$p_alcohol_diff_sd_lab <- factor(df$p_alcohol_diff_sd_lab, levels = sd_labels)
  }
  
  if("p_eatout_diff_sd_lab" %in% varNames){
    df$p_eatout_diff_sd_lab <- factor(df$p_eatout_diff_sd_lab, levels = sd_labels)
  }
  
  return(df)

}


# -------------------------------------------------------------------------
# plot tweet trend hex state maps
# -------------------------------------------------------------------------
plotHexStateMap <- function(df_shp, outcomeStr, id_label = "StateAbbr") {
  
  
  spec_pal <- rev( brewer.pal(name="RdYlBu",n=5) )
  values_fac <- levels( df_shp[[outcomeStr]] )
  
  # color_pal = c(values_fac[1] = spec_pal[1], 
  #               values_fac[2] = spec_pal[2], 
  #               values_fac[3] = spec_pal[3], 
  #               values_fac[4] = spec_pal[4], 
  #               values_fac[5] = spec_pal[5] )
  
  color_pal = c("\u2264 -1.5 SD" = spec_pal[1], 
                "(-1.5 SD, -0.5 SD]" = spec_pal[2], 
                "(-0.5 SD, 0.5 SD]" = spec_pal[3], 
                "(0.5 SD, 1.5 SD]" = spec_pal[4], 
                "> 1.5 SD" = spec_pal[5] )
  
  p1 <- df_shp %>%
    ggplot() + 
    geom_sf(aes(fill = !!sym(outcomeStr) )) +
    # geom_sf(aes(fill = cut_number(!!sym(outcomeStr), n = 5) ) ) +
    geom_sf_text(aes(label = !!sym(id_label)), size = 3) +
    theme_void() +
    theme( plot.background = element_rect(fill = "#f5f5f2", color = NA) )  +
    scale_fill_manual(values = color_pal, drop = FALSE)
  # coord_sf(datum=st_crs("mercator"))
  
  return(p1)
  
}


plotCountyMap <- function(df_shp, outcomeStr) {
  
  
  spec_pal <- rev( brewer.pal(name="RdYlBu",n=5) )
  color_pal = c("\u2264 -1.5 SD" = spec_pal[1], 
                "(-1.5 SD, -0.5 SD]" = spec_pal[2], 
                "(-0.5 SD, 0.5 SD]" = spec_pal[3], 
                "(0.5 SD, 1.5 SD]" = spec_pal[4], 
                "> 1.5 SD" = spec_pal[5] )
  
  p1 <- plot_usmap(region = "counties", data = df_county, values = outcomeStr, 
             size = .25, col = "gray") +
    scale_fill_manual(values = color_pal, drop = FALSE) +
    theme(legend.position = "right")
  
  return(p1)
  
}


# -------------------------------------------------------------------------
# import ACS data
# -------------------------------------------------------------------------

getAcsClean <- function(geography_in = "county",
                        year_in = 2019) 
{
  
  acs_var_codes <- c("B03002_001",
                     "B03002_003",
                     "B03002_004",
                     "B03002_012",
                     "B19001_001",
                     "B19001_002",
                     "B19001_003",
                     "B19001_004",
                     "B19001_005",
                     "B19001_014",
                     "B19001_015",
                     "B19001_016",
                     "B19001_017",
                     "B01001_001",
                     "B01001_005",
                     "B01001_006",
                     "B01001_007",
                     "B01001_008",
                     "B01001_009",
                     "B01001_010",
                     "B01001_029",
                     "B01001_030",
                     "B01001_031",
                     "B01001_032",
                     "B01001_033",
                     "B01001_034",
                     "B01001_020",
                     "B01001_021",
                     "B01001_022",
                     "B01001_023",
                     "B01001_024",
                     "B01001_025",
                     "B01001_044",
                     "B01001_045",
                     "B01001_046",
                     "B01001_047",
                     "B01001_048",
                     "B01001_049"
                     )
  acs_var_names <- c("POP_r",
                     "NHWht",
                     "NHBlk",
                     "Hisp",
                     "POP_i",
                     "INC_lo1",
                     "INC_lo2",
                     "INC_lo3",
                     "INC_lo4",
                     "INC_hi1",
                     "INC_hi2",
                     "INC_hi3",
                     "INC_hi4",
                     "POP_agebysex",
                     "m_young_1",
                     "m_young_2",
                     "m_young_3",
                     "m_young_4",
                     "m_young_5",
                     "m_young_6",
                     "f_young_1",
                     "f_young_2",
                     "f_young_3",
                     "f_young_4",
                     "f_young_5",
                     "f_young_6",
                     "m_old_1",
                     "m_old_2",
                     "m_old_3",
                     "m_old_4",
                     "m_old_5",
                     "m_old_6",
                     "f_old_1",
                     "f_old_2",
                     "f_old_3",
                     "f_old_4",
                     "f_old_5",
                     "f_old_6")
  
  
  # import acs data with variables
  df_long <- get_acs(geography = geography_in,
                     variables = acs_var_codes,
                     year = year_in)
  # df_long <- get_acs(geography = "county",
  #                    variables = acs_var_codes,
  #                    year = 2019)
  
  
  df_wide <- df_long %>%
    # short name
    rename(E = estimate, M = moe)  %>%
    mutate(variable = factor(variable, 
                             levels = acs_var_codes,
                             labels = acs_var_names ))  %>%
    # labels = paste0(acs_var_names, year_in) )) %>%
    # turn to wider table
    pivot_wider(names_from = variable, values_from = c("E", "M") ) %>%
    rename(YEAR = NAME) %>%
    mutate(YEAR = year_in) %>%
    rename("FIPS" = "GEOID")
  
  return(df_wide)
}


calc_ICE_metrics <- function(df){
  df_out <- df %>%
    # calculate segregation metrics
    mutate(E_old = E_m_old_1 + E_m_old_2 + E_m_old_3 + E_m_old_4 + E_m_old_5 + E_m_old_6 +
             E_f_old_1 + E_f_old_2 + E_f_old_3 + E_f_old_4 + E_f_old_5 + E_f_old_6 ,
           E_young = E_m_young_1 + E_m_young_2 + E_m_young_3 + E_m_young_4 + E_m_young_5 + E_m_young_6 +
             E_f_young_1 + E_f_young_2 + E_f_young_3 + E_f_young_4 + E_f_young_5 + E_f_young_6 ,
           pctOld = E_old / E_POP_agebysex * 100 ,
           pctYoung = E_young / E_POP_agebysex * 100,
           E_INC_lo = E_INC_lo1 + E_INC_lo2 + E_INC_lo3 + E_INC_lo4,
           E_INC_hi = E_INC_hi1 + E_INC_hi2 + E_INC_hi3 + E_INC_hi4,
           pctLowIncome = E_INC_lo / E_POP_i * 100 ,
           pctHighIncome = E_INC_hi / E_POP_i * 100,
           pctBlack = E_NHBlk / E_POP_r * 100,
           pctWhite = E_NHWht / E_POP_r * 100,
           pctHisp = E_Hisp / E_POP_r * 100,
           ICE_I =  (E_INC_hi - E_INC_lo) / E_POP_i ,
           ICE_B = (E_NHWht - E_NHBlk) / E_POP_r,
           ICE_H = (E_NHWht - E_Hisp) / E_POP_r,
           ICE_BH = (E_NHWht - (E_Hisp+E_NHBlk) ) / E_POP_r)
  
  return(df_out)
}

# -------------------------------------------------------------------------
# plot state clusters
# -------------------------------------------------------------------------

getClusters <- function(df, var1str, var2str, n_clusters = 4){
  
  # standardize
  # df <- df %>%
  # mutate(
  # clustvar1 = !!sym(var1str) / sd(!!sym(var1str)),
  # clustvar2 = !!sym(var2str) 
  # clustvar2 = !!sym(var2str) / sd(!!sym(var2str))
  # )
  # df[,var1str] <- df[,var1str] / sd (df$var1str)
  # df[,var2str] <- df[,var2str] / sd (df$var2str)
  
  # clustering
  mdl_k <- kmeans(df[, c(var1str, var2str)], n_clusters)
  # mdl_k <- kmeans(df[, c("clustvar1", "clustvar2")], n_clusters)
  
  # add cluster label to data frame
  df_out <- df %>% 
    mutate(CLUSTER = factor(mdl_k$cluster,
                            levels = 1:n_clusters) )
  # mutate(CLUSTER = mdl_k$cluster)
  
  return(df_out)
}


plotStateClusters <- function(df, var1str, var2str, n_clusters = 4){
  
  # cluster fast food - baseline / change
  df %>%
    getClusters(var1str, var1str, n_clusters)  %>%
    ggplot() +
    geom_text(aes(x = !!sym(var1str), y = !!sym(var2str), label = StateAbbr, col = CLUSTER) , size = 4) +
    theme(text = element_text(size = 18))
}




# -------------------------------------------------------------------------
# CUSP dataset processing
# -------------------------------------------------------------------------



# function for calculating days closed
calcDaysClosed <- function(CLOSE_DATE, 
                           REOPEN_DATE,
                           # startDate = as.Date("2020-05-15"), 
                           startDate = as.Date("2020-03-14"), 
                           endDate = as.Date("2021-01-31")){
  
  
  if (is.na(CLOSE_DATE) ){
    date_f <- 0
    date_0 <- 0
    
  } else {
    
    date_0 <- max(c(CLOSE_DATE, startDate) )
    if(is.na(REOPEN_DATE)){
      date_f <- endDate
    } else {
      date_f <-min(c(REOPEN_DATE, endDate) )
    }
    
  }
  
  days_closed <- as.numeric(date_f - date_0)
  days_closed <- ifelse(days_closed < 0, 0, days_closed)
  
  return(days_closed)
  
}



# -------------------------------------------------------------------------
# regression models
# -------------------------------------------------------------------------



# convert model output to formatted data frame
gen_mdl_df <- function(mdl, round_dig = 3){
  # add p-value
  pvalue<- round(  summary(mdl)$coefficients[,4] , digits = 4)
  significant <- pvalue < 0.05
  
  # add confidence intervals and create data frame
  # df<-bind_cols( round (  exp(bind_cols(OR = coef(mdl), confint(mdl) ) )  , digits = round_dig),
  df<-bind_cols( exp(bind_cols(OR = coef(mdl), confint_tidy(mdl, func = stats::confint.default) ) )  ,
                 
                 data.frame(pvalue, significant )   )
  
  
  df[ "Variable" ] <- rownames(df)
  rownames(df)=NULL
  colnames(df) <- c("OR", "CI_l", "CI_u", "pvalue", "significant", "Variable")
  
  
  df <- df %>%
    # calculate probabilities
    mutate(OR_prob = OR / (1 + OR) * 100 - 50,
           CI_l_prob = CI_l / (1 + CI_l) * 100 - 50,
           CI_u_prob = CI_u / (1 + CI_u) * 100 - 50) %>%
    # string for p-value
    mutate(  pvalue = round(pvalue, digits = 3), 
             p_str = format(pvalue, nsmall = 3),
             p_str = ifelse(pvalue >= 0.05, paste0(p_str, "") , 
                            ifelse(pvalue >= 0.01, paste0( p_str, ""),
                                   ifelse(pvalue >= 0.001, paste0( p_str, ""), "< 0.001" ) ) ) )  %>%
    # string for OR and CI
    mutate(OR_CI_str = paste0(format(round(OR, digits = round_dig), nsmall = round_dig), " (", 
                              format(round(CI_l, digits = round_dig), nsmall = round_dig), ", ", 
                              format(round(CI_u, digits = round_dig), nsmall = round_dig), ")") ) %>%
    mutate(prob_CI_str = paste0(format(round(OR_prob, digits = 2), nsmall = 2), "% (", 
                                format(round(CI_l_prob, digits = 2), nsmall = 2), "%, ", 
                                format(round(CI_u_prob, digits = 2), nsmall = 2), "%)") ) %>%
    
    
    
    select( c("Variable", "OR_CI_str", "prob_CI_str", "p_str", "OR", "CI_l", "CI_u", "OR_prob", "CI_l_prob", "CI_u_prob",  "pvalue", "significant") )
  
  
  return(df)
}


# tion for displaying results
displayModelResults <- function(mdl, CALC_PROB = FALSE)
{
  
  if(is.data.frame(mdl) ){
    df <- mdl
  }
  else{
    df <- gen_mdl_df(mdl)
  }  
  
  
  
  df <- df %>%
    # mutate(Variable = factor(Variable,
                             # levels = df$Variable)) %>%
    filter(Variable != "(Intercept)") %>%
    mutate(Variable = factor(Variable, levels = rev(levels(df$Variable))),
           significant = factor(significant,
                                levels = c(TRUE, FALSE),
                                labels = c("p < 0.05", "p >= 0.05") ) ) 
  
  
  
  # if(!CALC_PROB){
    OR_sym <- sym("OR")
    CI_l_sym <- sym("CI_l")
    CI_u_sym <- sym("CI_u")
  # } else {
  #   OR_sym <- sym("OR_prob")
  #   CI_l_sym <- sym("CI_l_prob")
  #   CI_u_sym <- sym("CI_u_prob")
  # }
  
  
  
  p1 <- df %>%
    ggplot(aes(x = Index, y = !!OR_sym, ymin = !!CI_l_sym, ymax = !!CI_u_sym)) +

    # geom_pointrange( aes(col = significant), fatten = 2 ) + # fatten = 1, size = .75 # 
    geom_pointrange( col = "black", fatten = 2 ) + # fatten = 1, size = .75 # 
    theme_bw() +
    # scale_y_continuous(trans='log2', limits = c(.1, 35)) +

    coord_flip() +
    theme(legend.position = "top",
          legend.title = element_blank(),
          text = element_text(size = 14),
          plot.title = element_text(size = 14)) + 
    scale_color_brewer(palette = "Dark2") +
    xlab("") 
    # theme(axis.text.y = element_blank())
  
  
  # if(!CALC_PROB){
    p1 <- p1 + 
      scale_y_continuous(trans='log2',
                         breaks = seq(0.8, 1.2, by = .02))  +
      geom_hline(aes(yintercept = 1), col = "red", linetype = 'dashed') 
  # } else {
    # p1 <- p1 +     
      # geom_hline(aes(yintercept = 0), col = "red", linetype = 'dashed') 
  # }
  
  

  
  
  
  
  return(p1)
}


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------



