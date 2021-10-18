## https://www.r-bloggers.com/2018/04/time-series-deep-learning-forecasting-sunspots-with-keras-stateful-lstm-in-r/

## https://blogs.rstudio.com/ai/posts/2021-03-10-forecasting-time-series-with-torch_1/

## https://blogs.rstudio.com/ai/posts/2017-12-20-time-series-forecasting-with-recurrent-neural-networks/

## https://blog.paperspace.com/time-series-forecasting-regression-and-lstm/

## https://algotech.netlify.app/blog/time-series-prediction-with-lstm/


setwd("E:/school/data mining/project/mimic-iii-clinical-database-1.4/mimic-iii-clinical-database-1.4/codes")









source("packagesUsed.R")
source("code1.R")
data$decodedAdminDate<-as.Date.POSIXct(data$decodedAdminDate)
data$index<-data$decodedAdminDate


df2 <- data %>%                              
  group_by(decodedAdminDate) %>%
  summarise(value = n_distinct(HADM_ID),patients=n_distinct(HADM_ID)) %>% 
  mutate(index = decodedAdminDate) 



df2<-df2 %>% column_to_rownames(., var = "decodedAdminDate")
df2<-tibble::as_tibble(df2)


library(tidyverse)
library(timetk)


spots <- df2 %>%
  dplyr::select(index,value) %>% 
  tk_tbl(timetk_idx = TRUE) %>% 
  as_tbl_time(index = index)



## Visualize


p1 <- spots %>%
  ggplot(aes(index, value)) +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  theme_tq() +
  labs(
    title = "Summary of daily admissions (Full Data Set)"
  )
p2 <- spots %>%
  filter_time("start" ~ "2012") %>%
  ggplot(aes(index, value)) +
  geom_line(color = palette_light()[[1]], alpha = 0.5) +
  geom_point(color = palette_light()[[1]]) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  theme_tq() +
  labs(
    title = "Zoomed in to see the Visualizing Cycle)",
    caption = "datasets::MIMIC daily admissions"
  )
p_title <- ggdraw() + 
  draw_label("Admissions", size = 18, fontface = "bold", colour = palette_light()[[1]])
plot_grid(p_title, p1, p2, ncol = 1, rel_heights = c(0.1, 1, 1))



## Evaluate ACF

tidy_acf <- function(data, value, lags = 0:20) {
  value_expr <- enquo(value)
  acf_values <- data %>%
    pull(value) %>%
    acf(lag.max = tail(lags, 1), plot = FALSE) %>%
    .$acf %>%
    .[,,1]
  
  ret <- tibble(acf = acf_values) %>%
    rowid_to_column(var = "lag") %>%
    mutate(lag = lag - 1) %>%
    filter(lag %in% lags)
  return(ret)
}


max_lag <- 12 * 50
spots %>%
  tidy_acf(value, lags = 0:max_lag)


spots %>%
  tidy_acf(value, lags = 0:max_lag) %>%
  ggplot(aes(lag, acf)) +
  geom_segment(aes(xend = lag, yend = 0), color = palette_light()[[1]]) +
  geom_vline(xintercept = 120, size = 3, color = palette_light()[[2]]) +
  annotate("text", label = "10 Year Mark", x = 130, y = 0.8, 
           color = palette_light()[[2]], size = 6, hjust = 0) +
  theme_tq() +
  labs(title = "ACF: Admissions")


spots %>%
  tidy_acf(value, lags = 115:135) %>%
  ggplot(aes(lag, acf)) +
  geom_vline(xintercept = 120, size = 3, color = palette_light()[[2]]) +
  geom_segment(aes(xend = lag, yend = 0), color = palette_light()[[1]]) +
  geom_point(color = palette_light()[[1]], size = 2) +
  geom_label(aes(label = acf %>% round(2)), vjust = -1,
             color = palette_light()[[1]]) +
  annotate("text", label = "10 Year Mark", x = 121, y = 0.8, 
           color = palette_light()[[2]], size = 5, hjust = 0) +
  theme_tq() +
  labs(title = "ACF: Admissions",
       subtitle = "Zoomed in on Lags 115 to 135")


optimal_lag_setting <- spots %>%
  tidy_acf(value, lags = 115:135) %>%
  filter(acf == max(acf)) %>%
  pull(lag)
optimal_lag_setting


periods_train <- 12 * 50
periods_test  <- 12 * 10
skip_span     <- 12 * 20
rolling_origin_resamples <- rolling_origin(
  spots,
  initial    = periods_train,
  assess     = periods_test,
  cumulative = FALSE,
  skip       = skip_span
)
rolling_origin_resamples



# Plotting function for a single split
plot_split <- function(split, expand_y_axis = TRUE, alpha = 1, size = 1, base_size = 14) {
  # Manipulate data
  train_tbl <- training(split) %>%
    add_column(key = "training") 
  test_tbl  <- testing(split) %>%
    add_column(key = "testing") 
  data_manipulated <- bind_rows(train_tbl, test_tbl) %>%
    as_tbl_time(index = index) %>%
    mutate(key = fct_relevel(key, "training", "testing"))
 
  # Collect attributes
  train_time_summary <- train_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  test_time_summary <- test_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  # Visualize
  g <- data_manipulated %>%
    ggplot(aes(x = index, y = value, color = key)) +
    geom_line(size = size, alpha = alpha) +
    theme_tq(base_size = base_size) +
    scale_color_tq() +
    labs(
      title    = glue("Split: {split$id}"),
      subtitle = glue("{train_time_summary$start} to {test_time_summary$end}"),
      y = "", x = ""
    ) +
    theme(legend.position = "none") 
  
  if (expand_y_axis) {
    
    sun_spots_time_summary <- sun_spots %>% 
      tk_index() %>% 
      tk_get_timeseries_summary()
    
    g <- g +
      scale_x_date(limits = c(sun_spots_time_summary$start, 
                              sun_spots_time_summary$end))
  }
  
  return(g)
}



rolling_origin_resamples$splits[[1]] %>%
  plot_split(expand_y_axis = TRUE) +
  theme(legend.position = "bottom")


# Plotting function that scales to all splits 
plot_sampling_plan <- function(sampling_tbl, expand_y_axis = TRUE, 
                               ncol = 3, alpha = 1, size = 1, base_size = 14, 
                               title = "Sampling Plan") {
  
  # Map plot_split() to sampling_tbl
  sampling_tbl_with_plots <- sampling_tbl %>%
    mutate(gg_plots = map(splits, plot_split, 
                          expand_y_axis = expand_y_axis,
                          alpha = alpha, base_size = base_size))
  
  # Make plots with cowplot
  plot_list <- sampling_tbl_with_plots$gg_plots 
  
  p_temp <- plot_list[[1]] + theme(legend.position = "bottom")
  legend <- get_legend(p_temp)
  
  p_body  <- plot_grid(plotlist = plot_list, ncol = ncol)
  
  p_title <- ggdraw() + 
    draw_label(title, size = 18, fontface = "bold", colour = palette_light()[[1]])
  
  g <- plot_grid(p_title, p_body, legend, ncol = 1, rel_heights = c(0.05, 1, 0.05))
  
  return(g)
  
}

rolling_origin_resamples %>%
  plot_sampling_plan(expand_y_axis = T, ncol = 3, alpha = 1, size = 1, base_size = 10, 
                     title = "Backtesting Strategy: Rolling Origin Sampling Plan")


split    <- rolling_origin_resamples$splits[[11]]
split_id <- rolling_origin_resamples$id[[11]]

plot_split(split, expand_y_axis = FALSE, size = 0.5) +
  theme(legend.position = "bottom") +
  ggtitle(glue("Split: {split_id}"))