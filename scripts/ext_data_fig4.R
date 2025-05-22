### Extended data fig 4
library(ggplot2)     
library(dplyr)
library(tidyr)
library(broom)
library(patchwork)


# Read CMIP data
data_his <- read.csv("data/CMIP_historical.csv")
data_proj <- read.csv("data/CMIP_projection.csv")
data_obs <- read.csv("data/climate.csv")
data <- rbind(data_his, data_proj)
data <- na.omit(data)

data$tasmax_mean <- data$tasmax_mean - 273
data$tasmin_mean <- data$tasmin_mean - 273
data_obs$temperature_2m_max_mean <- data_obs$temperature_2m_max_mean - 273
data_obs$temperature_2m_min_mean <- data_obs$temperature_2m_min_mean - 273
data_obs$tmmn_mean <- data_obs$tmmn_mean * 0.1
data_obs$tmmx_mean <- data_obs$tmmx_mean * 0.1

data$alpha3[data$country_co == "MI"] <- 'MWI'
data$alpha3[data$country_co == "ZA"] <- 'ZMB'
data$alpha3[data$country_co == "MZ"] <- 'MOZ'
data$alpha3[data$country_co == "ZI"] <- 'ZWE'
data$alpha3[data$country_co == "SF"] <- 'ZAF'

data_obs$alpha3[data_obs$ADM0_NAME == "Malawi"] <- 'MWI'
data_obs$alpha3[data_obs$ADM0_NAME == "Zambia"] <- 'ZMB'
data_obs$alpha3[data_obs$ADM0_NAME == "Mozambique"] <- 'MOZ'
data_obs$alpha3[data_obs$ADM0_NAME == "Zimbabwe"] <- 'ZWE'
data_obs$alpha3[data_obs$ADM0_NAME == "South Africa"] <- 'ZAF'

# Function to calculate the number of seconds from Jan 1 to Mar 31
number_of_days <- function(year) {
  start_date <- ymd(paste(year, "-01-01", sep=""))
  end_date <- ymd(paste(year, "-03-31", sep=""))
  return(as.numeric(difftime(end_date, start_date, units = "days")) + 1) 
}

data <- data %>%
  mutate(days_in_period = sapply(year, number_of_days),
         daily_precip_mm = pr_mean * 86400, # Convert rate from kg/m²/s to mm/day
         total_precip_mm = daily_precip_mm * days_in_period)

filtered_df <- data %>% 
  filter(year >= 2003, year <= 2022)

# Specify the columns for which you want to calculate the trends
cols <- c("temperature_2m_mean", "temperature_2m_min_mean", "temperature_2m_max_mean", "precipitation_sum", "tmmn_mean", "tmmx_mean", "pr_sum")

# Calculate the trend for each specified column, by country
trends <- data_obs %>%
  group_by(alpha3) %>%
  summarize(across(all_of(cols), 
                   ~coef(lm(. ~ year, data = cur_data()))[["year"]],
                   .names = "slope_{.col}"),
            .groups = 'keep')  # Keeping the grouping structure
trends_long <- trends %>%
  pivot_longer(cols = starts_with("slope_"), 
               names_to = "variable", 
               values_to = "trend") %>%
  mutate(variable = sub("slope_", "", variable))

# Define a function to calculate the trend for a given variable
calculate_trend <- function(data, variable) {
  data %>%
    group_by(alpha3, model) %>%
    do(tidy(lm(reformulate("year", variable), data = .))) %>%
    filter(term == "year") %>%
    select(alpha3, estimate) %>%
    rename(trend = estimate)
}

trend_tas_mean <- calculate_trend(filtered_df, "tas_mean")
trend_tasmin_mean <- calculate_trend(filtered_df, "tasmin_mean")
trend_tasmax_mean <- calculate_trend(filtered_df, "tasmax_mean")
trend_pr_mean <- calculate_trend(filtered_df, "total_precip_mm")

# Precipitation
my_colors <- c(2, 1, 3, 4, 5)
trends_line_eral <- filter(trends_long, variable == "precipitation_sum")
trends_line_tc <- filter(trends_long, variable == "pr_sum")

group_stats_pr <- trend_pr_mean %>%
  group_by(alpha3) %>%
  summarize(min_trend = min(trend), max_trend = max(trend)) %>%
  ungroup()

box_plot_p <- ggplot(trend_pr_mean, aes(x=factor(alpha3, level=c('MWI', 'MOZ', 'ZAF', 'ZMB', 'ZWE')), y = trend, fill = alpha3), show.legend=FALSE) +
  geom_boxplot(outlier.shape = NA, ymin=group_stats_pr$min_trend, ymax=group_stats_pr$max_trend) +
  labs(title = "(a) prec",
       x = "",
       y = "Prec Trends (mm/year)",
       fill = "Country") +
  scale_fill_manual(values=c(my_colors)) + 
  geom_segment(data = trends_line_eral, 
               aes(x = as.numeric(factor(alpha3)) - 0.45, 
                   xend = as.numeric(factor(alpha3)) + 0.45, 
                   y = trend, yend = trend, 
                   linetype = "ERA5-Land"), 
               color = "red") + 
  geom_segment(data = trends_line_tc, 
               aes(x = as.numeric(factor(alpha3)) - 0.45, 
                   xend = as.numeric(factor(alpha3)) + 0.45, 
                   y = trend, yend = trend, 
                   linetype = "TerraClimate"), 
               color = "blue") +
  scale_linetype_manual(name = "Observed", 
                        values = c("ERA5-Land" = "dashed", "TerraClimate" = "dashed"),
                        guide = guide_legend(override.aes = list(color = c("red", "blue")))) +
  theme_classic() + 
  theme(legend.position = "none", plot.title=element_text(face="bold"))   

# Temperature max
trends_line_eral <- filter(trends_long, variable == "temperature_2m_max_mean")
trends_line_tc <- filter(trends_long, variable == "tmmx_mean")

group_stats_tx <- trend_tasmax_mean %>%
  group_by(alpha3) %>%
  summarize(min_trend = min(trend), max_trend = max(trend)) %>%
  ungroup()

box_plot_tx <- ggplot(trend_tasmax_mean, aes(x=factor(alpha3, level=c('MWI', 'MOZ', 'ZAF', 'ZMB', 'ZWE')), y = trend, fill = alpha3)) +
  geom_boxplot(outlier.shape = NA, ymin=group_stats_tx$min_trend, ymax=group_stats_tx$max_trend) +
  labs(title = "(c) tmax",
       x = "",
       y = "Tmax Trends (ºC/year)",
       fill = "Country") +
  scale_fill_manual(values=c(my_colors)) + 
  geom_segment(data = trends_line_eral, 
               aes(x = as.numeric(factor(alpha3)) - 0.45, 
                   xend = as.numeric(factor(alpha3)) + 0.45, 
                   y = trend, yend = trend, 
                   linetype = "ERA5-Land"), 
               color = "red") + 
  geom_segment(data = trends_line_tc, 
               aes(x = as.numeric(factor(alpha3)) - 0.45, 
                   xend = as.numeric(factor(alpha3)) + 0.45, 
                   y = trend, yend = trend, 
                   linetype = "TerraClimate"), 
               color = "blue") +
  scale_linetype_manual(name = "Observed", 
                        values = c("ERA5-Land" = "dashed", "TerraClimate" = "dashed"),
                        guide = guide_legend(override.aes = list(color = c("red", "blue")))) +
  guides(fill = FALSE) + 
  theme_classic() + theme(plot.title=element_text(face="bold"))

# Temperature min
trends_line_eral <- filter(trends_long, variable == "temperature_2m_min_mean")
trends_line_tc <- filter(trends_long, variable == "tmmn_mean")

group_stats_tn <- trend_tasmin_mean %>%
  group_by(alpha3) %>%
  summarize(min_trend = min(trend), max_trend = max(trend)) %>%
  ungroup()

box_plot_tn <- ggplot(trend_tasmin_mean, aes(x=factor(alpha3, level=c('MWI', 'MOZ', 'ZAF', 'ZMB', 'ZWE')), y = trend, fill = alpha3)) +
  geom_boxplot(outlier.shape = NA, ymin=group_stats_tn$min_trend, ymax=group_stats_tn$max_trend) +
  labs(title = "(b) tmin",
       x = "",
       y = "Tmin Trends (ºC/year)",
       fill = "Country") +
  scale_fill_manual(values=c(my_colors)) + 
  geom_segment(data = trends_line_eral, 
               aes(x = as.numeric(factor(alpha3)) - 0.45, 
                   xend = as.numeric(factor(alpha3)) + 0.45, 
                   y = trend, yend = trend, 
                   linetype = "ERA5-Land"), 
               color = "red") + 
  geom_segment(data = trends_line_tc, 
               aes(x = as.numeric(factor(alpha3)) - 0.45, 
                   xend = as.numeric(factor(alpha3)) + 0.45, 
                   y = trend, yend = trend, 
                   linetype = "TerraClimate"), 
               color = "blue")  +
  scale_linetype_manual(name = "Observed", 
                        values = c("ERA5-Land" = "dashed", "TerraClimate" = "dashed"),
                        guide = guide_legend(override.aes = list(color = c("red", "blue")))) +
  theme_classic() + 
  theme(legend.position = "none", plot.title=element_text(face="bold"))  

all_vars <- box_plot_p + box_plot_tn + box_plot_tx
