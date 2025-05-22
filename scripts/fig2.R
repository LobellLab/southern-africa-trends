### Fig 2
# a. Distribution of climate trend impacts within each country
# b. Distribution of productivity trends in each country

data <- read.csv("data/fig2/modis_jan_mar_03_22.csv")
fao_gaul <- read.csv("data/fig2/gaul_adm1.csv")

data <- na.omit(data)
data <- merge(data, fao_gaul, by.x="ADM1", by.y="gaul_adm1", all.x=TRUE)
data <- data[order(data$geom_id, data$year),]

# Convert adm codes to country names
data$country_name[data$ADM0 == 152] <- 'Malawi'
data$country_name[data$ADM0 == 170] <- 'Mozambique'
data$country_name[data$ADM0 == 227] <- 'South Africa'
data$country_name[data$ADM0 == 270] <- 'Zambia'
data$country_name[data$ADM0 == 271] <- 'Zimbabwe'

# Rename temp variables
names(data)[names(data) == "temperature"] <- "era5"
names(data)[names(data) == "temperature_2m_mean"] <- "era5_land"
names(data)[names(data) == "temperature_2m_max_mean"] <- "era5_land_max"
names(data)[names(data) == "temperature_2m_min_mean"] <- "era5_land_min"
names(data)[names(data) == "tmmx_mean"] <- "tc_max"
names(data)[names(data) == "tmmn_mean"] <- "tc_min"

## 2a.
# Make slopes dataframe
cols <- c("GCVI", "precipitation_sum", "era5_land_min", "era5_land_max")

slopes <- data %>%
  group_by(geom_id, country_name) %>%
  summarize(across(all_of(cols), ~coef(lm(. ~ year, data = cur_data()))[2], .names = "slope_{.col}"),
            .groups = 'keep')

# Create model formula
predictors <- c("era5_land_min", "era5_land_max", "precipitation_sum")
predictors_str <- paste(predictors, collapse=" + ")
model_formula_str <- paste("GCVI ~", predictors_str, "| geom_id + geom_id[[year]]")
model_formula <- as.formula(model_formula_str)

# Models for each country
unique_cname <- unique(data$country_name)
models_by_cname <- list()
model_summaries <- list()

# Extract and store summary of each model
for (cname in unique_cname) {
  data_subset <- subset(data, country_name == cname)
  model <- feols(model_formula, data = data_subset)
  models_by_cname[[as.character(cname)]] <- model
  
  model_summaries[[as.character(cname)]] <- summary(model)
}

# Extract coefficients per country
coefficients_list <- list()
for (cname in names(model_summaries)) {
  model_coeffs <- coef(model_summaries[[cname]])
  coefficients_list[[cname]] <- model_coeffs
}

# Initialize an empty dataframe
coefficients_df <- data.frame(
  Country = character(),
  precipitation_coef = numeric(),
  era5_land_min_coef = numeric(),
  era5_land_max_coef = numeric(),
  stringsAsFactors = FALSE  # To prevent factors
)

# Create a coefficients dataframe 
for (cname in names(coefficients_list)) {
  current_coeffs <- coefficients_list[[cname]]
  coefficients_df <- rbind(coefficients_df, data.frame(
    country_name = cname,
    precipitation_coef = current_coeffs["precipitation_sum"],
    era5_land_min_coef = current_coeffs["era5_land_min"],
    era5_land_max_coef = current_coeffs["era5_land_max"]
  ))
}
rownames(coefficients_df) <- NULL
# Add coefficients to data df
slopes <- merge(slopes, coefficients_df, by = "country_name")

slopes$precipitation_effect <- slopes$slope_precipitation * slopes$precipitation_coef
slopes$tx_effect <- slopes$slope_era5_land_max * slopes$era5_land_max_coef
slopes$tn_effect <- slopes$slope_era5_land_min * slopes$era5_land_min_coef
slopes$precipitation_t_effect <- slopes$precipitation_effect + slopes$tx_effect + slopes$tn_effect
slopes$t_sum_effect <- slopes$tx_effect + slopes$tn_effect

slopes_long_comb <- slopes %>%
  pivot_longer(
    cols = c(precipitation_effect, t_sum_effect),
    names_to = "variable",
    values_to = "effect"
  )

# Calculate % of pixels with positive vs. negative climate effects
total_rows <- nrow(slopes)
positive <- sum(slopes$precipitation_t_effect > 0)
negative <- sum(slopes$precipitation_t_effect < 0)
positive_per <- (positive / total_rows) * 100
negative_per <- (negative / total_rows) * 100

## 2b.
get_model_stats <- function(data, response_var) {
  model <- lm(as.formula(paste(response_var, "~ year")), data = data)
  coefficients <- summary(model)$coefficients
  data.frame(
    slope = coefficients['year', 'Estimate'],
    p_value = coefficients['year', 'Pr(>|t|)']
  )
}

# Calculate residuals for GCVI
data_res <- data
data_res$year <- 2003
model <- feols(GCVI ~ precipitation_sum + era5_land_min + era5_land_max | geom_id + geom_id[[year]], data = data_res)

data_res$GCVI_pred <- fitted(model)
data$GCVI_res <- resid(model)

results_old <- data %>%
  group_by(geom_id) %>%
  do({
    gcvi_stats <- get_model_stats(., "GCVI")
    gcvi_res_stats <- get_model_stats(., "GCVI_res")
    data.frame(
      geom_id = unique(.$geom_id),
      country_name = unique(.$country_name),
      slope_GCVI = gcvi_stats$slope,
      p_value_GCVI = gcvi_stats$p_value,
      slope_GCVI_res = gcvi_res_stats$slope,
      p_value_GCVI_res = gcvi_res_stats$p_value
    )
  })

# Determine slope categories
alpha <- 0.05
results <- results_old %>%
  mutate(category_GCVI = case_when(
    p_value_GCVI < alpha & slope_GCVI > 0 ~ "Significant Positive",
    p_value_GCVI < alpha & slope_GCVI < 0 ~ "Significant Negative",
    TRUE ~ "Not Significant"),
    category_GCVI_res = case_when(
      p_value_GCVI_res < alpha & slope_GCVI_res > 0 ~ "Significant Positive",
      p_value_GCVI_res < alpha & slope_GCVI_res < 0 ~ "Significant Negative",
      TRUE ~ "Not Significant"))

# Calculate percentages for each country
percentages_country <- results %>%
  pivot_longer(cols = c(category_GCVI, category_GCVI_res), names_to = "variable", values_to = "category") %>%
  group_by(country_name, variable, category) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(country_name, variable) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup() %>%
  pivot_wider(names_from = category, values_from = percentage, values_fill = list(percentage = 0))

long_data <- percentages_country %>%
  pivot_longer(cols = c(`Significant Positive`, `Significant Negative`, `Not Significant`), 
               names_to = "significance", values_to = "percentage")

long_data$significance <- factor(long_data$significance, 
                                 levels = c("Significant Negative", "Not Significant", "Significant Positive"))

# Custom labeller function
my_labeller <- as_labeller(c(category_GCVI = "GCVI", category_GCVI_res = "Climate Adjusted GCVI"))

# Prepare annotation stats
percentages <- slopes %>%
  group_by(country_name) %>%
  summarise(
    ltz = mean(precipitation_t_effect < 0) * 100,
    gtz = mean(precipitation_t_effect > 0) * 100
  ) %>%
  ungroup()

percentages <- slopes %>%
  group_by(country_name) %>%
  summarise(
    min_effect = min(precipitation_t_effect) - 0.001,
    max_effect = max(precipitation_t_effect) + 0.001,
    ltz_text = sprintf("%.0f%%", mean(precipitation_t_effect < 0) * 100),
    gtz_text = sprintf("%.0f%%", mean(precipitation_t_effect > 0) * 100)
  ) %>%
  ungroup()


# Ridge plot
both_plot <- ggplot(slopes, aes(x = precipitation_t_effect, y = factor(country_name, levels = c('Zimbabwe', 'Zambia', 'South Africa', 'Mozambique', 'Malawi')), fill = after_stat(x))) + 
  geom_density_ridges_gradient(scale = 1, rel_min_height = 0.01) + 
  labs(title = 'a', x = 'Climate Effect on GCVI', y = '% of Pixels') + 
  scale_fill_gradient2(name = 'precipitation', low = '#9e4623', mid = 'white', high = '#0c620e', midpoint = 0) + 
  theme_minimal() + 
  theme(panel.spacing = unit(0.1, "lines"), legend.title = element_blank(), plot.title = element_text(size=24,face="bold"))

# Annotate less than 0 percentages at min effect, adjust vjust slightly negative to push text up
both_plot <- both_plot + 
  geom_text(data=percentages, 
            aes(y = factor(country_name, levels = c('Zimbabwe', 'Zambia', 'South Africa', 'Mozambique', 'Malawi')), 
                x = -0.01, label=ltz_text), 
            hjust=1, vjust=-5, size=3, color="black")

# Annotate greater than 0 percentages at max effect, adjust vjust slightly negative to push text up
both_plot <- both_plot + 
  geom_text(data=percentages, 
            aes(y = factor(country_name, levels = c('Zimbabwe', 'Zambia', 'South Africa', 'Mozambique', 'Malawi')), 
                x = 0.01, label=gtz_text), 
            hjust=0, vjust=-5, size=3, color="black")




# Signifiance plot
long_data$alpha3[long_data$country_name == "Malawi"] <- 'MWI'
long_data$alpha3[long_data$country_name == "Zambia"] <- 'ZMB'
long_data$alpha3[long_data$country_name == "Mozambique"] <- 'MOZ'
long_data$alpha3[long_data$country_name == "Zimbabwe"] <- 'ZWE'
long_data$alpha3[long_data$country_name == "South Africa"] <- 'ZAF'

sig <- ggplot(long_data, aes(x = factor(alpha3, level=c('MWI', 'MOZ', 'ZAF', 'ZMB', 'ZWE')), y = percentage, fill = significance)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~variable, labeller = my_labeller) + 
  labs(title="b", x = "", y = "Percentage (%)") +
  scale_fill_manual(values = c("Significant Positive" = "blue", 
                               "Significant Negative" = "red", 
                               "Not Significant" = "lightgrey")) +
  theme_minimal() + 
  theme(plot.title=element_text(size=24, face="bold"), legend.title= element_blank())

fig2 <- both_plot + sig

