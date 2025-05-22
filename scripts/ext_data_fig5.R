### Extended data fig 5

data <- read.csv("data/ext_data_fig5/modis_jan_mar_03_22.csv")
fao_gaul <- read.csv("data/ext_data_fig5/gaul_adm1.csv")

data <- na.omit(data)
data <- merge(data, fao_gaul, by.x="ADM1", by.y="gaul_adm1", all.x=TRUE)

data$precipitation_sum_m <- data$precipitation_sum * 0.001
data$temperature_2m_max_mean <- data$temperature_2m_max_mean - 273
data$temperature_2m_min_mean <- data$temperature_2m_min_mean - 273

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

predictors <- c("tmmx", "tmmn", "precipitation_sum_m")
predictors <- paste(predictors, collapse = " + ")
model <- paste("GCVI ~", predictors, "| geom_id + geom_id[[year]]")
model_formula <- as.formula(model)

# Models for each country
unique_cname <- unique(data$country_name)
models_by_cname <- list()
model_summaries <- list()

for (cname in unique_cname) {
  data_subset <- subset(data, country_name == cname)
  names(data_subset)[names(data_subset) == "era5_land_max"] <- "tmmx"
  names(data_subset)[names(data_subset) == "era5_land_min"] <- "tmmn"
  model <- feols(model_formula, data = data_subset, cluster = "ADM1")
  models_by_cname[[as.character(cname)]] <- model
  
  # Extract and store the summary of each model
  model_summaries[[as.character(cname)]] <- summary(model)
}
par(las=1)
layout_matrix <- matrix(c(1, 1, 2), nrow = 1, byrow = TRUE)
layout(layout_matrix)
dict = c("tmmn"="tmin", "tmmx"="tmax", "precipitation_sum_m"="prec")
setFixest_coefplot(dict=dict)
coefplot(model_summaries, pt.pch = 20, keep=c("tmin","tmax"), value.lab = "", main="")
mtext(expression(paste("Temp Effect  (", Delta, " GCVI per °C)")), side = 2, line = 2.5, las = 0)
# Create a legend
my_colors <- 1:length(unique_cname)  
# my_colors <- c(1, 2, 3, 4, 5)
my_point_types <- rep(20, length(unique_cname)) 
legend("topleft", inset=c(0.0,0), xpd=FALSE,
       col = my_colors,
       pch = my_point_types,
       legend = as.character(unique_cname),)


coefplot(model_summaries, pt.pch = 20, keep="prec", value.lab="", main="")
mtext(expression(paste("Prec Effect  (", Delta, " GCVI per m)")), side = 2, line = 2.5, las = 0)