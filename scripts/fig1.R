### Fig 1.
# a. Trends in peak growing season greenness from MODIS for cropland pixels in regions
# b. Aggregate time series for each country of MODIS greenness, SIF, and maize yields from FAO

library(ggplot2)
library(dplyr)
library(tidyverse)
library(fixest)
library(patchwork)
library(broom)
library(ggridges)
library(hrbrthemes)
library(terra)
library(geodata)
library(RColorBrewer)
library(colormap)
library(sf)
library(rnaturalearth)
library(ggspatial)
library(cowplot)
library(grid)
library(gridExtra)
library(latex2exp)

## 1a
modis_2k <- rast("data/fig1/modis_trends_2km.tif")

# Plot raster
NAflag(modis_2k) <- -9999
gcviTrends <- plot(modis_2k,
                   box=FALSE,
                   # axes=FALSE,
                   ylim=c(-35, -8),
                   xlim=c(-40, 499),
                   range=c(-0.2, 0.2), type='continuous', col=hcl.colors(200, "RdYlGn"))

# Plot country boundaries
countries <- gadm(country = c("MWI", "MOZ", "ZAF", "ZMB", "ZWE"), level=0, path=tempdir())
countries <- plot(countries, add=TRUE)

countries_sf <- ne_countries(scale = "medium", returnclass = "sf")
countries_sf <- countries_sf %>% 
  filter(admin %in% c("Malawi", "Mozambique", "South Africa", "Zambia", "Zimbabwe"))

xlims <- c(-15, 43)
ylims <- c(-35, 0)
countries_sf <- st_crop(countries_sf, xmin=xlims[1], xmax=xlims[2], ymin=ylims[1], ymax=ylims[2])

df_modis_2k <- as.data.frame(modis_2k, xy = TRUE, na.rm = TRUE)

fig1a <- ggplot() +
  geom_tile(data = df_modis_2k, aes(x = x, y = y, fill = slope)) +
  geom_sf(data = countries_sf, color="black", fill=NA) +
  coord_sf() +
  scale_fill_gradientn(
    colors = hcl.colors(200, "RdYlGn"),
    limits = c(-0.2, 0.2),
    name = "GCVI Trends\n(2003-2022)" # Split the title into two lines
  ) +
  guides(fill = guide_colorbar(title.position = "top")) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.title.align = 0.5,
    legend.title = element_text(hjust = 0.5) # Adjust the horizontal alignment if needed
  )

## 1b
# Load the datasets
country_gcvi <- read.csv('data/fig1/CountryYearMeanGCVI.csv') %>% mutate(Year=year)
country_gcvi$Year <- country_gcvi$Year + 1

country_sif <- read.csv('data/fig1/sifMaxCountryMean.csv') %>% mutate(country_name=country_na, Year=year)
country_sif$.geo <- NULL
country_sif$Year <- country_sif$Year + 1

faostat_data <- read.csv('data/fig1/FAOSTAT_data_en_12-8-2023.csv') %>%
  filter(Element == "Yield", Item == "Maize (corn)")  %>% mutate(country_name=Area)

faostat_data$Value = faostat_data$Value / 10000

country_mapping <- data.frame(
  country_code = c("MI", "MZ", "ZA", "ZI", "SF"),
  country_name = c("Malawi", "Mozambique", "Zambia", "Zimbabwe", "South Africa")
)

country_gcvi <- country_gcvi %>%
  left_join(country_mapping, by = c("country" = "country_code"))

add_significance <- function(data, y_var, group_var = "country_name") {
  data %>%
    group_by(!!sym(group_var)) %>%
    do({
      model <- lm(reformulate("Year", response = y_var), data = .)
      summary_model <- summary(model)
      p_value <- coef(summary_model)[2,4]  # Extracting p-value for the slope
      data.frame(., is_significant = ifelse(p_value < 0.05, 1.5, 0.5))  # Assign 1.5 for true and 0.5 for false
    }) %>%
    ungroup()
}

country_gcvi <- add_significance(country_gcvi, y_var = "meanGCVI")
country_sif <- add_significance(country_sif, y_var = "mean")
faostat_data <- add_significance(faostat_data, y_var = "Value", group_var = "country_name")

# Create dataframe of all sources
combined_data <- bind_rows(
  country_gcvi %>%
    select(Year, meanGCVI, country_name, is_significant) %>%
    rename(value = meanGCVI) %>%
    mutate(Source = "MODIS"),
  faostat_data %>%
    select(Year, Value, Area, is_significant) %>%
    rename(value = Value, country_name = Area) %>%
    mutate(Source = "FAOSTAT") %>%
    filter(country_name %in% country_mapping$country_name),
  country_sif %>%
    select(Year, mean, country_name, is_significant) %>%
    rename(value = mean) %>%
    mutate(Source = "SIF")
)

countryPlot <- function(data) {
  
  country_filt <- unique(data$country_name)[1]
  country_data <- data %>% 
    filter(country_name == country_filt)
  
  faostat_values <- country_data$value[country_data$Source == "FAOSTAT"]
  modis_values <- country_data$value[country_data$Source == "MODIS"]
  sif_values <- country_data$value[country_data$Source == "SIF"]
  
  train_sec <- function(primary, secondary, na.rm = TRUE) {
    from <- range(secondary, na.rm = na.rm)
    to   <- range(primary, na.rm = na.rm)
    forward <- function(x) {
      scales::rescale(x, from = from, to = to)
    }
    reverse <- function(x) {
      scales::rescale(x, from = to, to = from)
    }
    list(fwd = forward, rev = reverse)
  }
  
  modis_sec <- train_sec(faostat_values, modis_values)
  sif_sec <- train_sec(faostat_values, sif_values)
  
  country_data$rescaled_value <- country_data$value
  country_data$rescaled_value[country_data$Source == "MODIS"] <- modis_sec$fwd(modis_values)
  country_data$rescaled_value[country_data$Source == "SIF"] <- sif_sec$fwd(sif_values)
  
  if (country_filt == "Zimbabwe") {
    
    plot <- ggplot() +
      geom_line(data = country_data %>% filter(Source == "FAOSTAT"), aes(x = Year, y = value, size = 0.2), color = "firebrick1") +
      geom_line(data = country_data %>% filter(Source == "MODIS"), aes(x = Year, y = modis_sec$fwd(value), size = 0.2), color = "cornflowerblue") +
      geom_line(data = country_data %>% filter(Source == "SIF"), aes(x = Year, y = sif_sec$fwd(value), size = 0.2), color = "chartreuse3") +
      scale_y_continuous(
        name = "", 
        sec.axis = sec_axis(~ ., name = "")
      ) +
      scale_x_continuous(name = "", breaks = c(2005, 2010, 2015, 2020)) +
      scale_size_continuous(range = c(0.4, 0.6)) +  # Adjust size scaling
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.title = element_text(vjust = 3, size = 24)
      ) +
      ggtitle(country_filt) + theme(plot.title = element_text(size = 14, hjust=0.5))
    
  } else {
    plot <- ggplot() +
      geom_line(data = country_data %>% filter(Source == "FAOSTAT"), aes(x = Year, y = value, size = is_significant), color = "firebrick1") +
      geom_line(data = country_data %>% filter(Source == "MODIS"), aes(x = Year, y = modis_sec$fwd(value), size = is_significant), color = "cornflowerblue") +
      geom_line(data = country_data %>% filter(Source == "SIF"), aes(x = Year, y = sif_sec$fwd(value), size = is_significant), color = "chartreuse3") +
      scale_y_continuous(
        name = "", 
        sec.axis = sec_axis(~ ., name = "")
      ) +
      scale_x_continuous(name = "", breaks = c(2005, 2010, 2015, 2020)) +
      scale_size_continuous(range = c(0.5, 1.5)) +  # Adjust size scaling
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.title = element_text(vjust = 3, size = 24)
      ) +
      ggtitle(country_filt) + theme(plot.title = element_text(size = 14, hjust=0.5))
  }
  
  return(plot + theme(plot.title = element_text(face = "plain")))
}



data_names <- split(combined_data, combined_data$country_name)
all_plots <- lapply(data_names, function(x) countryPlot(x))


# Create a dummy plot for the legend
legend_plot <- ggplot() +
  geom_point(aes(x = 0.5, y = 0.5, color = "MODIS"), size = 5) +
  geom_point(aes(x = 0.5, y = 0.4, color = "FAOSTAT"), size = 5) +
  geom_point(aes(x = 0.5, y = 0.3, color = "SIF"), size = 5) +
  scale_color_manual(values = c("MODIS" = "cornflowerblue", "FAOSTAT" = "firebrick1", "SIF" = "chartreuse3"), name = "Data Source") +
  theme_void() +
  theme(legend.position = "right") 

legend <- get_legend(legend_plot)

all_plots_with_legend <- c(all_plots, list(legend))

p1 <- plot_grid(plotlist = all_plots_with_legend, ncol = 3)


## Save fig 1
y.grob <- textGrob("Maize Yield (t/ha)", rot=90)
y2.grob <- textGrob("Greenness or SIF (rescaled)", rot=90)
x.grob <- textGrob("Year")

p1 <- grid.arrange(arrangeGrob(p1, left=y.grob, right=y2.grob, bottom=x.grob))
fig1c <- plot_grid(p1, ncol=1, rel_heights = c(1, 0.1))
# fig1b <- fig1b + theme(plot.margin = margin(7, 0, 0, 0, "mm"))
fig1c <- fig1c + theme(plot.margin = margin(7, 0, 0, 0, "mm"))

combined_fig <- plot_grid(fig1a, fig1c, ncol=2,labels=c("a", "b"), label_size=24, rel_widths = c(1/3, 2/3))
print(combined_fig)
