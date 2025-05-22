### Extended data fig 6

library(ggplot2)      
library(dplyr)        
library(terra)        
library(sf)           
library(rnaturalearth)
library(ggspatial) 

sf_lt_modis <- rast("data/sf_lt_2km.tiff")
NAflag(sf_lt_modis) <- -9999
field_size <- rast("data/sf_lt_fieldsize_2km.tiff")
NAflag(field_size) <- -9999
NAflag(field_size) <- 0

countries_sf <- ne_countries(scale = "medium", returnclass = "sf")
countries_sf <- countries_sf %>% 
  filter(admin %in% c("South Africa", "Lesotho"))

xlims <- c(26.5, 30)
ylims <- c(-31, -28)
countries_sf <- st_crop(countries_sf, xmin=xlims[1], xmax=xlims[2], ymin=ylims[1], ymax=ylims[2])

sf_lt_modis <- as.data.frame(sf_lt_modis, xy = TRUE, na.rm = TRUE)
field_size <- as.data.frame(field_size, xy = TRUE, na.rm = TRUE)

label_data <- data.frame(
  country = c("South Africa", "Lesotho"),
  x = c(27.25, 28.25), # Adjust these coordinates as needed
  y = c(-30.75, -29.5)  # Adjust these coordinates as needed
)

fig6a <- ggplot() +geom_tile(data = sf_lt_modis, aes(x = x, y = y, fill = slope))+ geom_sf(data = countries_sf, color="black", fill=NA) + coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
  scale_fill_gradientn(colors = hcl.colors(200, "RdYlGn"), limits = c(-0.2, 0.2), name = "GCVI Trends\n(2003-2022)") +
  guides(fill = guide_colorbar(title.position = "top")) +
  theme_minimal()+
  labs(title="a") +
  theme(
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.title.align = 0.5,
    legend.title = element_text(hjust = 0.5),
    plot.title = element_text(size=24,face="bold")
  ) 
# labs(fill = "GCVI")

field_size$b1 <- factor(field_size$b1, 
                        levels = c(3502, 3503, 3504, 3505, 3506),
                        labels = c(">100", "16-100", "2.56-16", "0.64-2.56", "<0.64"))
field_size <- na.omit(field_size)

fig6b <- ggplot() +geom_tile(data = field_size, aes(x = x, y = y, fill = b1))+ geom_sf(data = countries_sf, color="black", fill=NA) + coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
  scale_fill_manual(values = c(">100" = "#1273de", 
                               "16-100" = "#008b02", 
                               "2.56-16" = "#c1e1c5", 
                               "0.64-2.56" = "#fef3bd", 
                               "<0.64" = "#d86b3f"),
                    name = "Field Size (ha)",
                    breaks = c("<0.64", "0.64-2.56", "2.56-16", "16-100", ">100"),
                    labels = c("<0.64", "0.64-2.56", "2.56-16", "16-100", ">100")) +
  theme_minimal() +
  labs(title="b") + 
  theme(
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size=24, face="bold")
  ) +
  labs(fill = "b1")

fig6a <- fig6a +
  geom_text(data = label_data, aes(x = x, y = y, label = country), color = "black", size = 6) +
  annotation_scale(location = "br", width_hint = 0.25) # Add a map scale at the bottom right

fig6b <- fig6b +
  geom_text(data = label_data, aes(x = x, y = y, label = country), color = "black", size = 6) +
  annotation_scale(location = "br", width_hint = 0.25) # Add a map scale at the bottom right

fig6 <- fig6a + fig6b

