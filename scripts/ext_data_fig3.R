### Extended data fig 3

library(dplyr)
library(ggplot2)
library(patchwork)

DEFAULT_COLOR <- "black"

base_theme <- 
  theme_classic(base_size = 15) + 
  theme(
    panel.spacing = unit(1, "lines"),
    axis.title.x = element_text(color=DEFAULT_COLOR),
    axis.title.y = element_text(color=DEFAULT_COLOR),
    axis.line = element_line(color=DEFAULT_COLOR, size=0.25),
    axis.text.x = element_text(color=DEFAULT_COLOR), 
    axis.text.y = element_text(color=DEFAULT_COLOR),
    axis.ticks = element_line(color=DEFAULT_COLOR, size=0.25), 
    legend.text = element_text(color=DEFAULT_COLOR),
    legend.title = element_text(color=DEFAULT_COLOR),
    plot.subtitle = element_text(color=DEFAULT_COLOR),
    strip.background = element_rect(color=DEFAULT_COLOR, fill=NA, size=0.5),
    strip.text = element_text(color=DEFAULT_COLOR)
  )

pp <- read.csv("/Users/richardlee/Documents/greenness_trends/repo/data/ext_data_fig3/prod_price.csv")

pp_plot <- ggplot(pp, aes(x = Year, y = Value)) + 
  geom_line() + 
  facet_wrap(~ Area) +
  labs(x = "Year", y = "Producer Price (USD)") +
  base_theme
