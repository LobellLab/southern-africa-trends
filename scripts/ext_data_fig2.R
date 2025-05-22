### Extended data fig 2

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

faostat <- read.csv("/Users/richardlee/Documents/greenness_trends/repo/data/ext_data_fig2/FAOSTAT_data_en_8-28-2024.csv")

filtered_data <- faostat %>%
  filter(Element %in% c("Production", "Area harvested"))

## get production and area data for each country year crop
yield_data <- filtered_data %>%
  group_by(Area, Item, Year) %>%
  summarize(Production = sum(ifelse(Element == "Production", Value, 0)),
            Area_Harvested = sum(ifelse(Element == "Area harvested", Value, 0)),
            .groups = 'drop') %>%
  filter(!is.na(Production) & !is.na(Area_Harvested))

## calories per 100g
## calories retrieved from: https://fdc.nal.usda.gov/
calorie_convert <- data.frame(
  Item = c("Maize (corn)", "Groundnuts, excluding shelled", "Beans, dry", "Pigeon peas, dry", "Cassava, fresh", "Sorghum", "Wheat", "Sunflower seed", "Soya bean", "Sugar cane", "Seed cotton, unginned", "Millet"),
  cal = c(364, 588, 341, 343, 160, 329, 332, 609, 446, 401, 506, 378)
)

## calculate kcal/ha
yield_with_calories <- yield_data %>%
  left_join(calorie_convert, by = "Item") %>%
  filter(!is.na(cal)) %>%
  mutate(calorie_yield = (Production * 1e6 / Area_Harvested) * (cal / 100))

top5_aggregated_calories <- yield_with_calories %>%
  group_by(Area, Year, Item) %>%
  summarize(total_calories = sum(calorie_yield * Area_Harvested),
            total_area = sum(Area_Harvested), 
            .groups = 'drop') %>%
  group_by(Area, Year) %>%
  arrange(desc(total_area)) %>%
  slice_head(n = 5) %>%
  summarize(agg_calories = sum(total_calories) / sum(total_area),
            .groups = 'drop') %>%
  mutate(Item = "Top 5 Crops")

maize_calories <- yield_with_calories %>%
  filter(Item == "Maize (corn)") %>%
  mutate(agg_calories = calorie_yield) %>%
  select(Area, Year, Item, agg_calories, Area_Harvested)

combined_calories <- bind_rows(top5_aggregated_calories %>% select(Area, Year, agg_calories, Item),
                               maize_calories %>% select(Area, Year, agg_calories, Item))

# Calculate total area for top 5 crops
top5_area <- yield_with_calories %>%
  group_by(Area, Year, Item) %>%
  summarize(total_area = sum(Area_Harvested), .groups = 'drop') %>%
  group_by(Area, Year) %>%
  arrange(desc(total_area)) %>%
  slice_head(n = 5) %>%
  summarize(top5_area = sum(total_area), .groups = 'drop')

# Merge maize area and top 5 area
maize_area <- yield_with_calories %>%
  filter(Item == "Maize (corn)") %>%
  select(Area, Year, maize_area = Area_Harvested)

# Merge all grains area with the existing area data
area_data <- left_join(maize_area, top5_area, by = c("Area", "Year")) %>%
  mutate(proportion = maize_area / top5_area)

all_grains_area <- yield_with_calories %>% 
  filter(Item %in% c("Barley", "Buckwheat", "Maize (corn)", "Millet", "Oats", "Rice", "Rye", "Sorghum", "Wheat")) %>%
  group_by(Area, Year) %>%
  summarize(grains_area = sum(Area_Harvested), .groups = 'drop')

area_data_with_grains <- area_data %>%
  left_join(all_grains_area, by = c("Area", "Year"))

grains <- c("Barley", "Buckwheat", "Maize (corn)", "Millet", "Oats", "Rice", "Rye", "Sorghum", "Wheat")

grains_aggregated_calories <- yield_with_calories %>%
  filter(Item %in% grains) %>% 
  group_by(Area, Year) %>%
  summarize(agg_calories = sum(calorie_yield * Area_Harvested) / sum(Area_Harvested),
            agg_area = sum(Area_Harvested),
            .groups = 'drop') %>%
  mutate(Item = "Grains")

# First Plot: Maize Area vs. Top 5 Crop Area
area_plot <- ggplot(area_data_with_grains, aes(x = Year)) +
  geom_line(aes(y = maize_area, color = "Maize")) +
  geom_line(aes(y = top5_area, color = "Top 5 Crops")) +
  geom_line(aes(y = grains_area, color = "All Grains")) +
  facet_wrap(~ Area, scales = "free_y") +
  scale_y_continuous(name = "Area Harvested (ha)") +
  labs(title = "", 
       x = "Year", 
       color = "Group") +  # Ensure consistent grouping label
  base_theme

# Combine all calorie data for maize, top 5 crops, and grains
combined_calories_all <- bind_rows(
  maize_calories %>% mutate(Item = "Maize"),
  top5_aggregated_calories %>% mutate(Item = "Top 5 Crops"),
  grains_aggregated_calories %>% mutate(Item = "All Grains")
)

# Second Plot: Maize Calories, Top 5 Crop Calories, and All Grains Calories
calorie_plot <- ggplot(combined_calories_all, aes(x = Year, y = agg_calories, color = Item, group = Item)) +
  geom_line() +
  facet_wrap(~ Area, scales = "free_y") +
  scale_y_continuous(name = "Calories (kcal/ha)") +
  labs(title = "",
       x = "Year",
       color = "Group") +  # Ensure consistent grouping label
  base_theme + 
  theme(legend.position = "none")

# Combine both plots into one figure with a single legend
supp_fig <- area_plot / calorie_plot + 
  plot_layout(guides = "collect", axis_titles = "collect")