library(tidyverse)
library(ggtext)
library(glue)
#install.packages("rcartocolor")
library(rcartocolor)
# devtools::install_github("AllanCameron/VoronoiPlus")
library(VoronoiPlus) 

all_countries <- read_csv("all_countries.csv")
country_regions <- read_csv("country_regions.csv")

au_data <- all_countries |> 
  select(Category, Subcategory, hoursPerDayCombined, country_iso3) |> 
  left_join(country_regions, by = "country_iso3") |> 
  filter(country_name == "Australia") |> 
  select(Category, Subcategory, hoursPerDayCombined)

au_vor <- voronoi_treemap(hoursPerDayCombined ~ Category + Subcategory,
                          data = au_data)

set.seed(1234)
groups <- filter(au_vor, level == 1)

subgroups <- filter(au_vor, level == 2) |> 
  group_by(group) |> 
  mutate(alpha = runif(1, 0, 0.6)) |> 
  ungroup()

cols_vec = rcartocolor::carto_pal(length(unique(au_data$Category))+1, "Prism")[1:length(unique(au_data$Category))]
names(cols_vec) = unique(au_data$Category)

ggplot() +
  geom_polygon(data = groups,mapping = aes(x = x, y = y, group = group, fill = group),
               colour = "white",linewidth = 5) +
  geom_polygon(data = subgroups,
               mapping = aes(x = x, y = y, group = group, alpha = alpha),
               fill = "#fafafa",colour = "#fafafa",linewidth = 0.3) +
  geom_text(data = groups %>% group_by(group) %>% 
              summarize(x = mean(x), y = mean(y)),
            aes(label=group,x,y),color="black") +
  scale_alpha_identity() +
  scale_fill_manual(values = cols_vec) +
  coord_equal() +
  theme_void(base_size = 12) +
  theme(legend.position = "non",
    plot.background = element_rect(fill = "white", colour = "white"),
    panel.background = element_rect(fill = "white", colour = "white"),
    plot.margin = margin(10, 10, 10, 10))

