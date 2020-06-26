# Load packages
library(tidyverse)
library(ggmap)
library(gganimate)
library(gifski)

# Load data
individuals <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/individuals.csv')
locations <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')

# Join the 2 data
joint_data <- individuals %>%
  inner_join(locations, by = "animal_id", suffix = c("_1", "_2"))

# Mutate timestamp into year and month, then arrange it in ascending order.
joint_data <- joint_data %>%
  mutate(timestamp_ym = format(timestamp, "%Y-%B")) %>%
  arrange(timestamp_ym)

# Identify longitude and latitude to be used for the map 
joint_data %>%
  summarise(mid_longitude = (max(longitude) + min(longitude)) / 2,
            mid_latitude = (max(latitude) + min(latitude)) / 2) # mid_longitude = -122, mid_latitude = 55.1

# Get the map
register_google(key = "SET YOUR GOOGLE MAPS STATIC API KEY")

map <- ggmap(get_googlemap(center = c(lon = -122, lat = 55.1),
                           zoom = 6, scale = 2, 
                           maptype = "terrain",
                           color = "color"))

# Add points to locate the caribous on the map
caribou_map <- map + geom_point(aes(x = longitude, y = latitude),
                                size = 1.5, alpha = 0.25, color = "orange",
                                data = joint_data) +
  labs(caption = "\n\n Source: BC Ministry of Environment | Graphic: Gabriel Tam",
       x = "Longitude", y = "Latitude") +
  theme(text = element_text(size = 22),
        plot.title = element_text(size = 24, face = "bold"))

# Animate caribou_map
animated_map <- caribou_map + transition_manual(frames = factor(timestamp_ym, levels = unique(timestamp_ym)),
                                                cumulative = FALSE) +
  ggtitle("Movement of Caribou in Northern British Columbia", 
          subtitle = "{current_frame}")

# Render and save animation
animate(plot = animated_map,
        nframes = length(unique(joint_data$timestamp_ym)),
        fps = 6, end_pause = 5,
        height = 500, width = 500, res = 50) +
  anim_save("week26.gif")
