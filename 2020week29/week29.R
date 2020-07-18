# Load libraries
library(tidytuesdayR)
library(tidyverse)
library(jpeg)
library(ggpubr)
library(ggimage)
theme_set(theme_minimal())

# Load data
tuesdata <- tidytuesdayR::tt_load('2020-07-14')
astronauts <- tuesdata$astronauts

# Wrangle data
astronauts2 <- astronauts %>%
  mutate(nationality = fct_lump(nationality, 4)) %>%
  arrange(year_of_mission, nationality) %>%
  group_by(year_of_mission) %>%
  mutate(n = 1:n(),
         image = case_when(
           nationality == "U.S." ~ "us.png",
           nationality == "U.S.S.R/Russia" ~ "russia.png",
           nationality == "Japan" ~ "japan.png",
           nationality == "Canada" ~ "canada.png",
           nationality == "France" ~ "france.png",
           TRUE ~ "other.png"
         ))

# Read in background image
img <- readJPEG("galaxy.jpg")

# Generate plot
ggplot(astronauts2, aes(x = year_of_mission, y = n, image = image)) +
  background_image(img) +
  geom_image(size = 0.015) +
  labs(title = "Nationality of astronauts who've completed space missions in 1961 - 2019",
       subtitle = "Country flag of the top 5 nationalities were shown.\nBlue flag represents any other countries.\n",
       x = "Year of mission",
       y = "",
       caption = "\nSource: Mariya Stavnichuk & Tatsuya Corlett | Image: Countryflags.com & Freepik.com | Graphic: Gabriel Tam") +
  theme(text = element_text(color = "gray24", family = "Times"),
        
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
        plot.title = element_text(size = 32, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 28, hjust = 0.5),
        plot.caption = element_text(size = 20),
        
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_blank(),
        axis.line.y = element_blank()) +
  ggsave("astronauts.png", width = 45, height = 35, units = c("cm"))
