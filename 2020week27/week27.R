# Load package
library(tidyverse)
library(ggpubr)
theme_set(theme_minimal())

# Read in data
tuesdata <- tidytuesdayR::tt_load('2020-06-30')
characters <- tuesdata$characters
character_visualisation <- tuesdata$character_visualization

# Group by character and tidy data
characters_by_character <- characters %>%
  group_by(character) %>%
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>%
  mutate(character = str_remove(character, " =.*")) %>%
  select(-issue)

character_visualisation_by_character <- character_visualisation %>%
  group_by(character) %>%
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>%
  mutate(character = str_remove(character, " =.*"),
         character = str_remove(character, "\\*")) %>%
  select(-issue)

# Join data
character_joint <- characters_by_character %>%
  full_join(character_visualisation_by_character, by = "character") %>%
  filter(!str_detect(character, "narration"))

# Relationship between thinking and eating
character_joint %>%
  ggplot(aes(thought, depicted_eating_food)) +
  geom_point(shape = 18, size = 4, color = "#FFFE03") +
  geom_smooth(method = "lm", color = "#F94115", fill = "#F94115", alpha = 0.2) +
  stat_cor(label.x = 1375, label.y = 15.75, size = 5, color = "#F94115", hjust = 0.5) +
  geom_label(x = 532, y = 20.75, label = "Wolverine", size = 5, fill = "#1D90F3") +
  geom_label(x = 1900, y = 15.75, label = "Storm", size = 5, fill = "#1D90F3") +
  labs(x = "No. of thought bubbles the character had",
       y = "No. of times the character\nwas depicted eating food",
       title = " Does thinking make <span style = 'color: #FFFE03;'>X-Men</span> characters hungry?",
       subtitle = "An analysis of the Uncanny X-Men #97-280\n",
       caption = "\n\nSource: Claremont Run Project | Graphic: Gabriel Tam") +
  theme(plot.background = element_rect(fill = "#0E1113"),
        panel.grid = element_line(color = "#efeff5"),
        text = element_text(color = "#efeff5"),
        plot.title = element_markdown(size = 22, color = "#1D90F3", face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.caption = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14, color = "#efeff5")) +
  ggsave(file = "thought_vs_eating.png", width = 12, height = 8)
