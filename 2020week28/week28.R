# Load the data
tuesdata <- tidytuesdayR::tt_load('2020-07-07')
coffee_ratings <- tuesdata$coffee_ratings

# Load packages
library(tidyverse)
library(lubridate)
library(stringr)

# Identify the highest-rated bean of the year from 2010 to 2018
best_beans <- coffee_ratings %>%
  filter(!is.na(grading_date)) %>%
  mutate(grading_year = year(mdy(grading_date)),
         # Tidy up the variety column
         variety = case_when(
           is.na(variety) ~"Unknown Variety",
           variety == "Other" ~ "Unknown Variety",
           TRUE ~ variety
         ),
         # Tidy up the owner_1 column
         owner_1 = tools::toTitleCase(owner_1),
         owner_1 = str_replace(owner_1, "\\.", ""),
         owner_1 = case_when(
           owner_1 == "NUCOFFEE" ~ "NuCoffee",
           TRUE ~ owner_1
         )) %>%
  group_by(grading_year) %>%
  top_n(1, total_cup_points) %>%
  arrange(grading_year) %>%
  rename_with(tools::toTitleCase) %>%
  select(Grading_year, Variety, Owner_1, Acidity, Aftertaste, Aroma, Balance, Body, Clean_Cup = Clean_cup,
         Cupper_Points = Cupper_points, Flavor, Moisture, Sweetness, Uniformity) %>%
  pivot_longer(cols = Acidity:Uniformity, names_to = "criteria", values_to = "grade")
  
# Visualise results on wind rose diagrams
ggplot(best_beans, aes(criteria, grade)) +
  geom_col(color = NA, fill = "#800000", alpha = 0.7) +
  coord_polar(clip = "off") +
  facet_wrap(~paste0(Grading_year, "\n", Variety, " by", "\n", Owner_1)) +
  theme_bw() +
  labs(title = "Profile of the Winning Coffee Beans",
       subtitle = "Below are the bean varieties and their owners that recieved the highest professional rating each year\nfrom 2010 to 2018. Each diagram shows the sub-ratings that they received on a 1-10 scale. \n",
       # Check James out (@jmzledoux)
       caption = "\nSource: James LeDoux & Coffee Quality Database | Graphic: Gabriel Tam") +
  theme(plot.margin = margin(0.5, 0.25, 0.5, 0.25, "cm"),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold", color = "#330000"),
        plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic", color = "#800000"),
        plot.caption = element_text(size = 10, color = "#28283e"),
        
        axis.title = element_blank(),
        axis.text = element_text(size = 9, color = "#28283e"),
        
        panel.border = element_blank(),
        panel.spacing = unit(1, "cm"),
        
        strip.background = element_rect(fill = NA, color = NA),
        strip.text = element_text(size = 10, face = "bold")
        ) +
  ggsave(file = "profile_of_best_beans.png", width = 10, height = 13)
