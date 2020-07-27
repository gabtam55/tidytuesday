# Load libraries
library(tidyverse)
library(patchwork)
library(ggthemes)

# Set theme
theme_set(theme_fivethirtyeight())

# Load data
tuesdata <- tidytuesdayR::tt_load('2020-07-21')
animal_outcomes <- tuesdata$animal_outcomes

# Check the 'Total' column
animal_outcomes %>%
  mutate(total2 = rowSums(.[4:11], na.rm = TRUE)) %>%
  mutate(check = case_when(
    Total == total2 ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  filter(check == FALSE) # Errors were identified within the 'Total' column

# Correct the 'Total' column
animal_outcomes_corrected <- animal_outcomes %>%
  mutate(Total_2 = rowSums(.[4:11], na.rm = TRUE))

# Wrangle data
## Combine 'recalimed', 'rehomed' and 'released'
animal_outcomes_2 <- animal_outcomes_corrected %>%
  mutate(outcome_2 = case_when(
    outcome == "Reclaimed" | outcome == "Rehomed" | outcome == "Released" ~ "Reclaimed, rehomed or released",
    TRUE ~ outcome
  )) %>%
  group_by(year, animal_type, outcome_2) %>%
  summarise(count = sum(Total_2))

## Pivot data longer
animal_outcomes_long <- animal_outcomes_corrected %>%
  pivot_longer(cols = ACT:WA, names_to = "region", values_to = "count") %>%
  select(-Total, -Total_2)

# Create graphs
## Graph 1: Animals euthanised from 1999 to 2018
g1 <- animal_outcomes_2 %>%
  filter(outcome_2 == "Euthanized") %>%
  ggplot(aes(year, count, color = animal_type)) +
  geom_line(size = 1, show.legend = FALSE) +
  geom_vline(xintercept = 2007, linetype = 3) +
  labs(title = "Number of animals euthanised") +
  annotate(geom = "curve", x = 2011, y = 35000, xend = 2007.1, yend = 30000,
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 2011.1, y = 35000, hjust = "left", size = 3,
           label = "Since 2007, the number of cats and dogs\neuthanised have been dropping while\nthe opposite trend was found in wildlife.") +
  theme(plot.title = element_text(size = 14),
        panel.grid.major.x = element_blank())
  
## Graph 2: Animals recalimed, rehomed or released from 1999 to 2018
g2 <- animal_outcomes_2 %>%
  filter(outcome_2 == "Reclaimed, rehomed or released") %>%
  ggplot(aes(year, count, color = animal_type)) +
  geom_line(size = 1) +
  guides(color = guide_legend(ncol = 6)) +
  geom_vline(xintercept = 2010, linetype = 3) +
  labs(title = "Number of animals reclaimed/ rehomed/ released") +
  annotate(geom = "curve", x = 2012, y = 19000, xend = 2010.1, yend = 25000,
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 2011, y = 18500, hjust = "left", vjust = "top", size = 3,
           label = "Since 2010, the number of cats reclaimed/\nrehomed continued to rise while a downward\ntrend was found in dogs.") +
  annotate(geom = "text", x = 2006, y = 7500, hjust = "left", vjust = "top", size = 3,
           label = "Overall, the number of wildlife released have been low.") +
  theme(plot.title = element_text(size = 14),
        panel.grid.major.x = element_blank(),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"))

## Graph 3: Percentage of wildlife euthanised in each region from 2007 to 2018
g3 <- animal_outcomes_long %>%
  filter(animal_type == "Wildlife", year >= 2007) %>%
  pivot_wider(names_from = outcome, values_from = count) %>%
  mutate(total = rowSums(.[4:9], na.rm = TRUE)) %>%
  group_by(region) %>%
  summarise(perc_euthanized = sum(Euthanized) / sum(total)) %>%
  ggplot(aes(fct_reorder(region, perc_euthanized), perc_euthanized)) +
  geom_col(fill = "#F564E3", width = 0.75) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  coord_flip() +
  labs(title = "% of wildlife euthanised in each region from 2007 to\n2018") +
  annotate(geom = "segment", x = "QLD", y = 0.65, xend = "QLD", yend = 0.59,
           arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = "QLD", y = 0.66, hjust = "left", size = 3,
           label = "In Queensland,\n58% of the wildlife\nwere ethanized.") +
  theme(plot.title = element_text(size = 10),
        panel.grid.major = element_blank())

## Graph 4: Number of wildlife euthanised from 2007 to 2018 (Total vs QLD)
g4 <- animal_outcomes_corrected %>%
  filter(animal_type == "Wildlife", year >= 2007, outcome == "Euthanized") %>%
  select(1:3, 7, 13) %>%
  pivot_longer(cols = QLD:Total_2, names_to = "region", values_to = "count") %>%
  ggplot(aes(year, count, fill = region)) +
  scale_fill_manual(values = c("QLD" = "#F564E3", "Total_2" = "#551A8B")) +
  geom_ribbon(aes(ymax = count, ymin = 0), alpha = 0.3, show.legend = FALSE) +
  labs(title = "Number of wildlife euthanised from 2007 to 2018\n") +
  annotate(geom = "text", x = 2007.1, y = 12500, hjust = "left", size = 3,
           label = "Significant proportion of wildlife\neuthanized in Australia occured\nin Queensland.") +
  annotate(geom = "text", x = 2008.5, y = 5300, hjust = "left", vjust = "bottom", size = 4, color = "#191970",
           label = "Australia") +
  annotate(geom = "text", x = 2015, y = 2500, hjust = "middle", size = 4, color = "#68228B",
           label = "Queensland") +
  theme(plot.title = element_text(size = 10),
        panel.grid.major.x = element_blank())

## Graph 5: Number of cats and dogs reclaimed and rehomed
anno <- data.frame(xstart = c(2004, 2002.5), ystart = c(28000, 28000), outcome = c("Reclaimed", "Rehomed"),
                   lab = c("Dogs were much more likely to be\nreclaimed than cats were.",
                           "More cats are getting rehomed\nwhile less dogs are getting so."))

g5 <- animal_outcomes_corrected %>%
  filter(animal_type %in% c("Cats", "Dogs"), outcome %in% c("Reclaimed", "Rehomed")) %>%
  ggplot(aes(year, Total_2)) +
  scale_color_manual(values = c("Cats" = "#F8766D", "Dogs" = "#B79F00")) +
  geom_line(aes(color = animal_type), size = 0.75, show.legend = FALSE) +
  facet_grid(. ~ outcome) +
  geom_text(data = anno, aes(x = xstart, y = ystart, label = lab, hjust = "left", vjust = "top"), size = 3) +
  labs(title = "Number of cats and dogs reclaimed/ rehomed\n") +
  theme(plot.title = element_text(size = 10),
        panel.grid.major.x = element_blank(),
        strip.text = element_text(size = 9, face = "bold.italic"),
        strip.background = element_rect(fill = "grey70"))

# Patch graphs altogether
layout <- "
AABB
CDEE"

g1 + g2 + g3 + g4 + g5 +
  plot_layout(design = layout, guide = "collect") + 
  theme(legend.position = "bottom") +
  plot_annotation(title = "Outcomes of animals taken care of by RSPCA Australia",
                  caption = "Source: RSPCA | Graphic: Gabriel Tam",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 20),
                                plot.caption = element_text(size = 12)))

# Save image
ggsave("rspca_animal_outcomes.png", width = 51, height = 29, units = c("cm"))
