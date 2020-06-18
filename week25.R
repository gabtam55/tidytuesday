# Load packages
library(tidyverse)
library(scales)

# Load data
slave_routes <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')

# Extract the 9 port_origin that were involved in transporting most n_slaves_arrived as a vector
top9_port_origin <- slave_routes %>%
  filter(!is.na(port_origin)) %>%
  group_by(port_origin) %>%
  summarise(sum = sum(n_slaves_arrived, na.rm = TRUE)) %>%
  arrange(desc(sum)) %>%
  top_n(sum, n = 9) %>%
  pull(port_origin)

# Generate line graph for n_slaves_arrived over decades from the 9 port_origin
slave_routes %>%
  filter(!is.na(year_arrival), !is.na(port_arrival)) %>%
  mutate(decade = year_arrival - year_arrival %% 10) %>%
  group_by(decade, port_origin) %>%
  summarise(sum_slaves_arrived = sum(n_slaves_arrived, na.rm = TRUE)) %>%
  filter(port_origin %in% top9_port_origin) %>%
  ggplot(aes(decade, sum_slaves_arrived, color = port_origin)) +
  geom_line(size = 0.75, show.legend = FALSE) +
  facet_wrap(~port_origin) +
  labs(title = "Busiest Ports Of Origin",
       subtitle = "Number of slaves transported transatlantically from 1510 to 1860",
       caption = "\nSource: Slave Voyages | Graphic: Gabriel Tam",
       x = NULL, y = NULL) +
  expand_limits(x = c(1500, 1900)) +
  scale_x_continuous(breaks = seq(1500, 1900, 100)) +
  scale_y_continuous(labels = comma) +
  theme_gray() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.title = element_text(size = 18, face = "bold", hjust = 1),
        plot.subtitle = element_text(size = 12, hjust = 1, margin = margin(0, 0, 10, 0)),
        plot.caption = element_text(size = 10, hjust = -0.08),
        panel.spacing = unit(2, "lines"),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 8.5, color = "white", face = "bold"),
        strip.background = element_rect(fill = "black")) +
  ggsave(file = "busiest_ports_of_origin.png", width = 8, height = 8)
