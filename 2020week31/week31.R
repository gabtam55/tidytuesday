# Palmerpenguins package citation
## Horst AM, Hill AP, Gorman KB (2020). palmerpenguins: Palmer
## Archipelago (Antarctica) penguin data. R package version 0.1.0.
## https://allisonhorst.github.io/palmerpenguins/. doi:
## 10.5281/zenodo.3960218.

# Load libraries
library(tidyverse)
library(ggpubr)
library(magick)
library(cowplot)
library(patchwork)
# Testing123

# Load data
tuesdata <- tidytuesdayR::tt_load('2020-07-28')
penguins <- tuesdata$penguins

# Clean data
penguins_cleaned <- penguins %>%
  filter(!is.na(body_mass_g), !is.na(sex)) %>%
  mutate(year = as.factor(year))

# Graph 1: Weight across years
g1 <- ggboxplot(penguins_cleaned, x = "year", y = "body_mass_g",
          color = "year", palette = c("#96ceb4", "#ffeead", "#ff6f69"),
          title = "Body mass remained steady across years ", xlab = "", ylab = "Body Mass (g)") +
  stat_compare_means(comparisons = list(c("2007", "2008"), c("2008", "2009"), c("2007", "2009")), method = "t.test") +
  stat_compare_means(method = "anova", label.x = 0.75, label.y = 8000) +
  font("title", size = 18, face = "bold", family = "Didot")

g1 <- ggpar(g1,
            legend = "right", legend.title = "",
            ylim = c(2000, 8000))

# Graph 2: Weight by species
g2 <- ggboxplot(penguins_cleaned, x = "species", y = "body_mass_g",
          color = "species", palette = c("#96ceb4", "#ffeead", "#ff6f69"),
          add = "jitter", shape = "species",
          title = "Gentoo are heavier than Adelie and Chinstrap", xlab = "", ylab = "Body Mass (g)") +
  stat_compare_means(comparisons = list(c("Adelie", "Chinstrap"), c("Chinstrap", "Gentoo"), c("Adelie", "Gentoo")), method = "t.test") +
  stat_compare_means(method = "anova", label.x = 0.75, label.y = 8000) +
  font("title", size = 18, face = "bold", family = "Didot")

g2<- ggpar(g2,
           legend = "right", legend.title = "",
           ylim = c(2000, 8000))

# Graph 3: Weight by sex
g3 <- ggviolin(penguins_cleaned, x = "sex", y = "body_mass_g",
         color = "sex", palette = c("#96ceb4", "#ffeead"),
         add = "jitter", shape = "sex",
         title = "Female are lighter", xlab = "", ylab = "Body Mass (g)")+
  stat_compare_means(method = "t.test", label.x = 1.5, label.y = 8000, hjust = 0.5) +
  font("title", size = 18, face = "bold", family = "Didot")

g3<- ggpar(g3,
           legend = "right", legend.title = "",
           ylim = c(2000, 8000))

# Graph 4: Weight of Adelie by islands
g4 <- penguins_cleaned %>%
  filter(species == "Adelie") %>%
  ggdotplot(x = "island", y = "body_mass_g",
          color = "island", palette = c("#96ceb4", "#ffeead", "#ff6f69"),
          add = "jitter", size = 1,
          title = "Adelie on different islands had similar weight", xlab = "", ylab = "Body Mass (g)") +
  font("title", size = 18, face = "bold", family = "Didot")

g4<- ggpar(g4,
           legend = "right", legend.title = "",
           ylim = c(2000, 8000))

# Import image
img <- image_read("https://allisonhorst.github.io/palmerpenguins/man/figures/palmerpenguins.png")
img <- ggdraw() +
  draw_image(img, scale = 0.5)

img2 <- image_read("https://allisonhorst.github.io/palmerpenguins/man/figures/lter_penguins.png")
img2 <- ggdraw()+
  draw_image(img2, scale = 0.9)


# Patch graphs and images together
g1 + img + g2 + g3 + img2 + g4 +
  plot_annotation(title = "Body Mass of Palmer Penguins from 2007 to 2009",
                  caption = "\nSource & Artwork: Gorman KB, Hill AP & Horst AM | Graphic: Gabriel Tam",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold", family = "Didot"),
                                plot.caption = element_text(size = 16, family = "Didot")))
# Save image
ggsave("palmer_penguins.png", width = 51, height = 29, units = c("cm"))
