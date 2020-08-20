# Load packages-----------------------------------------------------------------
library(tidyverse)
library(ggthemes)

# Read data---------------------------------------------------------------------
survey_data <- readxl::read_xlsx(here::here("Cleaned Survey Data.xlsx"))


# Explore data------------------------------------------------------------------
glimpse(survey_data)
summary(survey_data)
visdat::vis_dat(survey_data)
DataExplorer::create_report(survey_data)
inspectdf::show_plot(inspectdf::inspect_cat(survey_data))


# Wrangle data------------------------------------------------------------------
survey_data <- survey_data %>%
  ## Assign those who didn't provide income data to the US average salary
  ## (i.e. $62000) group
  mutate(income = case_when(
    `Household Income` == "Prefer not to answer" ~ "$50,000-$74,999",
    TRUE ~ `Household Income`
  )) %>%
  ## Convert education to factor and re-order it
  mutate(Education = factor(Education,
                            levels= c("Some highschool",
                                     "High School or GED", 
                                      "Some Undergraduate",
                                      "Completed Undergraduate",
                                      "Some Masters",
                                      "Completed Masters",
                                      "Some Phd",
                                      "Completed Phd"))) %>%
  ## For some reason 'Some Masters' were not detected and were all converted
  ## into NA from the above step. Therefore, manual renaming was requried.
  mutate(Education = replace_na(Education, "Some Masters"))


# Format data for plot----------------------------------------------------------
plot_data <- survey_data %>%
  summarise(`Y_Mental Illness` = sum(`I identify as having a mental illness`, na.rm = TRUE),
            `N_Mental Illness` = n() - sum(is.na(`I identify as having a mental illness`)) - `Y_Mental Illness`,
            Y_Anxiety = sum(Anxiety, na.rm = TRUE),
            N_Anxiety = n() - sum(is.na(Anxiety)) - Y_Anxiety,
            `Y_Compulsive Behaviour` = sum(`Compulsive behavior`, na.rm = TRUE),
            `N_Compulsive Behaviour` = n() - sum(is.na(`Compulsive behavior`)) - `Y_Compulsive Behaviour`,
            Y_Depression = sum(Depression, na.rm = TRUE),
            N_Depression = n() - sum(is.na(Depression)) - Y_Depression,
            `Y_Lack of Concentration` = sum(`Lack of concentration`, na.rm = TRUE),
            `N_Lack of Concentration` = n() - sum(is.na(`Lack of concentration`)) - `Y_Lack of Concentration`,
            `Y_Mood Swings` = sum(`Mood swings`, na.rm = TRUE),
            `N_Mood Swings` = n() - sum(is.na(`Mood swings`)) - `Y_Mood Swings`,
            `Y_Obsessive Thinking` = sum(`Obsessive thinking`, na.rm = TRUE),
            `N_Obsessive Thinking` = n() - sum(is.na(`Obsessive thinking`)) - `Y_Obsessive Thinking`,
            `Y_Panic Attacks` = sum(`Panic attacks`, na.rm = TRUE),
            `N_Panic Attacks` = n() - sum(is.na(`Panic attacks`)) - `Y_Panic Attacks`,
            Y_Tiredness = sum(Tiredness, na.rm = TRUE),
            N_Tiredness = n() - sum(is.na(Tiredness)) - Y_Tiredness) %>%
  pivot_longer(everything(),
               names_to = c(".value", "mental_issue"),
               names_sep= "_") %>%
  mutate(prevalence = Y / (Y + N)) %>%
  mutate(mental_issue= factor(mental_issue, levels = mental_issue))


# Generate bar chart to show prevalence of each mental illness------------------
plot_data %>%
  ggplot(aes(mental_issue, prevalence)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format(), lim = c(0, 1)) +
  theme_economist() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 90))
