library(shiny)
library(tidyverse)
library(readr)
library(ggplot2)
library(RColorBrewer)
library(showtext)

# Fonts
showtext_auto()
font_add(family = "raleway", regular = "fonts/Raleway-Regular.ttf")
font_add(family = "instruction", regular = "fonts/Instruction.otf")

# Turn off scientific notation
options(scipen=999)

# import latest csv with updated predictions
covid <- read_csv("visualization_scripts/data/us_state_level_clean_2020-11-29.csv")

lookup <- covid

names(lookup) <- c("a", "b", "c", "d", "e", "f")
# Lookup variable names / descriptions
lookup_list <- setNames(as.list(lookup$b), lookup$a)
lookup_list2 <- setNames(as.list(lookup$a), lookup$b)

lookup_state <- setNames(as.list(covid$Province_State), covid$State_FIPS)


# selecting states of interest
states_sel <- c("New York", "Florida", "Texas", "California")

# filtering dataframe for states
covid_sel <- covid %>%
  filter(Province_State %in% states_sel)


# LINE GRAPH PLOTS

# Deaths
g_deaths <- ggplot(data = covid_sel,
                   aes(x = Date, y = new_deaths_7,
                       group = Province_State, color = Province_State)) +
  geom_line(size = 1.1) +
  scale_color_viridis_d(option = "viridis", name = "State") +
  geom_vline(xintercept = as.Date("2020-10-01"), color = "slategray", size = 2, alpha = 0.3) +
  # geom_rect(aes(xmin = as.Date("2020-10-01"), xmax = max(Date), ymin = 0, ymax = max(new_deaths_7)), alpha = 0.1, inherit_aes = FALSE) +
  labs(title = "\nCOVID-19 Daily Deaths Count\n",
       x = "\nDate\n",
       y = "\n7-Day Average of Daily Deaths\n") +
  theme_minimal() +
  theme(text = element_text(family = "raleway"),
        plot.title = element_text(family = "instruction", color = "grey20"),
        axis.line = element_line(color = "grey60"))



# Confirmed
g_confirmed <- ggplot(data = covid_sel, aes(x = Date, y = new_confirmed_7, group = Province_State, color = Province_State)) +
  geom_line(size = 1.1) +
  scale_color_viridis_d(option = "magma", name = "State") +
  geom_vline(xintercept = as.Date("2020-10-01"), color = "slategray", size = 2, alpha = 0.3) +
  labs(title = "\nCOVID-19 Daily Confirmed Cases Count\n",
       x = "\nDate\n",
       y = "\n7-Day Average of Confirmed Cases\n") +
  theme_minimal() +
  theme(text = element_text(family = "raleway"),
        plot.title = element_text(family = "instruction", color = "grey20"),
        axis.line = element_line(color = "grey60"))

