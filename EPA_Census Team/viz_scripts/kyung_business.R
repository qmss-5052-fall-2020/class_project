
library(tidyverse)
library(tidycensus)
library(readxl)
library(plotly)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour="viridis",
  ggplot2.continuous.fill="viridis")

scale_colour_discrete = scale_color_viridis_d
scale_fill_discrete = scale_fill_viridis_d

knitr::opts_chunk$set(comment = NA,
                      message = FALSE,
                      warning = FALSE,
                      echo = FALSE)

Sys.getenv("CENSUS_API_KEY")
readRenviron("~/.Renviron")

county_2015_2019 = 
  read_csv("data_files/county_2015_2019.csv")

est_ind = 
  county_2015_2019 %>% 
  filter(industry != "Unclassified" & industry != "Total, all industries") %>% 
  subset(!str_detect(area, "Unknown Or Undefined")) %>%
  filter(st_name == "Michigan") %>%
  separate(area, into = c("county", "state"), sep = -17) %>% 
  select(-state) %>%
  mutate(year = as.numeric(year)) %>% 
  filter(county == "Manistee") %>% 
  group_by(year, industry) %>% 
  ggplot(aes(x = year,
             y = annual_average_establishment_count,
             fill = industry,
             text = paste("Industry: ", industry,
                          "\nEstablishment: ", annual_average_establishment_count)
  )) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Establishments by Industry in Manistee (2015-2019)",
       x = "Year",
       y = "Number of Establishments")

est_ind_plotly <- ggplotly(est_ind, tooltip = "text")

quotient = 
  county_2015_2019 %>% 
  filter(industry != "Unclassified" & industry != "Total, all industries") %>% 
  subset(!str_detect(area, "Unknown Or Undefined")) %>%
  filter(st_name == "Michigan") %>%
  separate(area, into = c("county", "state"), sep = -17) %>% 
  select(-state) %>%
  mutate(year = as.numeric(year)) %>% 
  filter(county == "Manistee") %>% 
  group_by(year, industry) %>% 
  ggplot(aes(x = year,
             y = employment_location_quotient_relative_to_u_s,
             fill = industry,
             text = paste("Industry: ", industry,
                          "\nLocation Quotient: ", employment_location_quotient_relative_to_u_s)
  )) +
  geom_bar(stat = "identity", color = "black", position = "dodge") +
  labs(title = "Location Quotient by Industry in Manistee (2015-2019)",
       x = "Year",
       y = "Number of Establishments")

quotient_plotly <- ggplotly(quotient, tooltip = "text")

mi_shp =
  get_acs(state = "MI", 
          geography = "county",
          variables = "B19013_001",
          year = 2018,
          geometry = TRUE) %>%
  janitor::clean_names() %>%
  select(geoid, name, geometry) %>%
  separate(name, into = c("county", "state"), sep = -17) %>%
  select(-state)

payroll_state = 
  county_2015_2019 %>% 
  filter(st_name == "Michigan" & year == "2019") %>% 
  separate(area, into = c("county", "state"), sep = -17) %>% 
  select(-state) %>% 
  filter(industry != "Total, all industries") %>% 
  group_by(county) %>% 
  summarize(wage = mean(annual_total_wages/annual_average_employment, na.rm = T))

payroll_plot = 
  left_join(mi_shp, payroll_state, by = "county") %>%
  ggplot(aes(fill = wage, 
             text = paste("County: ", county, "\nWage: $", round(wage))
  )) +
  geom_sf(color = "black") +
  labs(title = "Employee's Annual Wage in Michigan (2019)",
       x = "Longitude",
       y = "Latitude")

payroll_plotly <- ggplotly(payroll_plot, tooltip = "text")

diff_plot =
  county_2015_2019 %>% 
  filter(st_name == "Michigan") %>% 
  separate(area, into = c("county", "state"), sep = -17) %>% 
  select(-state) %>% 
  filter(industry != "Total, all industries" &
           industry != "Unclassified" &
           county == "Manistee") %>% 
  mutate(lag_annual_average_employment = Hmisc::Lag(annual_average_employment, +12),
         employment_diff = lag_annual_average_employment - annual_average_employment) %>% 
  filter(year != "2019") %>% 
  ggplot(aes(x = employment_diff,
             y = industry, 
             fill = industry,
             text = paste("Industry: ", industry, 
                          "\nEmployment Diff: ", employment_diff)
  )) +
  geom_bar(stat = "identity") +
  facet_grid(.~year) + 
  coord_flip() +
  theme(axis.text.x = element_blank()) +
  labs(title = "Net Change in Employeement by Industry in Manistee (2015-2019)",
       x = "Net Change",
       y = "")

diff_plotly <- ggplotly(diff_plot, tooltip = "text")

employee_state =
  county_2015_2019 %>% 
  filter(st_name == "Michigan" & year == "2019") %>% 
  separate(area, into = c("county", "state"), sep = -17) %>% 
  select(-state) %>% 
  filter(industry != "Total, all industries") %>% 
  group_by(county) %>% 
  summarize(employment = sum(annual_average_employment, na.rm = T))

employee_plot = 
  left_join(mi_shp, employee_state, by = "county") %>%
  ggplot(aes(fill = employment, 
             text = paste("County: ", county, "\nEmployee: ", employment)
  )) +
  geom_sf(color = "black") +
  labs(title = "Total Employment in Michigan (2019)",
       x = "Longitude",
       y = "Latitude")

employee_plotly <- ggplotly(employee_plot, tooltip = "text")
