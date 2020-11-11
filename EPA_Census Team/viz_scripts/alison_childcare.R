
library(tidyverse)
library(tidycensus)
library(ggplot2)
library(plotly)

Sys.getenv("CENSUS_API_KEY")

state = "26"
county = "101"
survey = "acs5"
year = 2018


# Table B11001: Household Type (Including Living Alone): https://censusreporter.org/tables/B11001/

B11001_vars = c(total = "B11001_001E", 
                t_family = "B11001_002E", 
                tf_married_couple = "B11001_003E",
                tf_other = "B11001_004E",
                tfo_male = "B11001_005E",
                tfo_female = "B11001_006E",
                t_nonfamily = "B11001_007E",
                tn_alone = "B11001_008E",
                tn_not_alone = "B11001_009E")

B11001_label = B11001_vars %>%
  enframe() %>%
  select(name) %>%
  unlist()

B11001 <- get_acs(geography = "county",
                  variables = B11001_vars,
                  state = state,
                  county = county,
                  survey = survey,
                  year = year)

household_type <- B11001 %>%
  mutate(label = B11001_label) %>%
  mutate(estimate_scaled = (estimate/9591)) %>%
  select(GEOID, NAME, variable, label, estimate_scaled, estimate, moe) %>%
  slice(3, 4, 8, 9) %>%
  mutate(label = case_when(label == "tf_married_couple" ~ "Lives with spouse",
                           label == "tf_other" ~ "Lives with other family",
                           label == "tn_alone" ~ "Lives alone",
                           label == "tn_not_alone" ~ "Lives with non-family")) %>%
  mutate(label = factor(label, levels = c("Lives with spouse", "Lives with other family", "Lives with non-family", "Lives alone")))

household_plot <- ggplot(data = household_type, aes(x = label, y = estimate_scaled)) + 
  geom_bar(stat = "identity") +
  ggtitle("Household type") +
  labs(caption = "Source: 2018 ACS 5-year survey")

household_plotly <- ggplotly(household_plot)


# Table B09005: Household Type for Children in Households: https://censusreporter.org/tables/B09005/

B09005_vars = c(total = "B09005_001E", 
                t_family = "B09005_002E", 
                tf_married_couple = "B09005_003E",
                tf_male = "B09005_004E",
                tf_female = "B09005_005E",
                t_nonfamily = "B09005_006E")

B09005_label = B09005_vars %>%
  enframe() %>%
  select(name) %>%
  unlist()

B09005 <- get_acs(geography = "county",
                  variables = B09005_vars,
                  state = state,
                  county = county,
                  survey = survey,
                  year = year)

household_type_children <- B09005 %>%
  mutate(label = B09005_label) %>%
  mutate(estimate_scaled = (estimate/4298)) %>%
  select(GEOID, NAME, variable, label, estimate_scaled, estimate, moe) %>%
  slice(3:6) %>%
  mutate(label = case_when(label == "tf_married_couple" ~ "Lives with married couple",
                           label == "tf_male" ~ "Lives with male householder",
                           label == "tf_female" ~ "Lives with female householder",
                           label == "t_nonfamily" ~ "Lives with non-family")) %>%
  mutate(label = factor(label, levels = c("Lives with married couple", "Lives with male householder", "Lives with female householder", 
                                          "Lives with non-family")))

children_plot <- ggplot(data = household_type_children, aes(x = label, y = estimate_scaled)) + 
  geom_bar(stat = "identity") +
  ggtitle("Type of households that children live in") +
  labs(caption = "Source: 2018 ACS 5-year survey")

children_plotly <- ggplotly(children_plot)


# Table B10001: Grandchildren Living With a Grandparent Householder by Age of Grandchild: https://censusreporter.org/tables/B10001/

B10001_vars = c(total = "B10001_001E", 
                t_under_6 = "B10001_002E", 
                t_6_to_11 = "B10001_003E",
                t_12_to_17 = "B10001_004E")

B10001_label = B10001_vars %>%
  enframe() %>%
  select(name) %>%
  unlist()

B10001 <- get_acs(geography = "county",
                  variables = B10001_vars,
                  state = state,
                  county = county,
                  survey = survey,
                  year = year)

grandchildren <- B10001 %>%
  mutate(label = B10001_label) %>%
  mutate(estimate_scaled = (estimate/327)) %>%
  select(GEOID, NAME, variable, label, estimate_scaled, estimate, moe) %>%
  slice(2:4) %>%
  mutate(label = case_when(label == "t_under_6" ~ "Under 6 years old",
                           label == "t_6_to_11" ~ "6 to 11 years old",
                           label == "t_12_to_17" ~ "12 to 17 years old")) %>%
  mutate(label = factor(label, levels = c("Under 6 years old", "6 to 11 years old", "12 to 17 years old")))

grandchildren_plot <- ggplot(data = grandchildren, aes(x = label, y = estimate_scaled)) + 
  geom_bar(stat = "identity") +
  ggtitle("Children living with grandparents") +
  labs(caption = "Source: 2018 ACS 5-year survey")

grandchildren_plotly <- ggplotly(grandchildren_plot)


# Table B10002: Grandchildren Living With a Grandparent Householder by Grandparent Responsibility and Presence of Parent: 
#      https://censusreporter.org/tables/B10002/

B10002_vars = c(total = "B10002_001E", 
                t_grandparent_responsible = "B10002_002E", 
                tgr_parent = "B10002_003E",
                tgr_no_parent = "B10002_004E",
                t_grandparent_not_responsible = "B10002_005E")

B10002_label = B10002_vars %>%
  enframe() %>%
  select(name) %>%
  unlist()

B10002 <- get_acs(geography = "county",
                  variables = B10002_vars,
                  state = state,
                  county = county,
                  survey = survey,
                  year = year)

grandparents <- B10002 %>%
  mutate(label = B10002_label) %>%
  mutate(estimate_scaled = (estimate/327)) %>%
  select(GEOID, NAME, variable, label, estimate_scaled, estimate, moe) %>%
  slice(3:5) %>%
  mutate(label = case_when(label == "tgr_parent" ~ "Parent present",
                           label == "tgr_no_parent" ~ "Parent not present",
                           label == "t_grandparent_not_responsible" ~ "Grandparent not responsible")) %>%
  mutate(label = factor(label, levels = c("Parent present", "Parent not present", "Grandparent not responsible")))

grandparents_plot <- ggplot(data = grandparents, aes(x = label, y = estimate_scaled)) + 
  geom_bar(stat = "identity") +
  ggtitle("Grandparent responsibility for children") +
  labs(caption = "Source: 2018 ACS 5-year survey")

grandparents_plotly <- ggplotly(grandparents_plot)
