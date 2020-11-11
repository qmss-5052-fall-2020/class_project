library(tidycensus)
library(tidyverse)
library(ggplot2)
library(plotly)
library(dplyr)

#Data from ACS5 2018 table B15003 - Education level totals for population over 25

api_key <- Sys.getenv("CENSUS_API_KEY")
census_api_key(api_key, overwrite = TRUE, install = TRUE)

readRenviron("~/.Renviron")


acs5_2018_vars <- load_variables(2018, "acs5", cache = TRUE)


#pulling data for Manistee Education
manistee_educ2 <- get_acs(geography = "county",
                         state = "MI",
                         county = "Manistee",
                         table =  'B15003',
                         year = 2018,
                         output = "wide",
                         geometry = TRUE
)


#EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER">
#adding columns for different education level totals
manistee_educ2$no_degree <- manistee_educ2$B15003_002E + manistee_educ2$B15003_003E + manistee_educ2$B15003_004E + manistee_educ2$B15003_005E + manistee_educ2$B15003_006E +  
                            manistee_educ2$B15003_007E + manistee_educ2$B15003_008E + manistee_educ2$B15003_009E + manistee_educ2$B15003_010E +
                            manistee_educ2$B15003_011E + manistee_educ2$B15003_012E + manistee_educ2$B15003_013E + manistee_educ2$B15003_014E +
                            manistee_educ2$B15003_015E + manistee_educ2$B15003_016E
manistee_educ2$high_school <- manistee_educ2$B15003_017E + manistee_educ2$B15003_018E
manistee_educ2$some_college <- manistee_educ2$B15003_019E + manistee_educ2$B15003_020E
manistee_educ2$associates <- manistee_educ2$B15003_021E
manistee_educ2$bachelors <- manistee_educ2$B15003_022E
manistee_educ2$post_grad <- manistee_educ2$B15003_023E + manistee_educ2$B15003_024E + manistee_educ2$B15003_025E
manistee_educ2$totalpop <- manistee_educ2$B15003_001E

manistee_educ <- subset(manistee_educ2, select =  c(GEOID, NAME,no_degree, high_school, some_college, associates, bachelors, post_grad,totalpop))
manistee_educ_pct<- manistee_educ %>% mutate(no_degree_pct = (no_degree/ totalpop),
                                                high_school_pct = (high_school/ totalpop),
                                                some_college_pct = (some_college/ totalpop),
                                                associates_pct = (associates/ totalpop),
                                                bachelors_pct = (bachelors/ totalpop),
                                                post_grad_pct = (post_grad/ totalpop)
)

manistee_educ_pct <- manistee_educ_pct %>% select("no_degree_pct", "high_school_pct", "some_college_pct", "associates_pct", "bachelors_pct", "post_grad_pct")
manistee_educ_pct<- as.data.frame(t(manistee_educ_pct))
manistee_educ_pct$education_level = row.names(manistee_educ_pct)
manistee_educ_pct <- rename(manistee_educ_pct, c("1" = "pct"))
manistee_educ_pct <- manistee_educ_pct %>% filter(education_level != "geometry")
manistee_educ_pct$education_level <- factor(manistee_educ_pct$education_level, levels = c("no_degree_pct", "high_school_pct", "some_college_pct", "associates_pct", "bachelors_pct", "post_grad_pct"))

educ_attainment_plot <- ggplot(data = manistee_educ_pct, aes(x = education_level, y=(as.numeric(pct)*100))) + 
  ggtitle("Educational Attainment Level for Manistee", "Population 25 years and over") +
  geom_bar(stat= "identity")+
  labs(
    x = "Educational Attatinment",
    y = "Percent"
  ) + 
  scale_x_discrete(labels=c("no_degree_pct" = "No Degree", "high_school_pct" = "High School/GED", "some_college_pct" = "Some College", "associates_pct" = "Associate's Degree", "bachelors_pct" = "Bachelor's Degree", "post_grad_pct" = "Post Graduate Degree"))
                                 

educ_attainment_plot

ggplotly(educ_attainment_plot)
