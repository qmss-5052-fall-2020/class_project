
## Visualization #1: Household Income

```{r}
library(tidycensus)
library(tidyverse)
library(gghighlight)

# Michigan household data on county level from ACS 5-year
mi_hhincome <- get_acs(geography = "county", 
                             variables = c(hhincome = "B19013_001"), 
                             year = 2018,
                             survey = "acs5", 
                             state = "MI")

# Rename to remove "County, Michigan"
mi_hhincome2 <- mi_hhincome %>%
  mutate(NAME = str_replace(NAME, " County, Michigan", ""))

# Dot plot
theme_set(theme_bw())

ggplot(mi_hhincome2, 
       aes(x=estimate, 
           y= reorder(NAME, estimate))) + 
  geom_point(size = 2) + 
  scale_x_continuous(labels = scales::dollar) + 
  labs(
    x = "Median Household Income", 
    y = "", 
    title = "Michigan Median Household Income by County") +
  gghighlight(GEOID == 26101, label_key = NAME)+               # highlight County point 
  theme(axis.text.y = element_text(size = 5, vjust=0.6))
```


## Visualization #2: Means of Transportation to Work for Manistee Residents

```{r}

#Variables for Work Commute Transportation
transportwork_vars <- c(Drove_alone = "B08006_003",
                        Carpooled = "B08006_004",
                        Public_transportation = "B08006_008",
                        Bicycle = "B08006_014",
                        Walked = "B08006_015",
                        Other_means = "B08006_016",
                        Worked_at_home = "B08006_017")

#Pull data for Work Commute Transportation for Michigan
mi_transportwork <- get_acs(geography = "county", 
                            state = "MI",
                            variables = transportwork_vars,
                            summary_var = "B08006_001")

#Calculate as percentages
mi_transportwork_pct <- mi_transportwork %>%
  mutate(pct = (estimate/ summary_est)) %>%
  select(GEOID, NAME, variable, pct)

#Subset for Manistee
manistee_transportwork_pct <- subset(mi_transportwork_pct, GEOID==26101)
manistee_transportwork_pct

#Bar graph Manistee data
ggplot(manistee_transportwork_pct, aes(x = reorder(variable, pct), y = pct, label = scales::percent(pct))) +
  geom_col() + 
  geom_text(position = position_dodge(width = .9), #move to center of bars
            vjust = -0.5, #nudge above top of bar
            size = 3) +
  labs(
    x = "Means of Transporation to Work", 
    y = "Percent", 
    title = "Manistee County Means of Transportation to Work")

```



