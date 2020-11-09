## Household median income ##
# Michigan household data on county level from ACS 5-year
mi_hhincome <- get_acs(geography = "county", 
                       variables = c(hhincome = "B19013_001"), 
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
  gghighlight(GEOID == 26101, label_key = NAME) +  # highlight County point 
  theme(axis.text.y = element_text(size = 5, vjust=0.6))



## Mode of transport for commute ##
#Variables for Work Commute Transportation
transportwork_vars <- c("Drove alone" = "B08006_003",
                        "Carpooled" = "B08006_004",
                        "Public transportation" = "B08006_008",
                        "Bicycle" = "B08006_014",
                        "Walked" = "B08006_015",
                        "Other means" = "B08006_016",
                        "Worked at home" = "B08006_017")

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



## HOUSING - Occupied vs Vacant ##
mi_vacancy_vars <- c("Vacant" = "B25002_003",
                     "Occupied" = "B25002_002")

mi_vacancy <- get_acs(geography = "county", 
                       variables = mi_vacancy_vars, 
                       survey = "acs5", 
                       state = "MI",
                       summary_var = "B25002_001")

#Calculate as percentages
manistee_vacancy_pct <- mi_vacancy %>%
  mutate(pct = (estimate/ summary_est)) %>%
  select(GEOID, NAME, variable, pct)

#Bar graph 
ggplot(manistee_vacancy_pct, aes(x = variable, y = pct, label = scales::percent(pct))) +
  geom_col() + 
  geom_text(position = position_dodge(width = .9), #move to center of bars
            vjust = -0.5, #nudge above top of bar
            size = 3)

# still trying to figure out how to add Michigan for comparison




## Price of occupied houses ##
mi_houseprice <- get_acs(geography = "county",
                         state = "MI",
                         table = "B25075",
                         summary_var = "B25075_001")

#calculate housing prices as percentage of total stock
mi_houseprice_pct <- mi_houseprice %>%
  mutate(pct = (estimate/ summary_est)) %>%
  select(GEOID, NAME, variable, pct)

#group 
mi_houseprice_grouped <- mi_houseprice_pct %>%
  filter(variable != "B25075_001") %>%
  mutate(incgroup = case_when(variable < "B25075_010" ~ "<$49K",
                              variable < "B25075_015" ~ "$50K - $99K",
                              variable < "B25075_019" ~ "$100K - $199K",
                              variable < "B25075_021" ~ "$200K - $299K",
                              variable < "B25075_022" ~ "$300K - $399K",
                              variable < "B25075_023" ~ "$400K - $499K",
                              variable < "B25075_024" ~ "$500K - $749K",
                              variable < "B25075_025" ~ "$750K - $1M",
                              TRUE ~ "$1M+")) %>%
  group_by(NAME, incgroup, GEOID) %>%
  summarize(group_est = sum(pct))

#subset for Manistee
manistee_houseprice <- subset(mi_houseprice_grouped, GEOID==26101)

#Bar graph 
ggplot(manistee_houseprice, aes(x = incgroup, y = group_est, label = scales::percent(group_est))) +
  geom_col() + 
  geom_text(position = position_dodge(width = .9), #move to center of bars
            vjust = -0.5, #nudge above top of bar
            size = 3)



## Housing Type of structure ##
mi_resstruc <- get_acs(geography = "county",
                       state = "MI",
                       table = "B25024",
                       summary_var = "B25024_001")

#percentage
mi_resstruc_pct <- mi_resstruc %>%
  mutate(pct = (estimate/ summary_est)) %>%
  select(GEOID, NAME, variable, pct)

#group
mi_resstruc_grouped <- mi_resstruc_pct %>%
  filter(variable != "B25024_001") %>%
  mutate(incgroup = case_when(variable < "B25024_004" ~ "Single",
                              variable < "B25024_010" ~ "Multifamily",
                              variable < "B25024_011" ~ "Mobile Home",
                              TRUE ~ "Boat, RV, Van, etc")) %>%
  group_by(NAME, incgroup, GEOID) %>%
  summarize(group_est = sum(pct))

#subset for county
manistee_resstruc <- subset(mi_resstruc_grouped, GEOID==26101)

#Bar graph 
ggplot(manistee_resstruc, aes(x = incgroup, y = group_est, label = scales::percent(group_est))) +
  geom_col() + 
  geom_text(position = position_dodge(width = .9), #move to center of bars
            vjust = -0.5, #nudge above top of bar
            size = 3)
