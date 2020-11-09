# Data you need to plot is in this folder
# See "Data Manipulation" folder for OG data & manipulation code" 

library(tidyverse)
library(ggplot2)
library(scales)

just_sex <- read.csv("data_files/just_sex")
age_breakdown_manistee <- read.csv("data_files/age_breakdown_manistee")
race_data_manistee <- read.csv("data_files/race_data_manistee")

# create "Blank_theme" to remove all the axis markings for pie chart 
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.text.x=element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

# Create Chart (in ggplot2 you have to make a bar chart first, then change "coordinates" to make it into a pie)
genderbar<- ggplot(just_sex, aes(x="", y=`Manistee.County..MI`, fill=`label`))+
  geom_bar(width = 1, stat = "identity")
genderpie <- genderbar + coord_polar("y", start=0) +
  geom_text(aes(y = `Manistee.County..MI`/3 + c(0, cumsum(`Manistee.County..MI`)[-length(`Manistee.County..MI`)]), 
                label = percent(`Manistee.County..MI`/sum(`Manistee.County..MI`), accuracy=1)), size=5) + 
  ggtitle(label = "Sex") +
  scale_fill_discrete(name = "", labels = c("Female", "Male")) +
  blank_theme
genderpie 

#plot
agebar <- ggplot(age_breakdown_manistee, aes(x= `NewCat`, y=`Per`)) + 
  geom_bar(width = .8, stat = "identity") + 
  labs(title="Population by Age Range for Manistee County") + 
  xlab("Age Range") + ylab("Percent of Population")
agebar

# Note: "Hispanic" is included in 2+ category and "hispanic" category, but not others 
racebar <- ggplot(race_data_manistee, aes(x= `CompRace`, y=as.numeric(`Per` * 100))) + 
  geom_bar(width = .8, stat = "identity") + 
  scale_y_continuous(limits = c(0,90), expand = c(0, 0)) +
  labs(title="Race & Ethnicity for Manistee County") + 
  xlab("Race") + ylab("Percent of Population")
racebar
