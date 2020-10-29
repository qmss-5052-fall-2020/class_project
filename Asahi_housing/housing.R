library(data.table)
library(dplyr)
library(extrafont)
library(ggplot2)
library(ggpubr)
library(sf)
library(tidycensus)

loadfonts()

api_key <- Sys.getenv("CENSUS_API_KEY")
census_api_key(api_key, install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

vars_acs5_2018 <- load_variables(year = 2018, dataset = "acs5")
rent_vars <- vars_acs5_2018[vars_acs5_2018$name %like% "B25063", ][3:26,]
rent_thresholds <- regmatches(rent_vars$label, gregexpr("[[:digit:]]+", rent_vars$label))

rent_threshold_labels <- c()
for (i in c(1:length(rent_thresholds))) {
  length <- length(rent_thresholds[[i]])
  label <- ""
  if ((length) == 1) {
    rent_threshold_labels[i] <- paste("less than $",rent_thresholds[[i]], sep = "")
  }
  else if ((length) == 2) {
    if (as.numeric(rent_thresholds[[i]][1]) < 100) {
      rent_threshold_labels[i] <- paste("greater than $",rent_thresholds[[i]][1],",",rent_thresholds[[i]][2], sep = "")
    }
    else {
      rent_threshold_labels[i] <- paste("$",rent_thresholds[[i]][1]," to $",rent_thresholds[[i]][2], sep = "")
    }
  }
  else {
    rent_threshold_labels[i] <- paste("$",rent_thresholds[[i]][1],",",rent_thresholds[[i]][2]," to ","$",rent_thresholds[[i]][3],",",rent_thresholds[[i]][4], sep = "")
  }
}

manistee_rent_tract <- get_acs(geography = "tract",
                               state = "MI",
                               county = "Manistee",
                               variables = rent_vars$name,
                               year = 2018,
                               output = "wide",
                               geometry = TRUE)
manistee_rent_county <- get_acs(geography = "county",
                                state = "MI",
                                county = "Manistee",
                                variables = rent_vars$name,
                                year = 2018,
                                output = "wide",
                                geometry = TRUE)

county_rents <- manistee_rent_county[ ,grepl("B.*E|E.*B" ,names(manistee_rent_county))]
st_geometry(county_rents) <- NULL

county_rents_DF <- data.frame(thresholds = rent_threshold_labels,
                              counts = as.numeric(as.vector(county_rents[1,])))
county_rents_DF$thresholds <- factor(county_rents_DF$thresholds, levels = county_rents_DF$thresholds)

font <- "Arial"
counts_plot <- ggplot(data=county_rents_DF, aes(x=thresholds, y=counts)) +
  ggtitle("Monthly Rent in Manistee County (2018 ACS5)") +
  xlab("Monthly Rent Ranges") +
  ylab("Households") +
  geom_bar(stat="identity") +
  theme(
    rect = element_rect(fill = "transparent"),
    legend.background = element_rect(fill = NA),
    legend.key = element_rect(fill = NA, color = NA),
    legend.position = "bottom",
    legend.title = element_text(color = "white", face = "bold", family = font),
    legend.text = element_text(color = "white", face = "bold", family = font),
    panel.background = element_rect(fill = NA),
    plot.background = element_rect(fill = NA, color = NA),
    panel.grid = element_blank(),
    # axis.title = element_text(color = "black", face = "bold", family = font)
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle=90,hjust=1),
    axis.ticks.x = element_line(color = "#495155")
  )
counts_plot

rent_perc_plot <- ggplot(data=county_rents_DF, aes(x=thresholds, y=100*counts/sum(county_rents_DF$counts))) +
  ggtitle("Monthly Rent in Manistee County (2018 ACS5)") +
  xlab("Monthly Rent Ranges") +
  ylab("Percent of Households") +
  geom_bar(stat="identity") +
  theme(
    rect = element_rect(fill = "transparent"),
    legend.background = element_rect(fill = NA),
    legend.key = element_rect(fill = NA, color = NA),
    legend.position = "bottom",
    legend.title = element_text(color = "white", face = "bold", family = font),
    legend.text = element_text(color = "white", face = "bold", family = font),
    panel.background = element_rect(fill = NA),
    plot.background = element_rect(fill = NA, color = NA),
    panel.grid = element_blank(),
    # axis.title = element_text(color = "black", face = "bold", family = font)
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle=90,hjust=1),
    axis.ticks.x = element_line(color = "#495155")
  )
rent_perc_plot

michigan_rent <- get_acs(geography = "state",
                         state = "MI",
                         variables = rent_vars$name,
                         year = 2018,
                         output = "wide",
                         geometry = TRUE)

michigan_rents_est <- michigan_rent[ ,grepl("B.*E|E.*B" ,names(michigan_rent))]
st_geometry(michigan_rents_est) <- NULL

michigan_rents_est_DF <- data.frame(thresholds = rent_threshold_labels,
                              counts = as.numeric(as.vector(michigan_rents_est[1,])))
michigan_rents_est_DF$thresholds <- factor(michigan_rents_est_DF$thresholds, levels = michigan_rents_est_DF$thresholds)

michigan_counts_plot <- ggplot(data=michigan_rents_est_DF, aes(x=thresholds, y=counts)) +
  ggtitle("Monthly Rent in Michigan (2018 ACS5)") +
  xlab("Monthly Rent Ranges") +
  ylab("Households") +
  geom_bar(stat="identity") +
  theme(
    rect = element_rect(fill = "transparent"),
    legend.background = element_rect(fill = NA),
    legend.key = element_rect(fill = NA, color = NA),
    legend.position = "bottom",
    legend.title = element_text(color = "white", face = "bold", family = font),
    legend.text = element_text(color = "white", face = "bold", family = font),
    panel.background = element_rect(fill = NA),
    plot.background = element_rect(fill = NA, color = NA),
    panel.grid = element_blank(),
    # axis.title = element_text(color = "black", face = "bold", family = font)
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle=90,hjust=1),
    axis.ticks.x = element_line(color = "#495155")
  )
michigan_counts_plot

michigan_perc_plot <- ggplot(data=michigan_rents_est_DF, aes(x=thresholds, y=100*counts/sum(michigan_rents_est_DF$counts))) +
  ggtitle("Monthly Rent in Michigan (2018 ACS5)") +
  xlab("Monthly Rent Ranges") +
  ylab("Percent of Households") +
  geom_bar(stat="identity") +
  theme(
    rect = element_rect(fill = "transparent"),
    legend.background = element_rect(fill = NA),
    legend.key = element_rect(fill = NA, color = NA),
    legend.position = "bottom",
    legend.title = element_text(color = "white", face = "bold", family = font),
    legend.text = element_text(color = "white", face = "bold", family = font),
    panel.background = element_rect(fill = NA),
    plot.background = element_rect(fill = NA, color = NA),
    panel.grid = element_blank(),
    # axis.title = element_text(color = "black", face = "bold", family = font)
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle=90,hjust=1),
    axis.ticks.x = element_line(color = "#495155")
  )
michigan_perc_plot

# compare manistee and michigan
# add geo as z var
perc_county_rents_DF <- county_rents_DF
perc_county_rents_DF$geo <- "Manistee County"
perc_county_rents_DF$perc <- perc_county_rents_DF$counts/sum(perc_county_rents_DF$counts)
perc_michigan_rents_est_DF <- michigan_rents_est_DF
perc_michigan_rents_est_DF$geo <- "Michigan"
perc_michigan_rents_est_DF$perc <- perc_michigan_rents_est_DF$counts/sum(perc_michigan_rents_est_DF$counts)
# combine vertically
compare_perc_manistee_michigan_DF <- rbind(perc_county_rents_DF,perc_michigan_rents_est_DF)

compare_manistee_michigan_perc_plot <- ggplot(data=compare_perc_manistee_michigan_DF, aes(x=thresholds, y=100*perc, fill = geo)) +
  ggtitle("Monthly Rent in Michigan vs Manistee County (2018 ACS5)") +
  xlab("Monthly Rent Ranges") +
  ylab("Percent of Households") +
  geom_bar(stat="identity",position ="dodge") +
  theme(
    rect = element_rect(fill = "transparent"),
    legend.background = element_rect(fill = NA),
    legend.key = element_rect(fill = NA, color = NA),
    legend.position = "bottom",
    legend.title = element_text(color = "white", face = "bold", family = font),
    legend.text = element_text(color = "black", face = "bold", family = font),
    panel.background = element_rect(fill = NA),
    plot.background = element_rect(fill = NA, color = NA),
    panel.grid = element_blank(),
    # axis.title = element_text(color = "black", face = "bold", family = font)
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle=90,hjust=1),
    axis.ticks.x = element_line(color = "#495155")
  )
compare_manistee_michigan_perc_plot

# tract distributions
manistee_rent_tract_est <- manistee_rent_tract[ ,grepl("E" ,names(manistee_rent_tract))]
st_geometry(manistee_rent_tract_est) <- NULL

# median rent by tract
manistee_median_rent_tract <- get_acs(geography = "tract",
                               state = "MI",
                               county = "Manistee",
                               variables = c("B25031_001"),
                               year = 2018,
                               output = "wide",
                               geometry = TRUE)

ggplot(data = manistee_median_rent_tract) + 
  ggtitle("Monthly Rent in Manistee County (2018 ACS5)") +
  labs(fill = "Median Rent") +
  geom_sf(color = "black", aes(fill= B25031_001E)) +
  theme(
    rect = element_rect(fill = "transparent"),
    panel.background = element_rect(fill = NA),
    plot.background = element_rect(fill = NA, color = NA),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )

# income and rent dist side-by-side
income_vars <- vars_acs5_2018[vars_acs5_2018$name %like% "B19001", ][2:17,]

manistee_income_county <- get_acs(geography = "county",
                                state = "MI",
                                county = "Manistee",
                                variables = income_vars$name,
                                year = 2018,
                                output = "wide",
                                geometry = TRUE)

income_thresholds <- regmatches(income_vars$label, gregexpr("[[:digit:]]+", income_vars$label))
income_threshold_labels <- c()
for (i in c(1:length(income_thresholds))) {
  length <- length(income_thresholds[[i]])
  label <- ""
  if ((length) == 2) {
    if (i == 1) {
      income_threshold_labels[i] <- paste("less than $",income_thresholds[[i]][1],",",income_thresholds[[i]][2], sep = "")
    }
    else {
      income_threshold_labels[i] <- paste("greater than $",income_thresholds[[i]][1],",",income_thresholds[[i]][2], sep = "")
    }
  }
  else {
    income_threshold_labels[i] <- paste("$",income_thresholds[[i]][1],",",income_thresholds[[i]][2]," to ","$",income_thresholds[[i]][3],",",income_thresholds[[i]][4], sep = "")
  }
}

county_income <- manistee_income_county[ ,grepl("B.*E|E.*B" ,names(manistee_income_county))]
st_geometry(county_income) <- NULL

county_income_DF <- data.frame(thresholds = income_threshold_labels,
                              counts = as.numeric(as.vector(county_income[1,])))
county_income_DF$thresholds <- factor(county_income_DF$thresholds, levels = county_income_DF$thresholds)

income_perc_plot <- ggplot(data=county_income_DF, aes(x=thresholds, y=100*counts/sum(county_income_DF$counts))) +
  ggtitle("Yearly Income in Manistee County (2018 ACS5)") +
  xlab("Income Ranges") +
  ylab("Percent of Households") +
  geom_bar(stat="identity") +
  theme(
    rect = element_rect(fill = "transparent"),
    legend.background = element_rect(fill = NA),
    legend.key = element_rect(fill = NA, color = NA),
    legend.position = "bottom",
    legend.title = element_text(color = "white", face = "bold", family = font),
    legend.text = element_text(color = "white", face = "bold", family = font),
    panel.background = element_rect(fill = NA),
    plot.background = element_rect(fill = NA, color = NA),
    panel.grid = element_blank(),
    # axis.title = element_text(color = "black", face = "bold", family = font)
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle=90,hjust=1),
    axis.ticks.x = element_line(color = "#495155")
  )
income_perc_plot

income_rent_county_plot <- ggarrange(income_perc_plot, rent_perc_plot, 
          ncol = 1, nrow = 2)
income_rent_county_plot
