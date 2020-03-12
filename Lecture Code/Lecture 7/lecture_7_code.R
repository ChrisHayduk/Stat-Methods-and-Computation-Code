####################################
##                                ##
##   SDGB-7844 - Lecture 6 Code   ##
##   Visualization                ##
##   Prof. Matthew Murphy         ##
##                                ##
####################################

# there are many libraries to make 
# working with maps easier in R
# install the packages below
# install.packages(c('maptools', 'RColorBrewer', 'rgdal', 'sp'))

library(tidyverse)
library(maptools)
library(RColorBrewer)
library(rgdal)
library(sp)

# read ACS data into R
med_home_value <- read_csv("acs_home_value.csv")

# read in shape files
usa_map <- readOGR(dsn="states_21basic", layer="states")

# view shapefile
plot(usa_map)

# use @ command to access a 'slot' in the R object
# @ is just a special case of '$' for list objects
usa_map@data
usa_map@polygons


# let's simply look at contiguous USA
med_home_value <- filter(med_home_value, NAME != "Hawaii", NAME != "Alaska")


# We will also need to remove these polygons 
# from our map object before plotting
# FIPS
# Alaska --> "02"
# Hawaii --> "15"
remove_fips <- c("02", "15")

# we'll use the row names to remove unwanted states
row.names(usa_map) <- as.character(usa_map$STATE_FIPS)

# remove Alaska and Hawaii from map object:
usa_map <- usa_map[!is.element(row.names(usa_map), remove_fips),]

# view updated map
plot(usa_map)


# in order to join 
# the map object to the data
# we will need to join on a common field
# in this case, it is the FIPS code
colnames(med_home_value)[1] <- "STATE_FIPS"

# the sp package has a nice merge feature
# that we can use on map data
# you MUST ensure that your data matches with the map object exactly!!!
sp_home <- sp::merge(usa_map, med_home_value, by="STATE_FIPS")


range(med_home_value$estimate, na.rm=TRUE)
# $105700 to $538400
# don’t want 400,000 different colors on this map!

# break up range into equal length intervals
# other options: by quintile, decile, etc.
# say we want to use 10 colors -- integer valued endpoints
seq(from=105600, to=506200, length=10)

# 0 to 9 is the first group, (0, 9]
# 9 to 18 is second group, (9, 18]
# and so forth
# (0,9]  --> from 0 to 9, including 9 but not including 0
# variable to color code map by: [map_object]@data$[variable_name]
sp_home@data  # total is a column in this data frame
plot_variable <- sp_home@data$estimate

# cut 
breaks_factor <- cut(plot_variable, breaks=seq(from=105600, to=506200, length=10))
levels(breaks_factor)   # list of categories
plot_variable[1:10]     # we can check whether the categorization was
breaks_factor[1:10]     # done correctly
table(breaks_factor)    

# number of factor levels ---> number of different colors
length(levels(breaks_factor))

# color palette
color_palette <- brewer.pal(n=length(levels(breaks_factor)),"Spectral")
color_coding <- color_palette[as.numeric(breaks_factor)]

# RColorBrewer has some great default pallettes
# Another great packages is "viridis" which has similar features
display.brewer.pal(length(levels(breaks_factor)), "Spectral")

plot(sp_home, # plot our combined map / data object
     col=color_coding, # color coding for our data
     main="American Community Survey (2016):\nMedian Value of Owner-Occupied Housing Units ($M)")

# creating a legend for this plot extremely important!
# let's edit our legend categories to data/1000
breaks_factor_legend <- cut(plot_variable/1000, breaks=seq(from=105600/1000, to=506200/1000, length=10))

legend("bottomleft", legend=levels(breaks_factor_legend),
       fill=color_palette, cex=0.7, bty="n", y.intersp=1.2, ncol=2)
# can see skewness on this plot!



#########################
# tidycensus (census api)
#########################

library(tidycensus)
library(ggplot2)
library(leaflet)
library(stringr)
library(sf)

census_api_key("68009a7ec40d6be0a273f14a8a8920e924062829", install = TRUE)

#Get variable names from 2016 ACS
v15 <- load_variables(2016, "acs5", cache = TRUE)

#B19013_001 is the ID for median income for household

#Get data
data <- get_acs(geography = "tract", variables = c(medincome = "B19013_001"), state = "NY", county = "New York County", geometry = TRUE)

#Remove missing data
data <- data[!is.na(data$estimate) & !is.na(data$moe),]

#Range is [12903, 235083]
range(data$estimate)

#Plot histogram of estimates
ggplot(data, aes(x=estimate)) + geom_histogram()

#Scatter plot of estimate vs. moe
ggplot(data, aes(x=estimate, y=moe)) + geom_point()

pal <- colorQuantile(palette = "viridis", domain = data$estimate, n = 10)

data %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ paste0(str_extract(NAME, "^([^,]*)"), "<br>", "Estimate: ", estimate, "<br>", "MoE: ", moe),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(estimate)) %>%
  addLegend("bottomright", 
            pal = pal,
            values = ~ estimate,
            title = "Income percentiles",
            opacity = 1)
