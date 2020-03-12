####################################
##                                ##
##   SDGB-7844 - Lecture 6 Code   ##
##   Visualization                ##
##   Prof. Matthew Murphy         ##
##                                ##
####################################

library(tidyverse)

# read data in to R
listings_clean <- read_csv("airbnb_listings_clean.csv")

# let's remove outliers for plotting
listings_clean <- subset(listings_clean, price < 1000) 

#########################
# Histogram & Box Plots
#########################

par(mfrow=c(1,2)) # figure with 2 plots: (1 row, 2 columns)

# histogram of listing price
hist(listings_clean$price, main = "Histogram: \nAirbnb Listing Price", 
     xlab = "Price ($)", ylab = "Frequency", 
     col = "firebrick", density = 75,
     angle = 50, border = "black")
# \n indicates new line, just like \t was tab

# box plot of listing price
boxplot(listings_clean$price, las=TRUE, col="blue", 
        horizontal=TRUE, pch=19,
        main="Boxplot: \nAirbnb Listing Price ",
        xlab="Price ($)")
# argument horizontal=FALSE for vertical box plot (default)
# argument pch specifies dot shape (default is 1, see ?points for details)

#########################
# Grouped Data
# Box Plots (cont.)
# Bar Plots
#########################

# 1 rows, 2 columns of plots already set from par(mfrow=c(1,2))
# when we made the histogram and box plot
# side-by-side box plot of listing price by borough
boxplot(listings_clean$price ~ listings_clean$neighbourhood_group, las=TRUE,
        main="Airbnb Listing Prices \n By NYC Borough",
        xlab="Borough", ylab="Price", pch=19, cex.axis=0.9)

# cex.axis, cex.main, cex.lab: changes font size of axis,
#     title, and axis labels
# bar graph of number of listings per borough
barplot(sort(table(listings_clean$neighbourhood_group)), las=TRUE, col="firebrick",
        main="Airbnb Listings per Borough", cex.names=0.9,
        xlab="Borough", ylab="number of listings")
# can change font size, type, etc. see ?par for details

#########################
# Scatter Plots
#########################

dev.off() # resets the plotting parameters
plot(log(listings_clean$price), listings_clean$reviews_per_month) # hard to read

# color code by borough
color.vector <- rep("cadetblue", times=nrow(listings_clean))  # Manhattan
color.vector[listings_clean$neighbourhood_group=="Brooklyn"] <- "limegreen"
color.vector[listings_clean$neighbourhood_group=="Queens"] <- "firebrick"
color.vector[listings_clean$neighbourhood_group=="Bronx"] <- "black"
color.vector[listings_clean$neighbourhood_group=="Staten Island"] <- "purple"

plot(listings_clean$price, listings_clean$reviews_per_month, main="Price by Reviews Per Month", col=color.vector,
     xlab="Price ($)", ylab="Reviews Per Month", pch=1, las=TRUE)
legend("topright", legend=names(table(listings_clean$neighbourhood_group)), pch=1, bty="n",
       col=c("black", "limegreen", "cadetblue", "firebrick", "purple"))

# location: coordinates or "topright", "topleft", "bottomleft", or "bottomright"
# bty="n" specifies no box around the legend
# ncol, horizontal : orientation of legend
# fill, lty, led, etc.: type of line/point/box (can mix types)

#########################
# Scatter Plot Matrix
#########################

# note: can add other plotting parameters to
#       make plot look better
pairs(listings_clean[,c("price", "reviews_per_month", "minimum_nights")])


#########################
# Time Series
#########################

z <- rnorm(10)        # simulate 10 draws from standard normal
par(mfrow=c(1,3))
plot(z, las=TRUE)               # just points
plot(z, type="l", las=TRUE)     # line
plot(z, type="b", las=TRUE)     # lines and points



#########################
# ggplot2
#########################


library(ggplot2)
# let's recreate our scatterplot
# price vs. reviews by borough

#main plotting functions
ggplot(data = listings_clean) + 
  
  # there are many geom_type functions:
  # geom_hist, geom_point, geom_line...
  geom_point(mapping = 
               
               #aesthetic mapping
               #arguments: x, y, colour, group, fill, ..
               aes(x = price, y = reviews_per_month, color=neighbourhood_group))


#########################
# Histogram Geom
#########################

#create plot
ggplot(data = listings_clean) + 
  
  #map aesthetics and choose plot type
  geom_histogram(aes(x=price), color = "firebrick", fill = "firebrick") + 
  
  # labs will give you your label arguments
  labs(title="Histogram: Airbnb Listing Prices") +
  
  # ggplot provides "themes" that will 
  # help make your graphics look more professional
  # check out the ggthemes package too!
  theme_minimal()

#########################
# Scatter Geom
#########################

#create plot
ggplot(data = listings_clean) + 
  
  #map aesthetics and choose plot type
  #note that the color and fill arguments are OUTSIDE of the aesthetic mappings
  geom_point(aes(x=price, y=reviews_per_month), color = "firebrick", fill = "firebrick") + 
  
  # labs will give you your label arguments
  labs(title="Scatter Plot: Airbnb Listing Prices \n by Reviews Per Month") +
  
  # ggplot provides "themes" that will 
  # help make your graphics look more professional
  # check out the ggthemes package too!
  theme_minimal()


#########################
# Bar Geom
#########################

ggplot(data = listings_clean) + 
  geom_bar(mapping = aes(x = neighbourhood_group, stat="count")) + labs(title = "Count of Listings by Borough")

listings_clean %>%
group_by(neighbourhood_group) %>%
summarise(mean_price = mean(price)) %>%
ggplot() + 
  geom_bar(aes(x=neighbourhood_group, y=mean_price), stat="identity") + labs(title = "Mean Price by Borough")

#########################
# Times Series
#########################

z <- data.frame("z_values"=z, "time"=1:10)

# x = time, y = values
ggplot(data=z) + geom_line(aes(x=time, y=z_values))

#########################
# Layering Mappings
#########################

#let's layer on some different mappings
ggplot(data = listings_clean) + 
  
  #map aesthetics and choose plot type
  geom_point(aes(x=price, y=minimum_nights, size=calculated_host_listings_count,
                 fill=neighbourhood_group, color=neighbourhood_group)) + 
  
  # labs will give you your label arguments
  labs(title="Histogram: Airbnb Listing Prices \n Minimum Nights vs. Price") +
  
  # ggplot provides "themes" that will 
  # help make your graphics look more professional
  # check out the ggthemes package too!
  theme_minimal()

#########################
# Facets Wrapping
#########################

ggplot(data = listings_clean) +
  
  geom_histogram(mapping = aes(x = price)) +
  
  # facet wrap will "wrap" each mapping around 
  # the variable specified
  facet_wrap(~ neighbourhood_group, nrow = 1)  + labs(title = "Distribution of Price by Borough")

#########################
# dplyr + ggplot2
#########################


# let's combine some dplyr + ggplot2
library(dplyr)

# since data is the first argument in ggplot
# we can "pipe" right into the function
listings_clean %>%
  
  #manhattan neighbourhoods only
  filter(neighbourhood_group=="Manhattan") %>%
  ggplot() +
  geom_histogram(mapping = aes(x = price)) +
  
  # facet wrap will "wrap" each mapping around 
  # the variable specified
  facet_wrap(~ neighbourhood, nrow = 5)  + labs(title = "Distribution of Price by Neighbourhood")


listings_clean %>%
  ggplot() +
  geom_histogram(mapping = aes(x = price)) +
  
  # facet wrap will "wrap" each mapping around 
  # the variable specified
  facet_wrap(~ room_type, nrow = 2) + labs(title = "Distribution of Price by Room Type")


#########################
# Saving Plots
#########################

# save file as a pdf with width 8 inches and height 8 inches
pdf("test_plot.pdf", width=8, height=8)
# all plotting code
hist(listings_clean$price)
dev.off() # R knows plot is complete

#########################
# tidycensus (census api)
#########################

#-----------------------------Exercises-----------------------------#

#Exercise 1

library(tidycensus)

census_api_key("68009a7ec40d6be0a273f14a8a8920e924062829", install = TRUE)

#Exercise 2

