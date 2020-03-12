####################################
##                                ##
##   SDGB-7844 - Lecture 4 Code   ##
##   Exploratory Data Analysis    ##
##   Prof. Matthew Murphy         ##
##                                ##
####################################

#########################
# Workspace & R Files
#########################

# listing every object in your workspace
ls()
# removing items in your workspace
rm()
# clearing your workspace - deletes all
rm(list=ls())
# saving every object in your workspace
save.image()
# file_name.RData
# loading a saved workspace
load()

# determine/change directory R is pointing to using
# can also do this manually in RStudio
getwd()
setwd()

###################################
# variance and standard deviation #
###################################

x <- faithful$eruptions
n <- length(x)

sum_of_sq <- 0
for(i in 1:n){
  sum_of_sq <- sum_of_sq + (x[i]-mean(x))^2
} # end for loop
sum_of_sq/(n-1)            # variance
sqrt(sum_of_sq/(n-1))      # standard deviation

# shorter version:
sum((x-mean(x))^2)/(length(x)-1)          # variance
sqrt(sum((x-mean(x))^2)/(length(x)-1))    # standard deviation

# shortest:
var(x)        # variance
sd(x)         # standard deviation

# data frame - can combine variable types in columns
y <- data.frame("a"=c(1,2,3),
                "b"=c("words","are","here"),
                "c"=c(TRUE,TRUE,FALSE))
colnames(y) #will return column names
rownames(y) #will return row names
str(y) #will give a summary of object data types
# 4 ways to extract column (vector) "a"
y$a
y[,1]
y[,"a"]
y[["a"]]

####################
# loading  data    # 
####################

# remember to set working directory
# setwd("~/Downloads")

# loading in data
listings <- read.csv("airbnb_listings_nyc.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)

dim(listings)
colnames(listings)
head(listings)
tail(listings)
nrow(listings)


##################
# rename columns #
##################

# rename columns for ease of use
colnames(listings)[1:3] <- c("listing_id", "listing_name", "airbnb_host_id")


#################
# data cleaning #
#################


# searching for missing data
# NA denotes missing; 
# Note: blank is generally also missing, but not treated as missing by R 
#       so, (i) check for this issue,  (ii) replace any blank
#       spaces with NAs, and (iii) blank spaces in numerical columns turn the  
#       entire column into a character column, so you may need to filistings this

# beware of multiple blank spaces
# in this df, we only have "" instead of " " or "   " and so forth

which(listings$reviews_per_month== "") 
which(listings$reviews_per_month== " ") 

which(listings$last_review== "") 
which(listings$last_review== " ") 

# replace blank spaces with NA
listings$last_review[listings$last_review== ""] <- NA
listings$last_review[listings$last_review == " "] <- NA

listings$reviews_per_month[listings$reviews_per_month== ""] <- NA
listings$reviews_per_month[listings$reviews_per_month == " "] <- NA

#listings[is.na(listings) == TRUE,]

# are there some properties with high number of min nights?
listings$minimum_nights[order(listings$minimum_nights, decreasing = TRUE)][1:10]

# complete cases will give only observations
# where we have data for every column
listings[!complete.cases(listings),]

listings_clean <- subset(listings, !is.na(reviews_per_month))   # omit obs. with missing reviews
listings_clean <- subset(listings_clean, minimum_nights < 60)  # omit obs. with large min nights

listings_cc <- listings[complete.cases(listings),]  # omit obs. with any missing data
listings_cc <- subset(listings_cc, minimum_nights < 60) # omit obs. with large min nights

# missing rates:
1-nrow(listings_clean)/nrow(listings)      # fraction missing selfies.pop
1-nrow(listings_cc)/nrow(listings)  # fraction missing at least one variable

listings_clean <- listings_cc # use updated listings for remainder of analysis
rm(listings_cc, listings)

########
# data errors

# list unique cities
sort(unique(as.character(listings_clean$neighbourhood_group)))

# list unique countries
sort(unique(as.character(listings_clean$neighbourhood_group)))
listings_clean$neighbourhood_group[listings_clean$neighbourhood_group=="StatenIsland"] <- "Staten Island"
sort(unique(as.character(listings_clean$neighbourhood_group)))


################################
# using the occupancy model
################################

# occupancy calculation
# parameters:
# number of reviews (50% review rate assumed)
# 5.5 Airbnb reported average length of stay
listings_clean$occupancy <- (listings_clean$number_of_reviews*2) * 5.5

# we wil only be analyzing the listings 
# that are "full apartments"
entire_apt <- subset(listings_clean, room_type == "Entire home/apt")

# let's take a look at average occupancy
mean(entire_apt$occupancy)/365

# hotel level occupancy was defined as
# 70% of available days occupied
entire_apt$hotel_ind <- ifelse(entire_apt$occupancy > 255, "Unregulated Hotel", "Normal Occupancy")

# glance at categorical data
table(entire_apt$hotel_ind)

# calculate % of unregulated hotels
nrow(entire_apt[entire_apt$hotel_ind=="Unregulated Hotel", ])/nrow(entire_apt)

# The Hotel Room Occupancy Tax Rate of 5.875% went into effect on December 20, 2013. 
# Let's try to figure out how much tax NYC is missing out on
hotels <- entire_apt[entire_apt$hotel_ind=="Unregulated Hotel",]

# let's calculate revenue for these properties
sum(hotels$price*hotels$occupancy)

# how much of that revenue would be collected as tax?
sum(hotels$price*hotels$occupancy)*.05875

################################################
# exploratory data analysis: tabulations     ###
# and aggregations                           ###
################################################

# distribution across boroughs:

table(listings_clean$neighbourhood_group)

# neighborhood with most listings in data (for least use which.min)

which.max(table(listings_clean$neighbourhood))

# calculating listings per borough
# way to read: aggregate price, by neighbourhood_group using sum()

temp <- aggregate(listings_clean$price, # data to aggregate
                  by=list("neighbourhood_group" = listings_clean$neighbourhood_group), # by this grouping
                          sum, # this argument is function we execute
                          na.rm=TRUE) # na.rm is important
                  
colnames(temp) <- c("neighbourhood_group", "sum_listings")
temp
                  
# average price, availability
apply(listings_clean[,c("price", "availability_365")],
                        2,
                        mean,
                        na.rm=TRUE)
# second argument: 1 --> by row; 2 --> by column
# apply() and aggregate() are VERY USEFUL functions
# lets us avoid using loops
# use lapply() for lists


##################
# writing output #
###################


# write processed data to a new file
# note: never overwrite your original file
#       you should always be able to access your raw data
write.csv(listings_clean, file="listings_clean.csv",
          append=FALSE, sep=",", row.names=FALSE,
          col.names=TRUE)
# note: append --> add these rows to an existing file (TRUE)
#                  or make a new file (FALSE)?

df <- read.csv("listings_clean.csv", sep=",",
               header=TRUE, stringsAsFactors = FALSE)
head(df)
tail(df)
rm(df)