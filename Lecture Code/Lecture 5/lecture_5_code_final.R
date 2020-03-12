####################################
##                                ##
##   SDGB-7844 - Lecture 5 Code   ##
##   Tidy Data                    ##
##   Prof. Matthew Murphy         ##
##                                ##
####################################


# install packages command will 
# automatically search CRAN and install
install.packages("tidyverse")

# to use functions from a package
# you must "load" the packages

# library command will load packages for you
# the tidyverse library will load all of
# the tidyverse packages
library(tidyverse)

# be careful for function conflicts
# R will use the function from package 
# which was called first
dplyr::filter()
stats::filter()

#################
# reading data  #
#################

# read_csv(): comma separated (CSV) files
# read_tsv(): tab separated files
# read_delim(): general delimited files
# read_fwf(): fixed width files
# read_table(): tabular files where colums are separated by white-space.
# read_log(): reads Apache style log files

listings <- read_csv("airbnb_listings_nyc.csv")

# you will notice that read_csv recognizes 
# certain attributes about the columns
# last_review is recognized as a Date
# "blank" data handled as 'NA'
str(listings)

# tbl (tibble) is a special class
# of data frame
# if you want to learn more check R4DS
class(listings)


#################
# data cleaning #
#################


# searching for missing data
# NA denotes missing; 
# Note: blank is generally also missing, but not treated as missing by R 
#       so, (i) check for this issue,  (ii) replace any blank
#       spaces with NAs, and (iii) blank spaces in numerical columns turn the  
#       entire column into a character column, so you may need to filistings this

colnames(listings)[1:3] <- c("listing_id", "listing_name", "airbnb_host_id")

# beware of multiple blank spaces
# in this df, we only have "" instead of " " or "   " and so forth

which(listings$reviews_per_month== "") 
which(listings$reviews_per_month== " ") 

# replace blank spaces with NA
listings$last_review[listings$last_review== ""] <- NA

listings$last_review[7113]
listings$last_review[listings$last_review == " "] <- NA

listings$reviews_per_month[listings$reviews_per_month== ""] <- NA
listings$reviews_per_month[listings$reviews_per_month == " "] <- NA

# are there some properties with high number of min nights?
listings$minimum_nights[order(listings$minimum_nights, decreasing = TRUE)][1:10]

# complete cases will give only observations
# where we have data for every column
listings[!complete.cases(listings),]


listings_cc <- listings[complete.cases(listings),]  # omit obs. with any missing data
listings_cc <- subset(listings_cc, minimum_nights < 60) # omit obs. with large min nights


listings_clean <- listings_cc # use updated listings for remainder of analysis
rm(listings_cc)

# list unique boroughs
sort(unique(as.character(listings$neighbourhood_group)))

# list unique boroughs / fix Staten Island error
sort(unique(as.character(listings_clean$neighbourhood_group)))
listings_clean$neighbourhood_group[listings_clean$neighbourhood_group=="StatenIsland"] <- "Staten Island"
sort(unique(as.character(listings_clean$neighbourhood_group)))

#################
# select        #
#################

# we'll be exploring functions from the package:
library(dplyr)

# the first argument in tidyverse functions is 'data'
# usually this needs to reference a data frame
listings_select <- select(listings_clean, listing_id,last_review, room_type, price)
listings_select

# using a "-" sign in a select will de-select that column
# you can combine select and de-select operations
listings_dont_select <- select(listings_clean, -number_of_reviews)
listings_dont_select

# you can choose all columns between two
# similar to seq() command...
listings_seq <- select(listings_clean, listing_id:price)
listings_seq

# you can search columns for values
listings_contains <- select(listings_clean, contains("id"))


#################
# filter        #
#################

# filter function is similar to [ ] with a boolean expression
min_nights <- filter(listings_clean, minimum_nights < 30)

# let's see how much data we lost
nrow(min_nights)/nrow(listings_clean)

# filter works with dates too
thisyr <- filter(listings_clean, last_review >= as.Date("2017-09-08"))

# characters too
entire_apt <- filter(listings_clean, room_type == "Entire home/apt")

# use the '&' to combine methods
thisyr_entire <- filter(listings_clean, room_type == "Entire home/apt" & last_review >= as.Date("2017-09-08"))


#############################
#        Pipes              #
#         %>%               #
#############################

# let's combine select / filter commands
# Using the %>% operator allows you to chain your tidyverse commands together
# In plain language, it will "forward" your data argument into the next function

listings_clean %>% 
  select(listing_id, minimum_nights, 
         last_review, price) %>%
  filter(minimum_nights < 30) %>% 
  filter(last_review >= as.Date("2017-09-08"))

# the period '.' is a placeholder for your data
# if you want to use the pipe with functions
# where data is not the first argument
# you can use the period to specify
# which argument you want your data to be "forwarded" to
listings_clean %>% 
  select( . , listing_id, minimum_nights, 
         last_review, price) %>%
  filter( . , minimum_nights < 30) %>% 
  filter( . , last_review >= as.Date("2017-09-08"))


############################
# group by                  #
# summarize / summarise     #
#############################


# the group_by and summarize commands 
# are a replacement for aggregate()
listings_clean %>%
  group_by(neighbourhood_group) %>%
  summarise(mean_price = mean(price))

# the n() function will give you count data
# add arrange() to the end of your function to 
# rank / order your data
listings_clean %>%
  group_by(neighbourhood_group) %>%
  summarise(listing_count = n()) %>%
  arrange(desc(listing_count))

# mutate will add a new column to your data set
listings_clean %>%
  mutate(occupancy = (number_of_reviews * 2) * 5.5) %>%
  select(occupancy)


#################
# joins         #
#################

# let's read in the review data
reviews_2018 <- read_csv("reviews_aggregated.csv")

# looks like our listing_id is 
# the "key" between our two data sets
head(reviews_2018)

# the _join functions follow the form
# direction_join(x, y, by = )
# in this case we want to keep the data
# in the "left" df - listings
# and join matches in reviews_2018
listings_clean %>% 
  left_join(reviews_2018, by = c("listing_id" = "listing_id"))

# the _join functions follow the form
# direction_join(x, y, by = )
# in this case we want to keep the data
# in the "right" df - reviews_2018
# and join matches in listings
listings_clean %>% 
  right_join(reviews_2018, by = c("listing_id" = "listing_id"))

#################
# tidy data     #
#################

# we'll be exploring functions from the package:
library(tidyr)

# let's execute on the example we worked through
person <- c("John Smith", "Jane Doe", "Mary Johnson")
treatment_a <- c(14, 5, 7)
treatment_b <- c(8, 11, 13)

df <- data.frame(person, treatment_a, treatment_b)

#################
# gather        #
#################

# the gather command will take your
# "wide" data and make it "long"
# arguments(new_variable, data_value, ...)
# ... = columns to be gathered
tidy_data <-  df %>% 
  gather(treatment, result, treatment_a, treatment_b)

df
tidy_data

#################
# spread        #
#################

# the spread command will take your
# "long" data and make it "wide"
# arguments(variable_to_spread, data_values)
# this is useful when preping data for
# modeling packages
tidy_data

rect_data <- tidy_data %>% 
  spread(treatment, result)

rect_data

#################
# separate      #
#################

# the separate function is very useful
# this can help with data cleaning
# here we will "separate"
# first and last names of our subjects
# you can separate on any delimiter
tidy_data 

name_separated <- tidy_data %>% 
  separate(person, 
           into=c("first_name", "last_name"), 
           sep=" ")

name_separated

#################
# unite         #
#################

# the unite function will "unite" two
# columns. you do have to specify
# what the delimiting character will be
# in this case we'll choose an underscore
name_separated

one_name <- name_separated %>% 
  unite(full_name, first_name, 
        last_name, sep = '_')

one_name


#################
# data cleaning #
# the tidy way  #
#################

# compare to colnames() function below
# colnames(listings)[1:3] <- c("listing_id", "listing_name", "airbnb_host_id")
listings_tidyc <- listings %>% rename(listing_id = id, 
                                            listing_name = name, 
                                            airbnb_host_id = host_id )




# remember the StatenIsland vs. Staten Island error?
sort(unique(as.character(listings_tidyc$neighbourhood_group)))

# here's an alternative solution to:
# listings_clean$neighbourhood_group[listings_clean$neighbourhood_group=="StatenIsland"] <- "Staten Island"

listings_tidyc <- listings_tidyc %>% 
  mutate(
  neighbourhood_group = if_else( neighbourhood_group=="StatenIsland", "Staten Island", neighbourhood_group) 
         )



# here's an alternative solution to:
#listings_clean$minimum_nights[order(listings_clean$minimum_nights, decreasing = TRUE)][1:10]
listings_tidyc %>%
  select(minimum_nights) %>%
  arrange(desc(minimum_nights))



# here's an alternative solution to:
#listings_cc <- subset(listings_clean, minimum_nights < 60)
listings_tidyc <- listings_tidyc %>%
  filter(minimum_nights < 60)


# here's an alternative solution to:
# listings[!complete.cases(listings),]
listings_clean <- drop_na(listings)

listings %>% rename(listing_id = id, 
                listing_name = name, 
                airbnb_host_id = host_id ) %>%
             mutate(neighbourhood_group = if_else( neighbourhood_group=="StatenIsland", 
                                                   "Staten Island", # condition if yes
                                                   neighbourhood_group) ) %>% 
            filter(minimum_nights < 60) %>% drop_na()
  

#-----------------------------Exercises-----------------------------#

#1

data(mtcars)
