####################################
##                                ##
##   SDGB-7844 - Lecture 2 Code   ##
##   Basic Computation            ##
##   Prof. Matthew Murphy         ##
##                                ##
####################################

#######################
# R as a calculator   #
#######################


# this is a comment and is ignored by the computer
# use commenting LIBERALLY to explain your code
# +, -, *, /,^

# use ( parentheses ) to ensure order of operations
((59-11.9)*2) / (3+3)^2 

# assign a number to an object named z
z <- 59 

# you can use z to replace '59'
x <- ((z-11.9)*2) / (3+3)^2 # save expression to x
x

# reassign object x to the number 5 (from the number 4)
x <- 9 
x^(1/2)
# use the square root function sqrt()
sqrt(x)

# Other Math Ops #

# integer division
# no remainder after division
17%/%8

# modulus
# returns the remainder after division (very useful when writing loops)
17%%8

# math functions
# review from lecture 1 - assign number 9 to x
x <- 9 
x^(1/2)
# use the square root function sqrt()
sqrt(x)

#######################
# Common Calculations #
#######################

x <- 5
# e^x where e=2.718... (e.g., used in computing compound interest)
exp(1)
exp(x)
# logarithms
log(x)            # default is ln, log base e
log(x, base=10)   # log base 10
# absolute value |x|
abs(x)

#######################
# Special Cases / Ops #
#######################


# dividing by 0
6/0
-11/0
# "Not a number" (NaN)
0/0
Inf/Inf
0*Inf
Inf-Inf
# missing value (very important!)
NA
# empty set:
NULL

# special values
pi
Inf

#######################
# Creating Vectors
#######################

# c() function combines values
A <- c(13, 5, 3, 11)

# scalar
B <- 3

# substitute variable for '3'
A <- c(13, 5, B, 11)

# extract third element
# [ ] brackets are used to create subsets
# we will cover subsets in detail in future lectures
A[3]

# extract 1st & 2nd element
A[c(1, 2)]

# extract 3rd element
i <- 3
A[i]


# other ways to construct vectors:

# : is used by r as a "from : to"
# this is useful in selecting rows or columns
1:25
x <- 1:25

# seq( ) 
# the sequence function has three key arguments
# from, to, by
# length is also an argument
seq(from = 1, to=20, by=2)
seq(from =1, to=20, length=5)

# rep ( ) 
# the repeat function has two arguments:
# value and number of times to repeat
rep(14, times=10)
y <- rep(c(1,2,3), times=6)

# length of a vector
length(y) 

# combining methods
# apply a function element-wise to a vector
sqrt(y) + 5
x*y

# sort a vector in ascending order
sort(y, decreasing=FALSE)
order(y, decreasing=FALSE)

#######################
# Matrices
#######################

# Vectors to Matrices
x <- c(13, 5, 8, 11)
y <- c(7, 15, 4, 2)

# cbind( ) or column bind is used to create matrices or data frames
K <- cbind(x, y)
K

# the matrix function works similarly
# you must specify number of rows and columns
# byrow should always be FALSE
K <- matrix(c(x,y), nrow=4, ncol=2, byrow = FALSE)

# how NOT to create a matrix
rbind(x,y)
matrix(c(x,y), nrow=3, ncol=2, byrow=TRUE)

#subsetting a matrix
# K[row_number, column_number]
K[1,2]

# Matrix Operations
A <- matrix(c(1, 2, 9, 8, 7, 2), nrow=2, ncol=3, byrow=TRUE)
B <- matrix(c(7, 6, 1, 3, 9, 5), nrow=2, ncol=3, byrow=TRUE)

# common matrix functions & operations
dim(A)    # rows x columns
nrow(A)   # how many rows?
ncol(A)   # how many columns?

# applying a operators & functions element-wise to a matrix
4*A+8

# applying a function to matrix 
sqrt(A)

# addition
A+B

#######################
# Other Data Types
#######################


x <- c("statistical", "programming")

substr(x, start=1, stop=10)  # extract a subset of a string
nchar(x)                    # number of characters

# Boolean or Logical: TRUE or FALSE
# Often used in if / else statements
o <- c(TRUE, FALSE, FALSE)
m <- c(x, "R")
m[o]

#######################
# Data Frames & Lists
#######################

# data frame - can combine variable types in columns
y <- data.frame("a"=c(1,2,3),
                "b"=c("words","are","here"),
                "c"=c(TRUE,TRUE,FALSE))
colnames(y) #will return column names
rownames(y) #will return row names
str(y) #will give a summary of object data types


# this list is a general form of data frame
# varying object and/or data type
w <- list("d"=c(1,2), "e"=c("words","are","also", "here"))
str(w)

# Extracting elements from Data Frames & Lists

# data frames
# 4 ways to extract column (vector) "a"
y$a
y[,1]
y[,"a"]
y[["a"]]


# lists
# 2 ways to extract an element
w$d
w[[1]]

# structure function describes objects
str(y)
str(w)

#########################
# Determining Data Type
#########################

x <- c(1, 2, 3)
y <- c("a", "b", "c")
z <- c(TRUE, FALSE, TRUE)
class(z)
mode(x)
mode(y)

# will return logical / boolean
is.integer(x)
is.numeric(x)
is.factor(y)
is.character(y)
is.logical(z)

# other options - data type
is.integer(y)
is.numeric(z)
is.factor(x)
is.character(y)
is.logical(y)

#object type

is.vector(x)
is.matrix(A)
is.data.frame(z)
is.list(w)

# extremely important function!!!
str(A)

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

# save code as a ".R" file
# R is case sensitive, remember this!
# Use to upload the code in the entire file in to the R console
source("FILE PATH.R")

# save/load history of commands entered into console
savehistory()
loadhistory()

# determine/change directory R is pointing to using
# can also do this manually in RStudio
getwd()
setwd()

#====================================================================#
#EXERCIES

#1
a <- seq(from = 2, to = 398, by = 2)

#2
b <- 1:100

b <- b[b%%2 != 0 & b%%3 != 0]

#3
vector1 <- c(4, 67, 8, 1)
vector2 <- c("Math", "functions", "in", "R.")
vector3 <- c(FALSE, FALSE, TRUE, TRUE)

y <- data.frame(vector1, vector2, vector3)

#4
colnames(y) <- c("numbers", "words", "boolean_values")

#5
save.image("lecture_2.RData")
rm(list=ls())
load("lecture_2.RData")
ls()

#6
x <- c(16, NA, 93, 71, NA, 8)
y <- sort(x, decreasing = TRUE)