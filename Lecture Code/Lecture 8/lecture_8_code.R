#############################################
##                                         ##
##   SDGB-7844 - Lecture 8 Code            ##
##   Discrete Probability Distributions    ##
##   Functions                             ##
##   Prof. Matthew Murphy                  ##
##                                         ## 
############################################


#############################################
#  plotting the probability mass function   #
#############################################

# die roll pmf

# plotting the dots
#pdf("diepmf.pdf", height=7, width=11)
par(mar=c(5, 5, 4, 2)+0.1)  # adding a wider margin to the left side so
# that the label "probability" is not cut off
plot(x=sort(rep(1:6, times=2)), 
     y=rep(c(0, 1/6), times=6), 
     pch=rep(c(1, 19), times=6), ylim=c(0, 2/6), type = 'h',
     xlab="outcomes", 
     yaxt="n", xaxt="n",
     ylab="", xlim=c(0, 7),
     main="PMF for a Die Roll", cex=3, cex.main=2.2, cex.lab=1.8)
# add points
points(c(rep(1/6, times = 6)), pch=19)

# add segments
segments(x0=(1:5)+1/10, y0=rep(0, times=6), x1=(2:6)-1/10)  	
# add arrows
arrows(x0=1-1/10, y0=0, x1=-1)
arrows(x0=6+1/10, y0=0, x1=7)
# y-axis     
axis(side=2, at=c(0, 1/6, 2/6), labels=c("0", "1/6", "2/6"), 
     las=TRUE, cex.axis=1.8)
# x-axis
axis(side=1, at=1:6, labels=1:6, cex.axis=1.8)
# y-axis label
mtext("probability", side=2, line=4, cex=1.8)

#dev.off()
# alternatively, can make a step function with plot.stepfun()


####################################################
# 3. plotting the cumulative distribution function #
####################################################

# die roll cdf

# plotting open and closed dots
#pdf("diecdf.pdf", height=7, width=11)
par(mar=c(5, 5, 4, 2)+0.1)  # adding a wider margin to the left side so
# that the label "probability" is not cut off
plot(x=sort(rep(1:6, times=2)), y=c(0, sort(rep(1:5, times=2))/6, 1), 
     pch=rep(c(1, 19), times=6), cex=1, xlim=c(-1, 7), cex.main=1.5, cex.lab=1.5, 
     main="CDF for a Die Roll", xlab="outcomes", ylab="", yaxt="n", xaxt="n")
segments(x0=1:5, y0=(1:5)/6, x1=(2:6)-1/15)
arrows(x0=1-1/15, y0=0, x1=-1)
arrows(x0=6, y0=1, x1=7)
# adding our own axes
axis(side=2, at=(0:6)/6, labels=c("0", "1/6", "2/6", "3/6", "4/6", "5/6", "6/6"), las=TRUE, cex.axis=1)
axis(side=1, at=1:6, labels=1:6, cex.axis=1.4)
# y-axis label
mtext("probability", side=2, line=4, cex=1.5)
#dev.off()

# alternatively, can make a step function with plot.stepfun()

#############################################################
# 4. expectation and variance of a die roll random variable #
#############################################################

# expected value
sum((1:6)/6)

# variance
sum((1:6)^2/6) - (sum((1:6)/6))^2

# standard deviation
sqrt(sum((1:6)^2/6) - (sum((1:6)/6))^2)

#################
# 5. Simulation #
#################


# simulate 1,000 rolls of a die
x <- sample(1:6, size=1000, replace=TRUE)

#pdf("diesim.pdf", height=5, width=10)
par(mfrow=c(1,2))
barplot(table(x), las=TRUE, xlab="roll outcomes", ylab="frequency", 
        main="Frequencies for \n Simulated Die Rolls", col="royalblue")

barplot(table(x)/1000, las=TRUE, xlab="roll outcomes", ylab="fraction", 
        main="Empirical PMF for \n Simulated Die Rolls", ylim=c(0, 0.2), col="royalblue")
abline(h=1/6, col="firebrick", lty=2, lwd=2)
axis(side=2, at=1/6, label="1/6", col="red", las=TRUE)
#dev.off()

###################
# 6. increasing n #
###################

#pdf("dien.pdf", width=10, height=5)
par(mfrow=c(1,3))

# n=1,000
x <- sample(1:6, size=1000, replace=TRUE)
barplot(table(x)/1000, las=TRUE, xlab="roll outcomes", 
        ylab="fraction", main="1,000 Rolls", ylim=c(0, 0.2), col = "royalblue")
abline(h=1/6, col="firebrick", lty=2, lwd=2)
axis(side=2, at=1/6, label="1/6", col="red", las=TRUE)

# n=10,000
y <- sample(1:6, size=10000, replace=TRUE)
barplot(table(y)/10000, las=TRUE, xlab="roll outcomes", 
        ylab="fraction",  main="10,000 Rolls", ylim=c(0, 0.2), col = "royalblue")
abline(h=1/6, col="firebrick", lty=2, lwd=2)
axis(side=2, at=1/6, label="1/6", col="red", las=TRUE)

# n=100,000
z <- sample(1:6, size=100000, replace=TRUE)
barplot(table(z)/100000, las=TRUE, xlab="roll outcomes", 
        ylab="fraction", main="100,000 Rolls", ylim=c(0, 0.2), col="royalblue")
abline(h=1/6, col="firebrick", lty=2, lwd=2)
axis(side=2, at=1/6, label="1/6", col="red", las=TRUE)

rm(x,y,z)
#dev.off()


####################
# lists            #
####################

### list (general form of data.frame--components 
###        can be of varying object and/or data type)
w <- list("d"=c(1,2), "e"=data.frame("a"=c("hi","hi","bye"), "b"=c(1, 6, 3)))
w       # print all of list w
w$d     # extract vector "d"
w[[1]]  # extract vector "d" (first list component)
rm(w)

###########################
#  writing the function   #
###########################

die_roll <- function(sides=6, n_sims=1000){
  # sides: specify n-sided die (default is 6)
  # n_sims: number of rolls (default is 100)
  # simulate die rolls
  x <- sample(1:sides, size=n_sims, replace=TRUE)
  # compute fractions, format result
  result <- data.frame("side"=1:sides,
                       "fraction"=as.vector(table(x)/n_sims))
  # return two objects: a data frame called result and
  # the number of simulation runs in list form
  return(list("result"=result, "n_sims"=n_sims))
} # end function



###########################
#   calling the function  #
###########################

die_roll() # run function with default settings
# for both arguments

die_roll(sides=7) # run function with default settings
# for "n.sim" argument only

die_roll(10, 100) # argument values listed in order so argument 
# names are not required


# save results to R object           
test <- die_roll(n_sim=100, sides=10) 
test          # print elements of test (note: test is a list)
test$result   # extract result element of list
test$n.sim    # extract n.sim element of list

ls() # list global objects
# note sides, n_sim, x, and result which were defined in die_roll()
# are not listed because they are local, not global, variables

###########################
#  mean example           #
###########################

mean_func <- function(x){
  #sample mean function
  
  #important when writing functions to ensure
  #that it will run properly if given an incorrect
  #data type.  Here we're throwing a warning.
  
  if(is.numeric(x) & is.vector(x)){
    
    #calculate mean
    mean_x <- sum(x)/length(x)
    
    #return single value
    return(mean_x)
    
  } else {
    
    #warning statement  
    print("WARNING: Data is not numeric.  This function can only accept numeric vectors.")
    
  }
  
}

# let's see how our function handles non numeric data:
x <- c("a", "b", "c")

# run function
mean_func(x)


# now let's try numeric values
x <- rnorm(30)

mean_func(x)

#assign data to variable
x_mean <- mean_func(x)

#EXERCISES

#1

two_means <- function(x){
  if(is.numeric(x) & is.vector(x)){
    
  }
  
  return(list("geom_mean" = geom_mean, "arith_mean" = arith_mean))
}