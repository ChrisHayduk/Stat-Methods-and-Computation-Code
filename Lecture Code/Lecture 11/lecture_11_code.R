#############################################
##                                         ##
##   SDGB-7844 - Lecture 11 Code           ##
##   Hypothesis Testing                    ##
##   Confidence Intervals                  ##
##   Prof. Matthew Murphy                  ##
##                                         ## 
############################################

###############################################
#  sampling distribution: Poisson data        #
###############################################

samp_dist <- function(n, k, lambda){
  # n=sample size, k=times to run, lambda=Poisson parameter
  # stop function if input invalid
  if(any(c(k, n, lambda) <= 0)){return("error!")}
  mean_vector <- rep(NA, times=k) # empty results vector
  for(i in 1:k){
    x <- rpois(n=n, lambda=lambda) # simulate
    mean_vector[i] <- mean(x) # compute x-bar
    rm(x) # clear memory
  } # end for loop
  return(mean_vector) #output
} # end function


###################################
#   running & graphing samp_dist  #
###################################

#pdf("poisson_sampdist.pdf", width=12, height=8)
par(mfrow=c(2,2))

#####
## Poisson population distribution

lambda <- 5
k <- 0:15
prob <- dpois(k, lambda=lambda)

barplot(prob, names.arg=k, las=TRUE, col="firebrick", xlab="k (number of calls)", ylim=c(0, 0.3),
		ylab="probability P(X=k)", main=expression(paste("PMF for X~Poisson(", lambda==5,")", sep="")))
legend("topright", legend=c(expression(paste(lambda==5,  sep="")), "", "", 
		expression(paste(mu==5,  sep="")), 
		expression(paste(sigma==2.24,  sep=""))), col="white", bty="n", cex=1.2)


#######

# sampling distribution simulation

z.10 <- samp_dist(n=10, k=500, lambda=5)
z.30 <- samp_dist(n=30, k=500, lambda=5)
z.100 <- samp_dist(n=100, k=500, lambda=5)

y.max <- ceiling(max(c(hist(z.10, plot=FALSE)$density, hist(z.30, plot=FALSE)$density, hist(z.100, plot=FALSE)$density)))
x.min <- min(c(z.10, z.30, z.100))
x.max <- max(c(z.10, z.30, z.100))

#####
## n= 10

# histogram: density not frequency
hist(z.10, freq=FALSE, ylim=c(0,y.max), xlim=c(x.min, x.max), 
      main=expression(paste("Sampling Distribution of ",bar(x), ": n=10, ", lambda==5,sep="")), 
        las=TRUE, col="cadet blue", xlab="sample means (from Poisson data)", ylab="density")

# add normal density curve to histogram:
curve(dnorm(x, mean=5, sd=sqrt(5/10)), add=TRUE, 
           lwd=2, col="firebrick")

# add legend
legend("topright", legend=c(expression(paste("E[",bar(x),"]=5", sep="")), 
							paste("mean of sample means: ", round(mean(z.10), digits=3), sep=""), "",
							expression(paste("SD[",bar(x),"]=0.707")),
							paste("sd of sample means: ", round(sd(z.10), digits=3), sep="")), bty="n")

#####
## n=30

# histogram: density not frequency
hist(z.30, freq=FALSE, ylim=c(0,y.max), xlim=c(x.min, x.max), 
      main=expression(paste("Sampling Distribution of ",bar(x), ": n=30, ", lambda==5,sep="")), 
        las=TRUE, col="cadet blue", xlab="sample means (from Poisson data)", ylab="density")

# add normal density curve to histogram:
curve(dnorm(x, mean=5, sd=sqrt(5/30)), add=TRUE, 
           lwd=2, col="firebrick")

# add legend
legend("topright", legend=c(expression(paste("E[",bar(x),"]=5", sep="")), 
							paste("mean of sample means: ", round(mean(z.30), digits=3), sep=""), "",
							expression(paste("SD[",bar(x),"]=0.408")),
							paste("sd of sample means: ", round(sd(z.30), digits=3), sep="")), bty="n")


#####
## n=100



# histogram: density not frequency
hist(z.100, freq=FALSE, ylim=c(0,y.max), xlim=c(x.min, x.max), 
      main=expression(paste("Sampling Distribution of ",bar(x), ": n=100, ", lambda==5,sep="")), 
        las=TRUE, col="cadet blue", xlab="sample means (from Poisson data)", ylab="density")

# add normal density curve to histogram:
curve(dnorm(x, mean=5, sd=sqrt(5/100)), add=TRUE, 
           lwd=2, col="firebrick")

# add legend
legend("topright", legend=c(expression(paste("E[",bar(x),"]=5", sep="")), 
							paste("mean of sample means: ", round(mean(z.100), digits=3), sep=""), "",
							expression(paste("SD[",bar(x),"]=0.224")),
							paste("sd of sample means: ", round(sd(z.100), digits=3), sep="")), bty="n")

rm(z.10, z.30, z.100, y.max, x.min, x.max)
#dev.off()


rm(list=ls()) # clear workspace


###################################################
#  upload and process transaction data            #
###################################################

# upload data
x <- read.csv("transaction_data.csv")

dim(x)
head(x)
tail(x)
colnames(x)

################################
# 4. exploratory data analysis #
################################

#pdf("sumplot_rent.pdf", height=4, width=12)
par(mfrow=c(1,3), mar=c(5, 5, 4, 2))
# histogram of data		
hist(x$transaction_amount, las=TRUE, xlab="transaction amount", ylab="frequency",		
		main="Histogram of \nTransaction Amount", cex.axis=1.2, cex.lab=1.3, col = "royalblue")
abline(v=200, col="red", lty=2, lwd=2)     # add vertical line at $1,500 mph                      

# boxplot
boxplot(x$transaction_amount, main="Box Plot of \nTransaction Amount", pch=19, las=TRUE, ylab="transaction amount")
abline(h=200, col="red", lty=2, lwd=2)     # add vertical line at $1,500 mph  

# normal quantile plots
qqnorm(x$transaction_amount, pch=19, las=TRUE, main="Normal Q-Q Plots of \nTransaction Amount", ylab="",
	cex.lab=1.3, cex.axis=1.2)
qqline(x$transaction_amount)
dev.off()

# summary statistics:
# amounts are in dollars, so we round to the nearest cent

nrow(x) # sample size

round(mean(x$transaction_amount), digits=2)
round(sd(x$transaction_amount), digits=2)

min(x$transaction_amount)
median(x$transaction_amount)
max(x$transaction_amount)

# percent of transactions with amount higher than 200
100*round(sum(x$transaction_amount > 200)/length(x$transaction_amount), digits=3)


###########################################
# transaction amounts, hypothesis tests   #
###########################################

####
# calculations by hand:

# test statistic
test.stat <- (mean(x$transaction_amount)-200)/(sd(x$transaction_amount)/sqrt(nrow(x)))
test.stat <- 2.8857

# p-value: P(T > test statistic)
# p - probability, yields CDF, i.e. probability of returning number smaller than an argument to this function
pt(test.stat, df=nrow(x)-1, lower.tail=FALSE)

# (graphical version of p-value)
dev.off()
#pdf("pvalue.pdf", height=4, width=7)


curve(dt(x, df=119), from=-3, to=5, las=TRUE, xlab="t", ylab="density", 	
	 main=paste("Density of the t(", nrow(x)-1, ") Distribution", sep=""), 
	 col="firebrick", lwd=2, xaxt="n")
axis(side=1, at=test.stat, labels=round(test.stat, digits=3))

w <- seq(test.stat, 5 ,length=200)

# d - density, yields density function value in a given point
y <- dt(w, df=nrow(x)-1)  

# shade in the p-value area	
polygon(c(test.stat, w, 5) , c(0, y, 0), col="blue", border=NA, density=40)
arrows(2.8857, 0.1, 2.8857, 0.02)
text(2.9, 0.13, labels=paste("p-value=P(T > ",round(test.stat, digits=3), ")", sep=""))
rm(w, y)
#dev.off()

# with t.test()
t.test(x=x$transaction_amount, alternative="greater", mu=200, conf.level = 0.99)


###########################################
#    one-, two-sided confidence intervals #
###########################################

# q - quantile, inverse CDF, i.e. what value is at given quantile
qt(p=0.01, df=119, lower.tail=FALSE)

qt(p=0.005, df=119, lower.tail=FALSE)


# confirming confidence interval in t.test() output for wind speed example

# lower bound: 
mean(x$transaction_amount) - (qt(p=0.01, df=119, lower.tail=FALSE)*sd(x$transaction_amount)/sqrt(nrow(x)))

# upper bound: infinity

# confidence interval: ($201.20, infinity)
# this matches the confidence interval computed from t.test() in previous section