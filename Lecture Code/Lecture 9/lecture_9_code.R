#############################################
##                                         ##
##   SDGB-7844 - Lecture 9 Code            ##
##   Simulation                            ##
##   Continuous Probability Distributions  ##
##   Prof. Matthew Murphy                  ##
##                                         ## 
############################################


######################################
#  Binomial Distribution Functions  #
######################################

####
# factorial: n! = n*(n-1)*...*2*1
factorial(x=4)   # 4! = 4*3*2*1=24
factorial(x=6)   # 6! = 6*5*4*3*2*1=720

####
# n choose k (combinations)
choose(n=5, k=3)     # number of ways to choose 3 items from 5 = 10
choose(n=10, k=0)    # number of ways to choose 0 items from 10 = 1


####
# binomial pmf:
dbinom(x=4, size=10, prob=0.5)     # P(X=4) when n=10, p=0.5 (approx. 0.205)
dbinom(x=12, size=20, prob=0.75)   # P(X=12) when n=20, p=0.75 (approx, 0.061)



# simulating draws from a binomial distribution
rbinom(n=10, size=5, prob=0.6)    # 10 independent draws from a binomial  
# dist. with n=5 (number of trials), p=0.6
rbinom(n=1000, size=20, prob=0.6) # 1,000 independent draws from a binomial  
# dist. with n=20 (number of trials), p=0.6

x <- rbinom(n=1000, size=5, prob=0.6) # comparing theory and practice
# note: if we run this command again, I will get a
# different vector of simulated values

par(mfrow=c(2,2)) # 2x2 figure with 4 plots max in 2 columns

# graph of binomial pmf, empirical pmf when n=5, p=0.5 (i.e., 5 flips of a fair coin)
# to compare graphs, we want the y-axis limits to be the same
y.max <- max(c(dbinom(0:5, size=5, prob=0.6), as.vector(table(x)/length(x)))) 
barplot(dbinom(0:5, size=5, prob=0.6), names.arg=0:5, las=TRUE,
        col="royalblue", xlab="k (number of successes)", ylab="probability P(X=k)",
        main="PMF for Binomial(n=5, p=0.6)", ylim=c(0, y.max))
barplot(as.vector(table(x)/length(x)), names.arg=0:5, las=TRUE,
        col="royalblue", xlab="k (number of successes)", ylab="probability P(X=k)",
        main="Empirical PMF for \nBinomial(n=5, p=0.6)", ylim=c(0, y.max))
text(x=1.8, y=0.3, labels="1,000 random draws")

# graph of binomial cdf, empirical cdf when n=5, p=0.5 (i.e., 5 flips of a fair coin)
plot.stepfun(stepfun(0:5, c(0, pbinom(q=0:5, size=5, prob=0.6)), right=TRUE), las=TRUE,
             ylab=expression(P(X<= k)), main="CDF for Binomial(n=5, p=0.6)")
# see ?plotmath for adding mathematical expressions to your plots
# ecdf() computes the empirical cdf function
plot(ecdf(x), las=TRUE, main="Empirical CDF for \nBinomial(n=5, p=0.6)")
text(x=0.8, y=0.9, labels=c("1,000 random draws"))
#dev.off()

########## 

exp.x <- 0
for(i in 0:5){ # compute E[X] for binom(n=5, p=0.6)
  exp.x <- exp.x + i*dbinom(x=i, size=5, prob=0.6)
} # end for loop
exp.x # equals 3

# turning this into a function:
binom.exp <- function(n, p){
  
  exp.x <- 0
  for(i in 0:n){ # compute E[X] for binom(n, p)
    exp.x <-  exp.x + i*dbinom(x=i, size=n, prob=p)	
  } # end for loop
  return(exp.x)	
} # end function

binom.exp(n=5, p=0.6) # also equals 3

# estimating the expected value using simulation:
mean(rbinom(n=10, size=5, prob=0.6))
mean(rbinom(n=100, size=5, prob=0.6))
mean(rbinom(n=1000, size=5, prob=0.6))

# higher number of draws --> higher sample size --> better estimate of mean


# binomial cdf:
pbinom(q=4, size=10, prob=0.5)           # P(X<=4) when n=10, p=0.5 (approx. 0.377)

# equivalent to computing: P(X=0)+P(X=1)+P(X=2)+P(X=3)+P(X=4)
dbinom(x=0, size=10, prob=0.5) + dbinom(x=1, size=10, prob=0.5) +
  dbinom(x=2, size=10, prob=0.5) + dbinom(x=3, size=10, prob=0.5) + 
  dbinom(x=4, size=10, prob=0.5)

# computing two cdf values at once
pbinom(q=c(12, 4), size=20, prob=0.75)   # P(X<=12), P(X<=4) when n=20, p=0.75

#### 
# binomial quantile function:
qbinom(p=0.5, size=15, prob=0.5)         # 50th percentile (median)  
#    when n=15, p=0.5
qbinom(p=c(0.2, 0.6), size=20, prob=0.6) # 20th and 60th percentiles  
#    when n=20, p=0.6
# 20% of distribution is at or below 
#    the 0.2 quantile
# 60% of distribution is at or below 
#    the 0.6 quantile





####################################
#  Normal Distribution Functions   #
####################################

dev.off() # clear previous plotting parameters

# mean and sd labeled
curve(dnorm(x, mean=0, sd=1), col="darkgreen", xlim=c(-3.5, 3.5), las=TRUE, lwd=2, 
      xaxt="n", yaxt="n", xlab="X",ylab="", main="Normal Distribution")
abline(v=0, col="black")
text(0.3, 0.05, labels=expression(mu), cex=1.3)
arrows(x0=0, y0=0.25, x1=1, y1=0.25, length=0.1)
text(1.3, 0.25, labels=expression(sigma), cex=1.3)


# normal pdf, assuming X has a normal distribution:
dnorm(x=2, mean=0, sd=1)   # density at x=2 for a N(0,1) distribution

# normal cdf, assuming X has a normal distribution:
pnorm(q=3, mean=0, sd=1)   # P(X <= 3) for a N(0,1) distribution

# normal quantile function, assuming X has a normal distribution:
qnorm(p=0.5, mean=0, sd=1) # 50th percentile (median) for a N(0,1) distribution

# simulating draws from a normal distribution
rnorm(n=50, mean=3, sd=5)  # simulating 50 draws from a N(3,5) distribution


# comparing theory and practice
y <- rnorm(n=1000, mean=0, sd=1)

par(mfrow=c(2,2)) # 2x2 figure with 4 plots max in 2 columns

###
# normal pdf, empirical pdf for N(0,1)

# empirical pdf
hist(y, freq=FALSE, las=TRUE, main="pdf for N(0,1)", ylim=c(0, 0.5)) 
# want density as y-axis not frequency so set freq=FALSE

# actual pdf for N(0,1)
curve(dnorm(x, mean=0, sd=1), col="blue", from=-4, to=4, add=TRUE, lwd=2, lty=2)

###
# normal cdf, empirical cdf for N(0,1)

# empirical cdf
plot(ecdf(y), las=TRUE, main="cdf for N(0,1)", lwd=2, xlim=c(-4, 4), col="limegreen")
curve(pnorm(q=x, mean=0, sd=1), from=-4, to=4, lty=2, lwd=2, col="blue", add=TRUE)
legend(-4.5, 0.9, legend=c("cdf", "ecdf"), col=c("blue", "limegreen"), lwd=2, lty=c(2, 1), bty="n")


###
#  normal quantile function, empirical quantile function for N(0,1)
plot(seq(0, 1, length=100), as.vector(quantile(y, probs=seq(0, 1, length=100))), lwd=2, las=TRUE, type="l", 
     main="quantile function for N(0,1)", xlab="quantile", ylab="value", col="limegreen")
curve(qnorm(p=x, mean=0, sd=1), from=0, to=1, lty=2, lwd=2, col="blue", add=TRUE)
legend("topleft", legend=c("quantile function", "emperical quantile function"), col=c("blue", "limegreen"), lwd=2, lty=c(2,1), bty="n")

####
# normal quantile plot
qqnorm(y, pch=20, las=TRUE)
qqline(y)



#########################################
# 3. PDFs, CDFs, and Quantile Functions #
#########################################

par(mfrow=c(1,3), mar=c(5, 5, 4, 2)+0.1)
# mar option changes the margins on plots; see ?par for more details

# plot pdf
curve(dnorm(x, mean=0, sd=1), main="pdf, f(x)", xlab="x", ylab="f(x)", las=TRUE,
      from=-4, to=4, cex.lab=1.6, cex.main=1.8, cex.axis=1.3, lwd=2)
x <- seq(-4, -0.5, length=100)
y <- dnorm(x, mean=0, sd=1)			
#axis(side=1, at=c(-4, -0.5))
polygon(c(-4, x, -0.5) , c(0, y, 0), col="cadetblue", border=NA, density=40)

# plot cdf
curve(pnorm(x, mean=0, sd=1), from=-4, to=4, main="cdf, F(x)", xlab="x", las=TRUE,
      ylab=expression(F(x)==P(X <= x)), cex.lab=1.6, cex.main=1.8, cex.axis=1.3, lwd=2)
abline(v=-0.5, lty=2, col="cadetblue")
abline(h=pnorm(-0.5), lty=2, col="cadetblue")	
points(-0.5, pnorm(-0.5), col="cadetblue", pch=20, cex=1.8)

# plot quantile function
curve(qnorm(x, mean=0, sd=1), from=0, to=1, las=TRUE, ylim=c(-4, 4), lwd=2,
      main=expression(paste("quantile function, ", Q(x)==F^{-1}(x), sep="")),
      xlab=expression(F(x)==P(X <= x)), ylab="x", cex.lab=1.6, cex.main=1.8, cex.axis=1.3)
abline(h=-0.5, lty=2, col="cadetblue")
abline(v=pnorm(-0.5), lty=2, col="cadetblue")	
points(pnorm(-0.5), -0.5, col="cadetblue", pch=20, cex=1.8)



############################
# 4. Replacing the Z-table #
############################

# P(X > 4) on N(6, 3^2)
pnorm(q=4, mean=6, sd=3, 
      lower.tail=FALSE)

# P(X <= 3) on N(2, 1)
pnorm(q=3, mean=2, sd=1)

# remember P(X <= x)=P(X<x)
# for continuous distributions

# area under the curve

dev.off()  # clearing previous plotting parameters

# draw pdf
curve(dnorm(x, mean=0, sd=1), col="firebrick", xlim=c(-5, 5), las=TRUE, lwd=2, ylim=c(0, 0.4),
      xlab="X", ylab="probability density, f(x)", 
      main=expression(paste("pdf for ", N(mu==0, sigma==1), sep="")))
# see ?plotmath for adding mathematical symbols to plots

# shade in area to compute P(-0.5 < X < 1.5)
x <- seq(-0.5, 1.5, length=100)
y <- dnorm(x, mean=0, sd=1)			
axis(side=1, at=c(-0.5, 1.5))
polygon(c(-0.5, x, 1.5) , c(0, y, 0), col="cadetblue", border=NA, density=40)

# legend
legend("topright", legend=c("P(-0.5 < X < 1.5)"), bty="n", cex=1.2, 
       density=40, fill="cadetblue")


#####################
# 5. Curve Function #
#####################

# examples
curve(dnorm(x, mean=3, sd=1), from=0, to=6, las=TRUE, 
      main="Density of Normal Distribution (mean=3, sd=1)", ylab="density")

# not great with discrete functions:
curve(dbinom(x, size=5, prob=0.6), from=0, to=5, las=TRUE,
      main="PMF of Binomial Distribution (n=5, p=0.6)", ylab="pmf")



########################################################
# 6. Normal Approximation to the Binomial Distribution #
########################################################


norm.approx.to.binom <- function(n.sim, n.trial, p){ # simulate binom data; plot results; add normal density
  
  # n.sim: number of draws from binomial distribution
  # n.trial: number of binomial trials
  # p: probability of success
  
  x <- rbinom(n=n.sim, size=n.trial, prob=p)
  
  # plot empirical pdf
  
  # y-axis should be tall enough to plot both the histogram and normal pdf
  # so maximum value of y should be larger of 
  #   (a) maximum bar height of the density histogram
  #   (b) maximum height of density function (which occurs at the mean)
  
  y.max <- max(c(max(hist(x, plot=FALSE)$density), dnorm(n.trial*p, mean=n.trial*p, sd=sqrt(n.trial*p*(1-p))) ))
  
  hist(x, freq=FALSE, las=TRUE, col="royalblue", 
       main=paste(n.trial, " draws from\nBinomial(n=", n.trial, ", p=",p, ")", sep=""),
       xlab="x", ylab="density", 
       ylim=c(0, y.max))
  
  # plot normal approximation pdf
  curve(dnorm(x, mean=n.trial*p, sd=sqrt(n.trial*p*(1-p))), from=min(x), to=max(x), 	
        add=TRUE, lwd=2, col="firebrick")
  
  return(0)
} # end function


# using the function -- as n.trial gets larger and larger

par(mfrow=c(2,2))
norm.approx.to.binom(n.sim=1000, n.trial=10, p=0.2)
norm.approx.to.binom(n.sim=1000, n.trial=100, p=0.2)
norm.approx.to.binom(n.sim=1000, n.trial=500, p=0.2)
norm.approx.to.binom(n.sim=1000, n.trial=1000, p=0.2)


############################
#    Saving Your Workspace #
############################

# set your working directory so you know where your R obejcts will be saved

# run code
x <- rnorm(n=1000, mean=0, sd=5)
x[1:10]
y <- rbinom(n=500,  size=10, prob=0.6)
y[1:10]

ls() # list objects currently in your workspace

# save objects currently in your workspace into a file
# file name MUST include .RData at the end
save.image("Lecture_8_objects.RData")

# shut down RStudio and re-start RStudio

####

# set working directory

# load objects created in "Lecture_8_objects.RData"
load("Lecture_8_objects.RData")

ls() # check whether objects loaded








