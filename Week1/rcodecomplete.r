####################################################
# Chapter-by-chapter R code for Reasoning with Data
# by Jeffrey Stanton
# Version dated November 26, 2016


# Introduction

install.packages("modeest") 
library("modeest")


#####################################
# Chapter 1

rainfall <- c(0,0,0,2,1,1,4) 		# Amount of rain each day this week
sum(rainfall) / length(rainfall) 	# Compute the mean the hard way
mean(rainfall)			# Use a function to compute the mean

# Only if you did not do this in the Introduction
#install.packages("modeest") 	# Download the mode estimation package
#library(modeest)			# Make the package ready to use
mfv(rainfall)				# mfv stands for most frequent value  

votes <- c(200,300,400)				# Here is scenario one 
(votes - mean(votes)) ^ 2				# Show a list of squared deviations
sum( (votes - mean(votes)) ^ 2)			# Add them together
sum( (votes - mean(votes)) ^ 2) / length(votes)	# Divide by the number of observations

votes1 <- c(200,300,400)				# Here is scenario one again
sqrt( sum((votes1 - mean(votes1))^2) / length(votes1) ) # That is the standard deviation
votes2 <- c(299,300,301)				# Here is scenario two
sqrt( sum((votes2 - mean(votes2))^2) / length(votes2) ) # And the same for candidate 2

# The commented code makes a high res plot when run in R
#png("Figure01_1.png", width = 6, height = 6, units = 'in', res = 300)
hist( rnorm(n=1000, mean=100, sd=10), main=NULL )
#dev.off()

# This shows a finer-grained histogram with 100 categories or “breaks”
# Try increasing the n and the breaks to see what happens
hist( rnorm(n=10000, mean=100, sd=10), breaks=100 )

#png("Figure01_2.png", width = 6, height = 6, units = 'in', res = 300)
hist(rpois(n=1000, lambda=1), main=NULL)
#dev.off()

mean(rpois(n=1000, lambda=1))

myPoiSample <- rpois(n=1000, lambda=1)
mean(myPoiSample)
hist(myPoiSample)



#####################################
# Chapter 2

table(rbinom(n=100,size=6,prob=0.5))

hist(rbinom(n=100,size=6,prob=0.5))

#png("Figure02_1.png", width = 6, height = 6, units = 'in', res = 300)
hist(rbinom(n=1000,size=6,prob=0.5), main=NULL)
#dev.off()

#png("Figure02_2.png", width = 6, height = 6, units = 'in', res = 300)
barplot(table(rbinom(n=1000,size=6,prob=0.5)), main=NULL)
#dev.off()

table(rbinom(n=1000,size=6,prob=0.5))/1000
#png("Figure02_3.png", width = 6, height = 6, units = 'in', res = 300)
barplot(table(rbinom(n=1000,size=6,prob=0.5))/1000, main=NULL)
#dev.off()

probTable <- table(rbinom(n=1000,size=6,prob=0.5))/1000
probTable
cumsum(probTable)

probTable <- table(rbinom(n=1000,size=6,prob=0.5))/1000
#png("Figure02_4.png", width = 6, height = 6, units = 'in', res = 300)
barplot(cumsum(probTable),main=NULL)
#dev.off()

probTable <- table(rbinom(n=10000,size=100,prob=0.5))/10000
barplot(probTable)
barplot(cumsum(probTable))

# Sidebar 2.1: Create Your Own Tables with R
#
toast <- matrix(c(2,1,3,4),ncol=2,byrow=TRUE) # Create a two column structure using the matrix() command
colnames(toast) <- c("Down","Up") # Label the columns
rownames(toast) <- c("Jelly","Butter") # Label the rows
toast <- as.table(toast) # Convert from metric to table
toast # Show the table on the console
margin.table(toast) # This is the grand total of toast drops
margin.table(toast,1) # These are the marginal totals for rows
margin.table(toast,2) # These are the marginal totals for columns
toastProbs <- toast/margin.table(toast) # Calculate probabilities
toastProbs # Report probabilities to console




#####################################
# Chapter 3

toastAngleData <- runif(1000,0,180)	# Random numbers from uniform distribution
head(toastAngleData)			# Look at the first few numbers in the list
tail(toastAngleData)			# Look at the last few numbers in the list
hist(toastAngleData)			# Plot a histogram of all 1000 data points

sample(toastAngleData,size=14,replace=TRUE)

mean(sample(toastAngleData,size=14,replace=TRUE))

replicate(4, mean(sample(toastAngleData,size=14,replace=TRUE)))

samplingDistribution <- replicate(10000,mean(sample(toastAngleData,size=14,replace=TRUE)))
#png("Figure03_1.png", width = 6, height = 6, units = 'in', res = 300)
hist(samplingDistribution, main=NULL)
#dev.off()

mean(samplingDistribution)
mean(toastAngleData)

summary(samplingDistribution)

quantile(samplingDistribution, c(.01, .05,  .50, .95, .99))

#png("Figure03_2.png", width = 6, height = 6, units = 'in', res = 300)
hist(samplingDistribution, main=NULL)
abline(v=quantile(samplingDistribution,.01))
abline(v=quantile(samplingDistribution,.05))
abline(v=quantile(samplingDistribution,.95))
abline(v=quantile(samplingDistribution,.99))
#dev.off()

samplingDistribution[ samplingDistribution<= quantile(samplingDistribution,.01) ]
summary( samplingDistribution[samplingDistribution <= quantile(samplingDistribution,.01)] )


#####################################
# Chapter 4

# Descriptive statistics and box plot for mtcars
mean( mtcars$mpg[ mtcars$am == 0 ] )		# Automatic transmissions
mean( mtcars$mpg[ mtcars$am == 1 ] )		# Manual transmissions

sd( mtcars$mpg[ mtcars$am == 0 ] )		# Automatic transmissions
sd( mtcars$mpg[ mtcars$am == 1 ] )		# Manual transmissions

#png("Figure04_1.png", width = 6, height = 6, units = 'in', res = 300)
boxplot(mpg ~ am, data=mtcars, main=NULL) # Boxplot of mpg, grouped by am
#dev.off()

mean( sample(mtcars$mpg[ mtcars$am == 0 ],size=19,replace=TRUE) )
mean( sample(mtcars$mpg[ mtcars$am == 1 ],size=13,replace=TRUE) )

mean( sample(mtcars$mpg[ mtcars$am == 0 ],size=19,replace=TRUE) ) - mean( sample(mtcars$mpg[ mtcars$am == 1 ],size=13,replace=TRUE) )


meanDiffs <- replicate(100, mean( sample(mtcars$mpg[ mtcars$am == 0 ],size=19,replace=TRUE) ) - mean( sample(mtcars$mpg[ mtcars$am == 1 ],size=13,replace=TRUE) ))

#png("Figure04_2.png", width = 6, height = 6, units = 'in', res = 300)
hist(meanDiffs, main=NULL)
#dev.off()

quantile(meanDiffs, c(0.025, 0.975))

t.test(mtcars$mpg[mtcars$am==0] ,mtcars$mpg[mtcars$am==1])

install.packages("animation")
library(animation)
conf.int(level=0.95)

#####################################
# Chapter 5

# Don't forget to install JAGS first
# Visit: http://mcmc-jags.sourceforge.net

install.packages("BEST")
library(BEST)
carsBest <- BESTmcmc(mtcars$mpg[mtcars$am==0] ,mtcars$mpg[mtcars$am==1])


#png("Figure05_1.png", width = 6, height = 6, units = 'in', res = 300)
plot(carsBest, main=NULL)
#dev.off()                  

t.test(mtcars$mpg[mtcars$am==0] ,mtcars$mpg[mtcars$am==1])


install.packages("effsize")
library(effsize)
cohen.d(mtcars$mpg[mtcars$am==0] ,mtcars$mpg[mtcars$am==1])

set.seed(54321) # Control randomization
carsTdist <- rt(n=10000,df=18.332) # 10,000 random t values
hist(carsTdist) # Show in a histogram
lowTvalues <- carsTdist[carsTdist <= -3.7671] # Here is the lower tail
hiTvalues <- carsTdist[carsTdist >= 3.7671] # Here is the upper tail
length(lowTvalues) + length(hiTvalues) # The number of observations in the tails

# This is the t-command used at the end of the exercises
t.test(rnorm(100000,mean=17.1,sd=3.8),rnorm(100000,mean=17.2,sd=3.8))




#####################################
# Chapter 6


set.seed(1)
pgrp1 <- sample(precip,20, replace=TRUE)
pgrp2 <- sample(precip,20, replace=TRUE)
pgrp3 <- sample(precip,20, replace=TRUE)

var(c(pgrp1,pgrp2,pgrp3))
var(precip)

mean(pgrp1)			# Examine the means of the three groups
mean(pgrp2)
mean(pgrp3)
barplot(c(mean(pgrp1),mean(pgrp2),mean(pgrp3)))	# Create a bar plot of the means
var(c(mean(pgrp1),mean(pgrp2),mean(pgrp3)))		# Variance among the means

pgrp3 <- pgrp3 - 5		# Take away five inches of rain from each point in sample 3

#png("Figure06_1.png", width = 6, height = 6, units = 'in', res = 300)
barplot(c(mean(pgrp1),mean(pgrp2),mean(pgrp3)), main=NULL)	# Bar plot of the new means
#dev.off()

var(c(mean(pgrp1),mean(pgrp2),mean(pgrp3)))
var(c(pgrp1,pgrp2,pgrp3))

randomFs <- rf(n=100,df1=2,df2=57)
#png("Figure06_2.png", width = 6, height = 6, units = 'in', res = 300)
hist(randomFs, main=NULL)
#dev.off()


# Run ANOVA on groups sampled from the same population
set.seed(10)  						# Control the randomization
precipAmount <- sample(precip,60,replace=TRUE) 	# Enough for 3 groups of 20
precipGrp <- as.factor(rep(seq(from=1,to=3,by=1),20)) # Group designators, 3 groups
precipDF <- data.frame(precipAmount, precipGrp) 	# Put everything in data frame
#png("Figure06_3.png", width = 6, height = 6, units = 'in', res = 300)
boxplot(precipAmount ~ precipGrp, data=precipDF, main=NULL) # Get a box plot of the distribs
#dev.off()
precipOut <- aov(precipAmount ~ precipGrp, data=precipDF) # Run the ANOVA
summary(precipOut) 					# Provide an ANOVA table

# Uses the density function to show the shape of different F-distributions
fVals <- seq(from=0.01,to=5,by=0.01)
plot(fVals,df(fVals,df1=2,df2=57))
points(fVals,df(fVals,df1=3,df2=57))
points(fVals,df(fVals,df1=4,df2=57))

hist(precipOut$residuals)  # Summarize the residuals


# This is the code for Sidebar 6.1
install.packages('gtools') #install gtools to get permutations
library(gtools) # Make the package ready
tinyPop <- c(1,2,3) # Here is a tiny population
allSamp <- permutations(n=3,r=3,v=tinyPop,repeats.allowed=T)
allSamp					# Verify: 27 unique samples
apply(allSamp,1,var) # List the sample variance of each sample
mean(apply(allSamp,1,var)) # What is the mean of those variances

# Code for sidebar 6.2
install.packages("BEST")
library(BEST)
data(mtcars)
priorList <- list(muM = c(20,20), muSD = c(4,4))
carsBest2 <- BESTmcmc(mtcars$mpg[mtcars$am==0] ,mtcars$mpg[mtcars$am==1],priors=priorList) 
plot(carsBest2, main=NULL) 


# Code for Bayesian ANOVA
install.packages("BayesFactor") 
library("BayesFactor") 


precipBayesOut <- anovaBF(precipAmount ~ precipGrp, data=precipDF) # Calc Bayes Factors
mcmcOut <- posterior(precipBayesOut,iterations=10000)  # Run mcmc iterations
#png("Figure06_4.png", width = 6, height = 6, units = 'in', res = 300)
plot(mcmcOut[,"mu"], main=NULL) 	# Show the range of values for the grand mean
#dev.off()

# Display a 95% HDI for precipitation group 1
#png("Figure06_5.png", width = 6, height = 6, units = 'in', res = 300)
par(mfcol=c(1,1))
hist(mcmcOut[,"precipGrp-1"], main=NULL)
abline(v=quantile(mcmcOut[,"precipGrp-1"],c(0.025)), col="black")
abline(v=quantile(mcmcOut[,"precipGrp-1"],c(0.975)), col="black")
#dev.off()
print("95% HDI lower bound: ") 
print(quantile(mcmcOut[,"precipGrp-1"],c(0.025)))
print("95% HDI upper bound: ") 
print(quantile(mcmcOut[,"precipGrp-1"],c(0.975)))



# Plot the posteriors for the three groups in a boxplot
#png("Figure06_6.png", width = 6, height = 6, units = 'in', res = 300)
boxplot(as.matrix(mcmcOut[,2:4]), main=NULL) # Boxplot the posteriors for the groups
#dev.off()

plot(mcmcOut[,1:6])



# Uncomment to show the other groups
# plot(mcmcOut[,"precipGrp-2"]) 	# Show the range of values for the second group
# plot(mcmcOut[,"precipGrp-3"]) 	# Show the range of values for the third group

precipBayesOut

# Analyze chickwt data that contains multiple groups with some mean differences
data(chickwts)
chicksOut <- aov(weight ~ feed, data=chickwts) # Run the ANOVA
summary(chicksOut)

chicksBayesOut <- anovaBF(weight ~ feed, data=chickwts) # Calc Bayes Factors
mcmcOut2 <- posterior(chicksBayesOut,iterations=10000)  # Run mcmc iterations
#png("Figure06_7.png", width = 6, height = 6, units = 'in', res = 300)
par(mar=c(7,4,4,2),las=2) # Make a little more room for the labels
boxplot(as.matrix(mcmcOut2[,2:7]), main=NULL) # Boxplot the posteriors for the groups
par(mar=c(5,4,4,2),las=0) # Reset the margins
#dev.off()
summary(mcmcOut2)

# Sample post hoc comparison of sunflower and meatmeal
plot(BESTmcmc(chickwts[chickwts$feed=="sunflower",1],chickwts[chickwts$feed=="meatmeal",1]))

chicksBayesOut


#####################################
# Chapter 7


set.seed(12345)
wood <- rnorm(24)
heat <- rnorm(24)
mean(wood)
mean(heat)
sd(wood)
sd(heat)

#png("Figure07_1.png", width = 6, height = 6, units = 'in', res = 300)
plot(wood,heat, main=NULL) 
#dev.off()

#png("Figure07_2.png", width = 6, height = 6, units = 'in', res = 300)
plot(wood,(wood-mean(wood)), main=NULL)
#dev.off()

cpWH <- wood * heat
#png("Figure07_3.png", width = 6, height = 6, units = 'in', res = 300)
hist(cpWH, main=NULL)
#dev.off()
mean(cpWH)

# Make a new, fake version of heat that will correlate with wood
newHeat <- wood/1.41 + heat/1.41   	# Make a mixture of the two old variables
mean(newHeat)				# What's the mean of our new heat variable?
sd(newHeat) 					# What's the sd of our new heat variable?

cpWnewH <- wood * newHeat
#png("Figure07_4.png", width = 6, height = 6, units = 'in', res = 300)
hist(cpWnewH, main=NULL)
#dev.off()
mean(cpWnewH)

#png("Figure07_5.png", width = 6, height = 6, units = 'in', res = 300)
plot(wood,newHeat, main=NULL) 
#dev.off()

cor(wood,newHeat)


set.seed(12345) 			# Start with a random number seed
wood <- rnorm(2400) 		# Make two vectors of N=2400
heat <- rnorm(2400)
fireDF <- data.frame(wood, heat) 	# Put them in a dataframe
nrow(fireDF) 				# Verifying 2400 rows of two variables
fireDF[sample(nrow(fireDF), 24), ] # Generates one sample of n=24  

cor(fireDF[sample(nrow(fireDF), 24), ])
cor(fireDF[sample(nrow(fireDF), 24), ])[1,2]

corDist <- replicate(5000,cor(fireDF[sample(nrow(fireDF), 24), ])[1,2])
#png("Figure07_6.png", width = 6, height = 6, units = 'in', res = 300)
hist(corDist, main=NULL)
#dev.off()
mean(corDist)

newHeat <- wood/1.41 + heat/1.41
newfireDF <- data.frame(wood, newHeat) # Put them in a dataframe
newcorDist <- replicate(5000,cor(newfireDF[sample(nrow(newfireDF), 24), ])[1,2],simplify=TRUE)
#png("Figure07_7.png", width = 6, height = 6, units = 'in', res = 300)
hist(newcorDist, main=NULL)
#dev.off()
mean(newcorDist)

# Conduct a null hypothesis test on one correlation
set.seed(12345)
wood <- rnorm(24)
heat <- rnorm(24)
cor.test(wood,heat)

cor.test(wood,(wood/1.41 + heat/1.41))

cor.test(iris[,"Sepal.Width"],iris[,"Petal.Width"])


install.packages("BayesFactor")
library("BayesFactor")

bfCorTest <- function (x,y) # Get r from BayesFactor
{
  zx <- scale(x) # Standardize X
  zy <- scale(y) # Standardize Y
  zData <- data.frame(x=zx,rhoNot0=zy) # Put in a data frame
  bfOut <- generalTestBF(x ~ rhoNot0, data=zData) # linear coefficient
  mcmcOut <- posterior(bfOut,iterations=10000) # posterior samples
  print(summary(mcmcOut[,"rhoNot0"])) # Get the HDI for r
  return(bfOut) # Return Bayes factor object
}


set.seed(12345)
wood <- rnorm(24)
heat <- rnorm(24)
bfCorTest(wood,heat)

newHeat <- wood/1.41 + heat/1.41
bfCorTest(newHeat, wood)

bfCorTest(iris[,"Sepal.Width"],iris[,"Petal.Width"])


# Chi-square section

make2x2table <- function(ul) # The user supplies the count for the upper left cell
{
  ll <- 50 - ul # Calculate the lower left cell
  ur <- 30 - ul # Calculate the upper right cell
  lr <- 50 - ur # Calculate the lower right cell
  
  # Put all of the cells into a 2x2 matrix
  matrix(c(ul,ur,ll,lr), nrow=2, ncol=2, byrow=TRUE)
}

make2x2table(15) # Should be like Table 7.2
make2x2table(0)   # Should be like Table 7.3
make2x2table(30) 	# Should be like Table 7.4

calcChiSquared <- function(actual, expected) # Calculate chi-squared
{
  diffs <- actual - expected        	# Take the raw difference for each cell
  diffsSq <- diffs ^ 2              	# Square each cell
  diffsSqNorm <- diffsSq / expected # Normalize with expected cells
  
  sum(diffsSqNorm)                 	 # Return the sum of the cells
}

# This makes a matrix that is just like Table 7.2
# This table represents the null hypothesis of independence
expectedValues <- matrix(c(15,15,35,35), nrow=2, ncol=2, byrow=TRUE)

calcChiSquared(make2x2table(15),expectedValues)
calcChiSquared(make2x2table(0),expectedValues)
calcChiSquared(make2x2table(30),expectedValues)

set.seed(12)
mean(rbinom(1000,30,prob=0.5))
#png("Figure07_8.png", width = 6, height = 6, units = 'in', res = 300)
hist(rbinom(1000,30,prob=0.5), main=NULL)
#dev.off()

chiDist <- replicate(100000,calcChiSquared(make2x2table(rbinom(n=1,size=30,prob=0.5)),expectedValues))

#png("Figure07_9.png", width = 6, height = 6, units = 'in', res = 300)
hist(chiDist, main=NULL)
#dev.off()

quantile(chiDist,probs=c(0.95))

calcChiSquared(make2x2table(20),expectedValues)

calcChiSquared(make2x2table(10),expectedValues)

# Run the chi-square test on Table 7.1 data
chisq.test(make2x2table(20), correct=FALSE)

# Run the chi-square test on Table 7.1 data
# data(Titanic) # Should not need this
badBoatMF <- ftable(Titanic, row.vars=2, col.vars="Survived")
badBoatMF
chisq.test(badBoatMF, correct=FALSE)


# Bayesian approach using BayesFactor package

ctBFout <- contingencyTableBF(make2x2table(20),sampleType="poisson",posterior=FALSE)
ctBFout

ctMCMCout <- contingencyTableBF(make2x2table(20),sampleType="poisson",posterior=TRUE,iterations=10000)
summary(ctMCMCout)
downProp <- ctMCMCout[,"lambda[1,1]"]/ctMCMCout[,"lambda[2,1]"]
#png("Figure07_10.png", width = 6, height = 6, units = 'in', res = 300)
hist(downProp, main=NULL)
#dev.off()

upProp <- ctMCMCout[,"lambda[1,2]"]/ctMCMCout[,"lambda[2,2]"]
#png("Figure07_11.png", width = 6, height = 6, units = 'in', res = 300)
hist(upProp, main=NULL)
#dev.off()

diffProp <- downProp-upProp
#png("Figure07_12.png", width = 6, height = 6, units = 'in', res = 300)
hist(diffProp, main=NULL)
abline(v=quantile(diffProp,c(0.025)), col="black")
abline(v=quantile(diffProp,c(0.975)), col="black")
#dev.off()

mean(diffProp)

badBoatMF <- ftable(Titanic, row.vars=2, col.vars="Survived")
ctBFout <- contingencyTableBF(badBoatMF,sampleType="poisson",posterior=FALSE)
ctBFout

ctMCMCout <- contingencyTableBF(badBoatMF,sampleType="poisson",posterior=TRUE,iterations=10000)
summary(ctMCMCout)
maleProp <- ctMCMCout[,"lambda[1,1]"]/ctMCMCout[,"lambda[1,2]"]
femaleProp <- ctMCMCout[,"lambda[2,1]"]/ctMCMCout[,"lambda[2,2]"]
diffProp <- maleProp - femaleProp
hist(diffProp)
mean(diffProp)
abline(v=quantile(diffProp,c(0.025)), col="black")
abline(v=quantile(diffProp,c(0.975)), col="black")

  
#####################################
# Chapter 8

set.seed(321)
hardwork <- rnorm(120)
basicsmarts <- rnorm(120)
curiosity <- rnorm(120)

randomnoise <- rnorm(120)
gpa <- hardwork/2 + basicsmarts/2 + curiosity/2 + randomnoise/2
sd(gpa)

#png("Figure08_1.png", width = 6, height = 6, units = 'in', res = 300)  
plot(hardwork, gpa, main=NULL)
#dev.off()

#png("Figure08_2.png", width = 6, height = 6, units = 'in', res = 300)  
plot(hardwork,gpa, main=NULL)
abline(a=0, b=0.56)
#dev.off()

arrows(min(hardwork),gpa[which.min(hardwork)],min(hardwork),min(hardwork)*0.56)

#png("Figure08_3.png", width = 6, height = 6, units = 'in', res = 300)  
hist(gpa - (hardwork * 0.56), main=NULL)
#dev.off()
sum(gpa - (hardwork * 0.56))


calcSQERR <- function(dv, iv, slope)
{
  (dv - (iv*slope))^2
}

head(calcSQERR(gpa,hardwork,0.56))
sum(calcSQERR(gpa,hardwork,0.56))

#png("Figure08_4.png", width = 6, height = 6, units = 'in', res = 300)  
hist(calcSQERR(gpa,hardwork,0.56), main=NULL)
#dev.off()

sumSQERR <- function(slope)
{
  sum(calcSQERR(gpa, hardwork, slope))
}

sumSQERR(0.56)

trySlopes <- seq(from=0, to=1, length.out=40)
sqerrList <- sapply(trySlopes, sumSQERR)
#png("Figure08_5.png", width = 6, height = 6, units = 'in', res = 300)  
plot(trySlopes, sqerrList, main=NULL)
#dev.off()

educdata <- data.frame(gpa, hardwork, basicsmarts, curiosity)
regOut <- lm(gpa ~ hardwork, data=educdata)
summary(regOut)

regOut3 <- lm(gpa ~ hardwork + basicsmarts + curiosity, data=educdata)
summary(regOut3)

#png("Figure08_6.png", width = 6, height = 6, units = 'in', res = 300) 
plot(randomnoise,regOut3$residuals, main=NULL)
#dev.off()

summary(residuals(regOut3))
cor(educdata)


regOutMCMC <- lmBF(gpa ~ hardwork + basicsmarts + curiosity, data=educdata, posterior=TRUE, iterations=10000)
summary(regOutMCMC)


#png("Figure08_7.png", width = 6, height = 6, units = 'in', res = 300) 
hist(regOutMCMC[,"hardwork"], main=NULL)
abline(v=quantile(regOutMCMC[,"hardwork"],c(0.025)), col="black")
abline(v=quantile(regOutMCMC[,"hardwork"],c(0.975)), col="black")
#dev.off()

rsqList <- 1 - (regOutMCMC[,"sig2"] / var(gpa))
# length(rsqList) 				# Confirms 10000 R-squared estimates
mean(rsqList) 				# Overall mean R-squared is 0.75
#png("Figure08_8.png", width = 6, height = 6, units = 'in', res = 300) 
hist(rsqList, main=NULL)
abline(v=quantile(rsqList,c(0.025)), col="black")
abline(v=quantile(rsqList,c(0.975)), col="black")
#dev.off()

regOutBF <- lmBF(gpa ~ hardwork + basicsmarts + curiosity, data=educdata)
regOutBF

# Real example using state.x77 data
stateData <- data.frame(state.x77)
stateOut <- lm(Life.Exp ~ HS.Grad + Income + Illiteracy,data=stateData)
summary(stateOut)

stateOutMCMC <- lmBF(Life.Exp ~ HS.Grad + Income + Illiteracy,data=stateData, posterior=TRUE, iterations=100000)
summary(stateOutMCMC)
rsqList <- 1 - (stateOutMCMC[,"sig2"] / var(stateData$Life.Exp))
mean(rsqList) 				# Overall mean R-squared
quantile(rsqList,c(0.025))
quantile(rsqList,c(0.975))

#hist(stateData$Illiteracy) # Histogram of raw data
#hist(stateOutMCMC[,"Illiteracy"]) # Posterior distribution of B weight
#boxplot(as.numeric(stateOutMCMC[,"Illiteracy"])) # Posterior distribution of B weight

stateOutBF <- lmBF(Life.Exp ~ HS.Grad + Income + Illiteracy,data=stateData)
stateOutBF

# Code for bonus homework item
set.seed(1)
betaVar <- scale(rbeta(50,shape1=1,shape2=10))
normVar <- rnorm(50)
poisVar <- scale(rpois(50,lambda=10))
noiseVar <- scale(runif(50))
depVar <- betaVar/2 + normVar/2 + poisVar/2 + noiseVar/2
oddData <- data.frame(depVar,betaVar,normVar,poisVar)

summary(lm(depVar ~ .,data=oddData))



#####################################
# Chapter 9


install.packages("HSAUR")
library("HSAUR")
data("weightgain", package = "HSAUR")

# Create an interaction plot
wg <- weightgain
#png("Figure09_1.png", width = 7, height = 6, units = 'in', res = 300) 
interaction.plot(x.factor=wg$source,trace.factor=wg$type,response=wg$weightgain)
#dev.off()

aovOut = aov(weightgain ~ source + type + source:type, data=weightgain)
aovOut2 = aov(weightgain ~ source * type, data=weightgain)

# Code for Sidebar 9.1
testF <- rf(n=10000, df1=1, df2=36)	# Generate random Fs for F(1,36)
hist(testF)					# Display a histogram
abline(v=quantile(testF,c(0.95)),col="black")		# Show threshold of significance, p<.05
quantile(testF,c(0.95))			# Report the threshold to the console
       
# Back to chapter 9 code. . .
aovOut3 = anovaBF(weightgain ~ source*type, data=weightgain)
aovOut3
aovOut3[4]/aovOut3[3]      # What's the odds ratio of model 4 vs. model 3?

mcmcOut <- posterior(aovOut3[4],iterations=10000)  # Run mcmc iterations
# summary(mcmcOut) # Review detailed posterior distributions

#png("Figure09_2.png", width = 7, height = 6, units = 'in', res = 300) 
boxplot(as.matrix(mcmcOut[,2:5]), main=NULL) # Figure 9.2
#dev.off()
#png("Figure09_3.png", width = 6, height = 6, units = 'in', res = 300) 
boxplot(as.matrix(mcmcOut[,6:7]), main=NULL) # Figure 9.3
#dev.off()



install.packages("gplots")
library("gplots")

#png("Figure09_4.png", width = 6, height = 6, units = 'in', res = 300) 
plotmeans(weightgain ~ interaction(source,type,sep =" "), data=weightgain,connect=list(c(1,2),c(3,4)))
#dev.off()



# Show regression lines on a scatterplot of radiation vs. ozone
install.packages("lattice")
library(lattice)
data(environmental)

#png("Figure09_5.png", width = 6, height = 6, units = 'in', res = 300) 
plot(environmental$radiation,environmental$ozone)

# This grey line shows what happens when we only consider
# the data with wind speeds above the median wind speed
hiWind <- subset(environmental, wind > median(environmental$wind))
hiLmOut <- lm(ozone ~ radiation,data=hiWind)
abline(hiLmOut,col="grey")

# This dotted black line shows what happens when we only consider
# the data with wind speeds at or below the median wind speed
loWind <- subset(environmental, wind <= median(environmental$wind))
loLmOut <- lm(ozone ~ radiation,data=loWind)
abline(loLmOut,col="black",lty=3)
#dev.off()

lmOut1 <- lm(ozone ~ radiation * wind, data=environmental)
summary(lmOut1)

#png("Figure09_6.png", width = 6, height = 6, units = 'in', res = 300) 
plot(environmental$radiation,residuals(lmOut1)) 
abline(h=0)
#dev.off()

#png("Figure09_7.png", width = 6, height = 6, units = 'in', res = 300) 
plot(environmental$ozone,residuals(lmOut1)) 
abline(h=0)
#dev.off()

# Sidebar 9.3 code
plot(environmental$radiation,environmental$ozone)
env <- environmental
env$radSqr <- env$radiation^2
lmOutQuad <- lm(ozone ~ radiation + wind + radSqr + radiation:wind, data=env)
summary(lmOutQuad)
pairs(environmental,panel=panel.smooth)


# Rerun the analysis with centered variables
stdenv <- data.frame(scale(environmental,center=TRUE,scale=FALSE))
lmOut2 <- lm(ozone ~ radiation * wind,data=stdenv)
summary(lmOut2)

lmOutSimple <- lm(ozone ~ radiation + wind,data=stdenv)
lmOutInteract <- lm(ozone ~ radiation + wind + radiation:wind,data=stdenv)
install.packages("lmSupport")
library(lmSupport)
modelCompare(lmOutSimple, lmOutInteract)


lmOutBayes1 <- lmBF(ozone ~ radiation + wind,data=stdenv)
lmOutBayes2 <- lmBF(ozone ~ radiation + wind + radiation:wind,data=stdenv)
lmOutBayes2/lmOutBayes1

mcmcOut <- lmBF(ozone ~ radiation + wind + radiation:wind,data=stdenv, posterior=TRUE,iterations=10000)
summary(mcmcOut)
rsqList <- 1 - (mcmcOut[,"sig2"] / var(stdenv$ozone))
mean(rsqList) 				# Overall mean R-squared
quantile(rsqList,c(0.025))
quantile(rsqList,c(0.975))

loWind$wind <- mean(loWind$wind) - sd(loWind$wind)
hiWind$wind <- mean(hiWind$wind) + sd(hiWind$wind)
loWindOzone <- modelPredictions(lmOut1, Data=loWind, Type = 'response')
hiWindOzone <- modelPredictions(lmOut1, Data=hiWind, Type = 'response')
#png("Figure09_8.png", width = 6, height = 6, units = 'in', res = 300) 
plot(loWind$radiation,loWindOzone$Predicted,xlim=c(0,350),ylim=c(10,90))
points(hiWind$radiation,hiWindOzone$Predicted,pch=3)
#dev.off()





#####################################
# Chapter 10


# Create a sequence of 100 numbers, ranging from -6 to 6 to serve as the X variable
logistX <- seq(from=-6, to=6, length.out=100)

# Compute the logit function using exp(), the inverse of log()
logistY <- exp(logistX)/(exp(logistX)+1)

#png("Figure10_1.png", width = 6, height = 6, units = 'in', res = 300) 
# Now review the beautiful S curve
plot(logistX,logistY)
#dev.off()


# Create a random, standard-normal predictor variable
set.seed(123)
logistX <- rnorm(n=100,mean=0,sd=1)

# Create an outcome variable as a logit function of the predictor
logistY <- exp(logistX)/(exp(logistX)+1)

# Make the dichotomous/binomial version of the outcome variable
binomY <- round(logistY)

# Add noise to the predictor so that it does not perfectly predict the outcome
logistX <- logistX/1.41 + rnorm(n=100,mean=0,sd=1)/1.41

#png("Figure10_2.png", width = 6, height = 6, units = 'in', res = 300) 
plot(logistX, binomY)
#dev.off()

binomY <- factor(round(logistY), labels=c('Truth','Lie'))
logistDF <- data.frame(logistX, logistY, binomY) # Make data frame

#png("Figure10_3.png", width = 6, height = 6, units = 'in', res = 300) 
boxplot(formula=logistX ~ binomY, data=logistDF, ylab="GSR", main=NULL)
#dev.off()

glmOut <- glm(binomY ~ logistX, data=logistDF, family=binomial())
summary(glmOut)

mean(residuals(glmOut))
hist(residuals(glmOut))

exp(coef(glmOut)) # Convert log odds to odds
exp(confint(glmOut)) # Look at confidence intervals around log-odds


anova(glmOut, test="Chisq")  # Compare null model to one predictor model


#png("Figure10_4.png", width = 6, height = 6, units = 'in', res = 300) 
par(mfrow=c(1,2))                                    # par() configures the plot area
plot(binomY, predict(glmOut),ylim=c(-4,4)) # Compare with earlier plot
plot(binomY, logistX,ylim=c(-4,4))
#dev.off()


# Logistic regression with a real data example

install.packages("car")
library(car)
data(Chile)

ChileY <- Chile[Chile$vote == "Y",] # Grab the Yes votes
ChileN <- Chile[Chile$vote == "N",] # Grab the No votes
ChileYN <- rbind(ChileY,ChileN) # Make a new dataset with those
ChileYN <- ChileYN[complete.cases(ChileYN),] # Get rid of missing data
ChileYN$vote <- factor(ChileYN$vote,levels=c("N","Y")) # Fix the factor
# dim(ChileYN)
# table(ChileYN$vote)

#png("Figure10_5.png", width = 6, height = 6, units = 'in', res = 300) 
par(mfrow=c(1,2))      
boxplot(age ~ vote, data=ChileYN, main=NULL)
boxplot(income ~ vote, data=ChileYN)
#dev.off()

chOut <- glm(formula = vote ~ age + income, family = binomial(), data = ChileYN)

# Intercept only model
#chOut <- glm(formula = vote ~ 1, family = binomial(), data = ChileYN)

summary(chOut)
exp(coef(chOut)) # Convert log odds to odds
exp(confint(chOut)) # Look at confidence intervals
anova(chOut, test="Chisq")   # Compare null model to predictor models

# ChileYN$vote <- ChileYN$vote/10


install.packages("BaylorEdPsych")
library(BaylorEdPsych)
PseudoR2(chOut)

# hist(predict(chOut,type="response"))
table(round(predict(chOut,type="response")),ChileYN$vote)


# Now do it the Bayesian way

install.packages("MCMCpack")    # Download MCMCpack package
library(MCMCpack) # Load the package 
ChileYN$vote <- as.numeric(ChileYN$vote) - 1 # Adjust the outcome variable
bayesLogitOut <- MCMClogit(formula = vote ~ age + income, data = ChileYN)
summary(bayesLogitOut) # Summarize the results

#png("Figure10_6.png", width = 6, height = 6, units = 'in', res = 300) 
plot(bayesLogitOut)
#dev.off()

exp(mean(bayesLogitOut[,"age"]))
exp(quantile(bayesLogitOut[,"age"],c(0.025)))
exp(quantile(bayesLogitOut[,"age"],c(0.975)))

ageLogOdds <- as.matrix(bayesLogitOut[,"age"])
ageOdds <- apply(ageLogOdds,1,exp)
#png("Figure10_7.png", width = 6, height = 6, units = 'in', res = 300) 
hist(ageOdds, main=NULL)
abline(v=quantile(ageOdds,c(0.025)),col="black")	
abline(v=quantile(ageOdds,c(0.975)),col="black")
#dev.off()




#####################################
# Chapter 11

#png("Figure11_1.png", width = 6, height = 6, units = 'in', res = 300) 
boxplot(weight ~ Time, data=ChickWeight)
#dev.off()

# t-test
ch16index <- ChickWeight$Time == 16 # Chicks measured at time 16
ch18index <- ChickWeight$Time == 18 # Chicks measured at time 18
bothChicks <- ChickWeight[ch16index | ch18index,] # Both sets together

time16weight <- bothChicks[bothChicks$Time == 16,"weight"] # Grab the weights for t=16
time18weight <- bothChicks[bothChicks$Time == 18,"weight"] # Grab the weights for t=18
cor(time16weight,time18weight) # Are they correlated?

mean(time16weight)
mean(time18weight)
t.test(time18weight,time16weight,paired = FALSE) # Independent groups t-test
BESTmcmc(time18weight,time16weight)# Run the Bayesian equivalent 

t.test(time18weight,time16weight,paired = TRUE) # Dependent groups t-test
weightDiffs <- time18weight - time16weight # Make difference scores
t.test(weightDiffs) # Run a one sample t-test on difference scores
BESTmcmc(weightDiffs) # Run the Bayesian equivalent on difference scores


# ANOVA within
chwBal <- ChickWeight # Copy the dataset
chwBal$TimeFact <- as.factor(chwBal$Time) # Convert Time to a factor
list <- rowSums(table(chwBal$Chick,chwBal$TimeFact))==12 # Make a list of rows
list <- list[list==TRUE] # Keep only those with 12 observations
list <- as.numeric(names(list)) # Extract the row indices
chwBal <- chwBal[chwBal$Chick %in% list,] # Match against the data

# table(chwBal$Chick,chwBal$TimeFact) # Check results

summary(aov(weight ~ TimeFact + Error(Chick), data=chwBal))

# Code for the Sidebar
library("ez")
install.packages("ez")
ezANOVA(data=chwBal, dv=.(weight), within=.(TimeFact), wid=.(Chick), detailed=TRUE)


# Treating time as a random variable
#summary(aov(weight ~ Time + Error(Chick), data=chwBal))
#summary(lme(weight ~ TimeFact, data=chwBal, random = ~1 | Chick))


set.seed(1234)				# Control random numbers
tslen <- 180					# About half a year of daily points
ex1 <- rnorm(n=tslen,mean=0,sd=10)        # Make a random variable
tex1 <- ex1 + seq(from=1, to=tslen, by=1) # Add the fake upward trend

#png("Figure11_2.png", width = 6, height = 6, units = 'in', res = 300) 
plot.ts(tex1) # Plot the time series with a connected line 
#dev.off()

ex2 <- rnorm(n=tslen,mean=0,sd=10)        # Make another random variable
tex2 <- ex2 + seq(from=1, to=tslen, by=1) # Add the fake upward trend
cor(ex1, ex2)                           # Correlation between the two random variables 
cor(tex1, tex2)                         # Correlation between the two time series 

#png("Figure11_3.png", width = 6, height = 6, units = 'in', res = 300) 
plot(tex1, tex2)
#dev.off()

ex3 <- rnorm(n=tslen,mean=0,sd=10)  
tex3 <- ex3 + seq(from=1, to=tslen, by=1) # Add the fake upward trend
tex3 <- tex3 + sin(seq(from=0,to=36,length.out=tslen))*20
#png("Figure11_4.png", width = 6, height = 6, units = 'in', res = 300) 
plot.ts(tex3)
#dev.off()

decOut <- decompose(ts(tex3,frequency=30))
#png("Figure11_5.png", width = 6, height = 6, units = 'in', res = 300) 
plot(decOut)
#dev.off()

mean(decOut$trend,na.rm=T)
mean(decOut$seasonal)
mean(decOut$random,na.rm=T)
cor(ex3, decOut$random, use="complete.obs")

set.seed(1234)
tslen <- 180
ex1 <- rnorm(n=tslen,mean=0,sd=10)        # Make a random variable
#png("Figure11_6.png", width = 6, height = 6, units = 'in', res = 300) 
acf(ex1, main=NULL)
#dev.off()

tex1 <- ex1 + seq(from=1, to=tslen, by=1) # Add the fake upward trend
#png("Figure11_7.png", width = 6, height = 6, units = 'in', res = 300) 
acf(tex1, main=NULL)
#dev.off()

ex3 <- rnorm(n=tslen,mean=0,sd=10)  
tex3 <- ex3 + seq(from=1, to=tslen, by=1) # Add the fake upward trend
tex3 <- tex3 + sin(seq(from=0,to=36,length.out=tslen))*20
acf(tex3)
acf(decOut$trend,na.action=na.pass)

#png("Figure11_8.png", width = 6, height = 6, units = 'in', res = 300) 
acf(decOut$seasonal, main=NULL)
#dev.off()

#png("Figure11_9.png", width = 6, height = 6, units = 'in', res = 300) 
acf(decOut$random,na.action=na.pass, main=NULL)
#dev.off()

install.packages("tseries")
library("tseries")
decComplete <- decOut$random[complete.cases(decOut$random)]
adf.test(decComplete) # Shows significant, so it is stationary

#png("Figure11_10.png", width = 6, height = 6, units = 'in', res = 300) 
plot(EuStockMarkets, main=NULL)
#dev.off()

#png("Figure11_11.png", width = 6, height = 6, units = 'in', res = 300) 
plot(diff(EuStockMarkets), main=NULL)
#dev.off()

# The following code examines change point analysis

install.packages("changepoint")
library(changepoint)

# Use changepoint analysis to locate the positon of a mean change
DAX <- EuStockMarkets[,1]
DAXcp <- cpt.mean(DAX)
DAXcp

#png("Figure11_12.png", width = 6, height = 6, units = 'in', res = 300) 
plot(DAXcp,cpt.col="grey",cpt.width=5)
#dev.off()

cpt.var(diff(EuStockMarkets[,"DAX"])) # Examine the change in variance


# Change to a simple output data structure to retrieve the confidence value
DAXcp <- cpt.mean(DAX,class=FALSE) 
DAXcp["conf.value"]


# Now difference the DAX series and look for a change in variance
dEUstocks <- diff(EuStockMarkets)
plot(dEUstocks)
dDAX <- dEUstocks[,1]
dDAXcp <- cpt.var(dDAX)
plot(dDAXcp,cpt.col="grey",cpt.width=5)
dDAXcp

install.packages("bcp")
library(bcp)

bcpDAX <- bcp(as.vector(DAX))
#png("Figure11_13.png", width = 8, height = 6, units = 'in', res = 300) 
plot(bcpDAX,outer.margins = list(left = unit(4,"lines"), bottom = unit(3, "lines"), right = unit(3, "lines"), top = unit(2,"lines")), main=NULL) 
#dev.off() 

#png("Figure11_14.png", width = 6, height = 6, units = 'in', res = 300) 
plot(bcpDAX$posterior.prob>.95)
#dev.off()

# Sidebar 11.2
# Run a model with p=1, d=0, and q=1; hold out the last ten values

tsFit <- arima(LakeHuron[1:88], order=c(1,0,1))	# Fit the model
predict(tsFit,n.ahead=10)				# Show the next ten predicted values
LakeHuron[89:98] 					# Compare with the actual values

# End of Sidebar 11.2


# Homework starter code
grp1 <- rnorm(100)
grp2 <- grp1 + runif(100,max=0.1)
t.test(grp1,grp2,paired=FALSE)
t.test(grp1,grp2,paired=TRUE)




#####################################
# Chapter 12

str(iris)  # Reveal the data structure for the iris dataset

irisN <- subset(iris,select=-Species)
str(irisN)

round(cor(irisN),digits=3)  # Show correlation matrix for the iris data

install.packages("psych")
library(psych)

irisNout <- principal(irisN)
irisNout

irisNout <- principal(irisN,nfactors=2)
irisNout

summary(irisN)

irisNS <- scale(irisN) # standardize each variable
flowerSize <- (irisNS[,1]+ irisNS[,3]+ irisNS[,4])/3 # All except Sepal.Width
mean(flowerSize)
sd(flowerSize)

# Sidebar 12.1 code
facScore1 <- irisNout$scores[,"RC1"]
facScore2 <- irisNout$scores[,"RC2"]
length(facScore1)
mean(facScore1)
sd(facScore1)
cor(facScore1,flowerSize)
cor(facScore2,flowerSize)



alpha(irisN,check.keys = TRUE)

irisNout <- principal(irisN,nfactors=2) # We just ran this earlier in the chapter
#png("Figure12_1.png", width = 5, height = 9, units = 'in', res = 300) 
plot(irisNout)
#dev.off()

irisNout$rotation

irisNout <- principal(irisN,nfactors=2, rotate="none")
#png("Figure12_2.png", width = 5, height = 9, units = 'in', res = 300) 
plot(irisNout)
#dev.off()



#####################################
# Appendix B

caseLabel <- c("A","B","C","D","E")
caseLabel
age <- c(43, 42, 12, 8, 5)

gender <- c("Male","Female","Female","Male","Female")
weight <- c(188,136,83,61,44)

myFamily <- data.frame(caseLabel, age, gender, weight)
str(myFamily)
summary(myFamily)
myFamily$age
age <- c(age, 11)
myFamily$age<-c(myFamily$age, 11)


#####################################
# Appendix C
install.packages("datasets")
install.packages("dplyr")
library(datasets)
library(dplyr)
data()

euStocks <- tbl_df(data.frame(EuStockMarkets))
euStocks

arrange(euStocks, DAX)
euStocks

euStocks <- arrange(euStocks, DAX)
euStocks

euStocks <- arrange(euStocks, desc(DAX), desc(SMI))
euStocks

euStocks <- select(euStocks, DAX, FTSE)
euStocks

euStocks <- mutate(euStocks, avindex = (DAX + FTSE)/2)
euStocks

filter(euStocks, avindex < mean(avindex))

