rm(list = ls())	# clean all objects from the workspace
			# command ‘ls()’ shows you names of all used variables

########## LOAD DATA

sunspot.year = read.csv("sunspot_year.csv")   # read data from CSV file

# Since it is an example, the data set is taken from the basic package ‘datasets’,  
# so you can work with the set just using it name (without reading from the CSV)

sunspot.year

########## PLOTS

# scatter plot
plot(sunspot.year)

# histogram
hist(sunspot.year)

# add normal curve
h = hist(sunspot.year)
xfit = seq(min(sunspot.year),max(sunspot.year),length=100) 
yfit = dnorm(xfit,mean=mean(sunspot.year),sd=sd(sunspot.year)) 
yfit = yfit*diff(h$mids[1:2])*length(yfit )
lines(xfit, yfit, col="red", lwd=2)


# cumulative distribution function plot
plot(ecdf(sunspot.year))
plot(ecdf(sunspot.year ), verticals = TRUE, do.points = FALSE)
yfit = rnorm(length(sunspot.year),mean=mean(sunspot.year),sd=sd(sunspot.year))
plot(ecdf(yfit), verticals = TRUE, do.points = FALSE, add=TRUE, col="red")


########## DESCRIPTIVE STATISTICS

summary(sunspot.year)

# to calculate skewness and kurtosis, you should install and load package ‘fBasics’ 
# (if you want you can use any other package, to get the right result the main thing )

install.packages("fBasics")	# install
library("fBasics")		# load			

skewness(sunspot.year)
kurtosis(sunspot.year)

# Variance for skewness and kurtosis
N = length(sunspot.year)

v_skew = 6*N*(N-1) / ((N-2)*(N+1)*(N+3))
v_kur = 4*(N^2-1)*v_skew / ((N-3)*(N+5))

# Confidence interval for mean
t.test(sunspot.year, conf.level=0.95)$conf.int

########## QUANTILES

# get quantile value 
quantile(sunspot.year, 0.95) 

# get quantile number knowing quantile value
ecdf(sunspot.year)( 1.5 )
