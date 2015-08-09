#Quiz #1

#Question 1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)

#minimization of  ∑wi(xi−μ)^2 is:
ans <- sum(x*w)/sum(w)

#Question 2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

lm(y~x-1) # here "-1" is just a parameters of lm() which defined that model should be built throught the origin

#Question 3
library(dplyr)
library(ggplot2)

data("mtcars")
lm(I(mtcars$mpg-mean(mtcars$mpg))~I(mtcars$wt-mean(mtcars$wt))-1)


# plotting the model:
freqData <- as.data.frame(table(mtcars$mpg, mtcars$wt))
names(freqData) <- c("mpg", "wt", "freq")
freqData$mpg <- as.numeric(as.character(freqData$mpg))
freqData$wt <- as.numeric(as.character(freqData$wt))


g <- ggplot(filter(freqData, freq > 0), aes(x = mtcars$wt, y = mtcars$mpg))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")       
g <- g + geom_abline(intercept = 0, slope = -5.344  , size = 3)
g

#Question 4
cor(Y,X) = .5
sd(X) = .5* sd(Y)
#a formula which should be applied: https://www.dropbox.com/s/w7fhbjurhyzmhn7/Screenshot%202015-08-09%2008.59.24.png?dl=0
ans <- 1

# Question 5
#mu(X),mu(Y) <- 0 # scores for both tests were normalized to have mean 0
#var(X),var(Y) <- 1 # scores where normalized to have variance 1
#cor(X,Y) <- 0.4
#On process of normalization
#https://www.dropbox.com/s/t70eg7ga1gm2au9/Screenshot%202015-08-09%2009.09.04.png?dl=0

#Question 6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
xn <- (x- mean(x))/sd(x)

#Question 7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

lm(y~x)

#Question 9
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)
