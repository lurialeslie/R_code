#load the data (method 1)

library(datasets)
data(iris)

iris_trial<- iris
#view the data

View(iris)

#head()/ tail()

head(iris, 4)
tail(iris, 5)

#display summary statistics

summary(iris)
summary(iris$Sepal.Length)

#check to see if there are missing data

sum(is.na(iris))


#install skimr package that expands on summary by providing larger set of statistics

install.packages("skimr")
library(skimr)
skim(iris_trial) # perform skim to display summary stats of iris dataset

# Group data by Species then perform skim
iris_trial %>% 
  dplyr::group_by(Species) %>% 
  skim() 

#############################
# Quick data visualization
#
# R base plot()
#############################

# Panel plots
plot(iris_trial)
plot(iris_trial, col = "magenta")

# Scatter plot
plot(iris_trial$Sepal.Width, iris_trial$Sepal.Length)

plot(iris_trial$Sepal.Width, iris_trial$Sepal.Length, col = "red")     # Makes red circles

plot(iris_trial$Sepal.Width, iris_trial$Sepal.Length, col = "red",     # Makes red circles + Adds x and y axis labels
     xlab = "Sepal width", ylab = "Sepal length")

# Histogram
hist(iris_trial$Sepal.Width)
hist(iris_trial$Sepal.Width, col = "cyan")   # Makes cyan bars

# Feature plots
# https://www.machinelearningplus.com/machine-learning/caret-package/

install.packages("caret")
install.packages("ISLR")
install.packages("lattice")
library(ISLR)
library(ggplot2)
library(caret)
library(lattice)


box_plot<- featurePlot(x = iris_trial[,1:4], 
            y = iris$Species, 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.8)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))
box_plot


#Scatterplot Matrix
featurePlot(x = iris_trial[, 1:4], 
            y = iris$Species, 
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))

#Overlayed Density Plots


featurePlot(x = iris_trial[, 1:4], 
            y = iris$Species,
            plot = "density", 
            ## Pass in options to xyplot() to 
            ## make it prettier
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(4, 1), 
            auto.key = list(columns = 3))
