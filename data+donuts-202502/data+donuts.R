# Data + Donuts: Making Beautiful Plots in R's Ggplot2
# Author: Jessica M. Alexander (https://jessb0t.github.io/)
# Workshop Date: 2025-02-13
# Summary: This script uses the standard 'iris' and 'mtcars' datasets to introduce fundamental
#           and semi-advanced features of plotting with ggplot.  The following R functions are covered:
#           - library()
#           - paste()
#           - c()
#           - data.frame()
#           - dplyr::filter()
#           - lplyr::pull()
#           - stats::cor.test()
#           - ggplot2::
#             - ggplot()
#             - aes()
#             - geom_point()
#             - geom_smooth()
#             - geom_label()
#             - facet_wrap()
#             - labs()
#             - theme_bw()
#             - ggsave()


#one-time install to your computer
#install.packages("tidyverse") #this installs multiple packages, including dplyr, lplyr, and ggplot2
#install.packages("datasets") #this installs the package that houses our sample data
#install.packages("stats") #this installs a very common package for standard statistical methods

#the following will need to be done whenever you create a new instance of R
#load necessary packages
library(datasets)
library(tidyverse)
library(stats)

#specify folder where plots should go (discuss relative merits of absolute v. relative paths)
outpath <- "your-filepath-here"

#helper functions and variables
#I like to include the date for any outputs, so this is a handy variable to have
today <- format(Sys.Date(), "%Y%m%d")

#this function takes numbers less than one and decides if they get 3 or four digits for display
digit_display <- function(number){
  if(abs(number)<0.001){
    x <- sprintf("%.4f", number)
  }else{
    x <- sprintf("%.3f", number)
  }
  return (x) #this is a string
}

#this function takes a p-value and gives you a string that either rounds to 3 decimal places or outputs the classic "p<0.001"
tinyps <- function(pval){
  if(pval < 0.001){
    x = "< 0.001"
  }else {
    tmp <- round(pval,3)
    x = paste("= ", as.character(tmp), sep="")
  }
  return (x) #this is a string
}


### EXAMPLE PLOT: IRIS DATASET
#about the dataset (https://en.wikipedia.org/wiki/Iris_flower_data_set)
#pretty picture of irises (https://www.kaggle.com/code/sufyansadiq/exploratory-data-analysis-on-iris-flower-dataset)
#measurements from 150 iris flowers of 5 variables:
# - Sepal.Length (numerical value, in centimeters)
# - Sepal.Width (numerical value, in centimeters)
# - Petal.Length (numerical value, in centimeters)
# - Petal.Width (numerical value, in centimeters)
# - Species (factor of three levels: 'setosa', 'versicolor', 'virginica')

#load the iris data
data(iris) #this will create a dataframe named 'iris' in your environment

#helpful note for your own data: when you want to load your data, you could call the dataframe 'df' and load it by replacing the filepath and filename below
#df <- read.csv("filepath-to-your-data/filename-of-your-data.csv")

#create dataframes that are subsets by species (there are other ways to do this, but this way is easy to read)
df_setosa <- filter(iris, Species=="setosa")
df_versicolor <- filter(iris, Species=="versicolor")
df_virginia <- filter(iris, Species=="virginica")

#using the species-specific dataframes, run correlation tests and assign to variables
corr_setosa <- cor.test(pull(df_setosa, Petal.Length), pull(df_setosa, Sepal.Length), method="pearson")
corr_versicolor <- cor.test(pull(df_versicolor, Petal.Length), pull(df_versicolor, Sepal.Length), method="pearson")
corr_virginia <- cor.test(pull(df_virginia, Petal.Length), pull(df_virginia, Sepal.Length), method="pearson")

#we need a special dataframe to display correlation coefficients and p-values in our plot
label_text_iris <- data.frame(
  label = c(paste("r = ", digit_display(corr_setosa$estimate),
                  "\np ", tinyps(corr_setosa$p.value), sep=""),
            paste("r = ", digit_display(corr_versicolor$estimate),
                  "\np ", tinyps(corr_versicolor$p.value), sep=""),
            paste("r = ", digit_display(corr_virginia$estimate),
                  "\np ", tinyps(corr_virginia$p.value), sep="")),
  Species = c("setosa", "versicolor", "virginica"),
  Petal.Length = c(5.5, 5.5, 5.5),
  Sepal.Length = c(4.5, 4.5, 4.5))

#we build our plot (and assign it to a variable so we can save it as a png file)
plot1 <-  ggplot(data=iris, aes(x=Petal.Length, y=Sepal.Length)) +
  geom_smooth(method="lm", color="darkgrey", se=FALSE) +
  geom_point(alpha=0.5) +
  facet_wrap(~Species, labeller=as_labeller(c(`setosa`="Setosa", `versicolor`="Versicolor", `virginica`="Virginica"))) +
  geom_label(data=label_text_iris, aes(x=Petal.Length, y=Sepal.Length, label=label), size=3, color="black") +
  labs(title="Correlation of Petal and Sepal Length",
       subtitle="in the 'iris' dataset",
       x="Petal Length (cm)",
       y="Sepal Length (cm)") +
  theme_bw()

#output the plot to file
fileout <- paste(outpath, "irisplot_", today, ".png", sep="")
ggsave(fileout, plot=plot1, width=6, height=5, units="in")


### YOUR TURN: MTCARS DATASET
#about the dataset (https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html)
#auto design details for 32 car models from Motor Trends magazine in 1974; we focus on these variables:
# - disp (numerical value, in cubic inches) = displacement
# - wt (numerical value, in lbs/1000) = weight
# - am (factor of two levels: 0=automatic, 1=manual) = type of transmission

#load the dataset
data(mtcars)

#now create the plot on your own; feel free to customize it in ways you might desire, for example:
# - try changing the theme (+ theme_xxxx())
# - try making the points a different color (color=)
# - try changing the opacity of the points (alpha=)
