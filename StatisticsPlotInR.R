#1. Bar plots 
barplot(height)#where height is a vector or matrix

install.packages("vcd")
library(vcd)

#a. if height is a vector, the values determine the heights of the bars in the plot and a vertical bar plot is produced
#including the option horiz=T produces a horizontal bar chart instead
#main option adds a plot title, whereas the xlab, ylab options add x-axis and y-axis labels, respectively

counts <-table(Arthritis$Improved)
counts #the number of 14 showed some improvement and 28 got marked improvement
barplot(counts, main="Simple Bar Plot", 
        xlab="Improvement", ylab="Frequency") #Vertical bar plot
barplot(counts, main="Horizontal Bar Plot",
        xlab="Frequency", ylab="Improvement", horiz=T) #Horizontal bar plot

#if the categorical variable to be plotted is a factor or ordered factor, we can create a vertical bar plot quickly with the plot() function

#Arthritis$Improved is a factor
plot(Arthritis$Improved, main="Simple Bar Plot",
     xlab="Improved", ylab="Frequency")


#b. if height is a matrix, the resulting graph will be a stacked or grouped bar plot
#beside=F(the default) means each column of the matrix produces a bar in the plot
#if beside=T, each column of the matrix represents a group, and the values in each column are juxtaposed rather than stacked
counts <-table(Arthritis$Improved, Arthritis$Treatment)
counts
barplot(counts, main="Stacked Bar Plot",
                     xlab="Treatment", ylab="Frequency",
                     col=c("red","yellow","green"),
                     legend=rownames(counts))
barplot(counts, main="Grouped Bar Plot",
xlab="Treatment",ylab="Frequency",
col=c("red","yellow", "green"),
legend=rownames(counts), beside=T)

#legend.text provides bar labels for the legend which are only useful when height is a matrix

#c. mean bar plots
states <-data.frame(state.region, state.x77)
means <- aggregate(states$Illiteracy, by=list(state.region), FUN=mean)
means
means<-means[order(means$x),]
barplot(means$x, names.arg=means$Group.1)
title("Mean Illiteracy Rate")
#we can also create mean bar plots with super-imposed confidence intervals using the barplot2() function in the gplots package

#d. Tweaking bar plots: for dealing with the problem of overlaping
#using the cex.names option for decreasing the font size
#names.arg allows to specify a charcter vector of names used to label the bars

par(mar=c(5,8,4,2))
par(las=2)
counts <- table(Arthritis$Improved)
barplot(counts, main="Treatment Outcome",
        horiz=T,
        cex.names=.8,
        names.arg=c("No Improvement", "Some Improvement",
                    "Marked Improvement"))

#2. spinograms: the stacked bar plot is rescaled so that the height of each bar is 1 and the segment heights represent proportions
spine() #in vcd package
attach(Arthritis)
counts <- table(Treatment, Improved)
spine(counts, main="Spinogram Example")
detach(Arthritis)


#3. Pie charts 
pie(x, labels) #x is a non-negative numeric vector indicating the area of each slice and blabels provide a chracter vector of slice labels

par(mfrow=c(2,2)) #combines four graphs into one 
slices <-c(10,12.4,16,8)
lbls <-c("US","UK","Australia","Germany","France")
pie(slices, labels=lbls, main="Simple Pie Chart") #graph 1

pct <-round(slices/sum(slices)*100)#add percentages to the pie chart
lbls2 <-paste(lbls, "", pct, "%", sep="")
pie(slices, labels=lbls2, col=rainbow(length(lbls2)),# providing color of number of length(lbls2)
    main="Pie Chart with Percentages") #graph 2

install.packages("plotrix")
library(plotrix)

pie3D(slices, labels=lbls, explode=.1, main="3D Pie Chart")# creat a 3d chart, from plotrix package
mytable <- table(state.region)
lbls3 <-paste(names(mytable), "\n", mytable, sep="")

pie(mytable, labels= lbls3,
    main="Pie Chart from a Table\n(with sample sizes)")#create a chart from table

#4. fan plot, which is better than pie charts to compare 
fan.plot() 

slices <-c(10, 12.4, 16, 8)
lbls <- c("US","UK","Australia", "Germany","France")
fan.plot(slices, labels = lbls, main="Fan Plot")


#5. Histograms

hist(x) #x is a numeric vector of values
#the option freq=F creates a plot based on probability densities rather than frequencies.
#breaks option controls the number of bins, the default produces equally spaced breaks when defining the cells of the histogram

par(mfrow=c(2,2))

hist(mtcars$mpg) #simple histogram, the default plot when no options are specified, five bins are created 

hist(mtcars$mpg, # got informative labels and title
     breaks=12,
     col="red",
     xlab="Miles Per Gallon",
     main="Colored histogram with 12 bins") #with specified bins and color

hist(mtcars$mpg,
     freq=F,
     breaks=12,
     col="red",
     xlab="Miles Per Gallon",
     main="Histogram, rug plot, density curve")
rug(jitter(mtcars$mpg)) #rug plot is a one dimensional representation of the actual data values, we can jitter the data on rug plot 
lines(density(mtcars$mpg), col="blue", lwd=2)

x<-mtcars$mpg

h<-hist(x, breaks=12, col="red", #with a normal curve and frame
        xlab="Miles Per Gallon",
        main="Histogram with normal curve and box")
xfit <-seq(min(x), max(x), length=40)
yfit<-dnorm(xfit, mean=mean(x), sd=sd(x))
yfit <-yfit&diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
box() #get a box figure around
#superimposing the normal curve comes from a suggestion posted to the R help mailing list by Peter Dalgaard

#6. Kernel density plot
#estimate the probability density of a random nonparamteric variable
#it is effective to view the distribution of a continuous variable
plot(density(x)) #x is a numeric vector

par(mfrow=c(2,1))
d <- density(mtcars$mpg)
plot(d)

d<- density(mtcars$mpg)
plot(d, main="Kernel Density of Miles Per Gallon")
polygon(d, col="red", border="blue")
rug(mtcars$mpg, col="brown")
#kernel density plots can be used to compare groups, but it is underutilized, probably due to a general lack of easily accessible software
#sm package fills this gap nicely

sm.density.compare() #allows to superimpose the kernel density plots of two or more groups

sm.density.compare(x, factor) #x is a numeric vector and factor is a grouping variable

install.packages("sm")
library(sm)
attach(mtcars)

cyl.f <-factor(cyl, levels=c(4,6,8),
               labels = c("4 cylinder", "6 cylinder",
                          "8 cylinder"))
sm.density.compare(mpg, cyl, xlab="Miles Per Gallon")
title(main="MPG Distribution by Car Cylinders")

colfill <-c(2:(1+length(levels(cyl.f))))#color
legend(locator(1), levels(cyl.f), fill=colfill)#locator(1) placing the legend by cliking in the graph by mouse

detach(mtcars)

#7. box-and-whiskers plots
boxplot(mtcars$mpg, main="Box plot", ylab="Miles per Gallon")


#8. using parallel box plots to compare groups
boxplot(formula, data=dataframe) #formula  example: y~A
#varwidth=T makes the box plot widths proportional to the squar root of their sample sizes
#horizontal=T to reverse the axis orientation

boxplot(mpg~cyl, data=mtcars,
        main="Car Mileage Data",
        xlab="Number of Cylinders",
        ylab="Miles Per Gallon", horizontal=T, varwidth=T)

#by adding notch=T we can get notched box plots
#if two boxes' notches don't overlap, there is strong evidence that their medians differ 

boxplot(mpg~cyl, data=mtcars,
        notch=T,
        varwidth=T,
        col="red",
        main="Car Mileage Data",
        xlab="Number of Cylinders",
        ylab="Miles Per Gallon")
#here notches differ that means the median car mileage for 4-, 6-, 8-cylinder cars differs

#9. box plots for two crossed factors
mtcars$cyl.f <- factor(mtcars$cyl, 
                       levels=c(4,6,8),
                       labels=c("4","6","8")) #create a factor for the number of cylinders
mtcars$am.f <-factor(mtcars$am, 
                     levels=c(0,1),
                     labels=c("auto","standard")) #creates a factor for transmission type
boxplot(mpg~am.f * cyl.f, 
        data=mtcars, 
        varwidth=T, col=c("gold","darkgreen"),#generate the box plot
        main="MPG Distribution by Auto Type",
        xlab="Auto Type", ylab="Miles Per Gallon")

#10. Violin plots
#a combination of a box plot and a kernel density plot
vioplot(x1, x2, ..., names=, col=)
#where x1, x2, ... represent one or more numeric vectors to be plotted(one violin plot is produced for each vector)
#names parameter provides a character vector of labels for the violin plots
#col is a vector specifying the colors for each violin plot

install.packages("vioplot")
library("vioplot")
x1 <- mtcars$mpg[mtcars$cyl==4]
x2 <- mtcars$mpg[mtcars$cyl==6]
x3 <- mtcars$mpg[mtcars$cyl==8]
vioplot(x1, x2, x3,
        names=c("4 cyl", "6 cyl", "8 cyl"),
        col="gold")
title("Violin Plots of Miles Per Gallon", ylab="Miles Per Gallon",
      xlab="Number of Cylinders")
#white dot is the median, the black boxes range from the lower to the upper quartile

#11. dot plots: plot every value for a variable
dotchart(x, labels=)
#x is a numeric vector and labels specifies a vector that labels each point
#can add a groups option to designate a factor specifying how the elements of x are grouped
#gcolor controls the groups' colors
#cex controls the size of the labels

dotchart(mtcars$mpg, labels=row.names(mtcars), cex=.7, 
         main="Gas Mileage for Car Models",
         xlab="Miles Per Gallon")

#dot plot grouped, roted and colored
x <-mtcars[order(mtcars$mpg),]
x$cyl <-factor(x$cyl)
x$color[x$cyl==4] <-"red"
x$color[x$cyl==6] <-"blue"
x$color[x$cyl==8] <-"darkgreen"
dotchart(x$mpg, labels = row.names(x),
         cex=.7,
         groups = x$cyl,
         gcolor = "black",
         color = x$color,
         pch=19,
         main = "Gas Mileage for Car Models\ngrouped by cylinder",
         xlab = "Miles Per Gallon")
#Jacoby(2006) provides very informative discussion of the dot plot 
#Hmisc package offer a dot plot function named dotchart2() with a number of additional features

