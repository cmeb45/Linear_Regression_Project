## LOAD LIBRARY PACKAGES
library(car)
library(ggplot2)
library(leaps)
library(lme4)
library(MASS)
library(plyr)
library(qcc)
library(sqldf)
library(xtable)

## CUSTOM FUNCTIONS
# Multiple plot function 
# Credit goes to Cookbook for R (http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/)
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



## INPUT DATA
# Read the data as a data frame
data <- read.table(file = "salary.txt", sep = ",", header = TRUE)

## SUMMARY STATISTICS
wage.sum <- data.frame(wage = data$wage)
race.sum <- data.frame(race = data$race)
print(xtable(summary(wage.sum)))
print(xtable(summary(race.sum)))


# Average earnings per week
paste("$",round(mean(data$wage),2),sep="")

# Average earnings per year
paste("$",52*round(mean(data$wage),2),sep="")

# Proportion of African Americans
paste(round(100*sum(data$race=="black")/nrow(data),1),"%",sep="")

# Proportion of African American wages
paste(round(100*sum(data[which(data$race=="black"),"wage"])/sum(data$wage),1),"%",sep="")

# Aggregate wages by race for Pareto chart
data.agg <- sqldf("SELECT race, SUM(wage) AS total_wages
                  FROM data
                  GROUP BY race")
data.counts <- data.agg$total_wages
names(data.counts) <- data.agg$race

p <- pareto.chart(data.counts, ylab="Total Weekly Wages",
                  xlab="Race",
                  main="Pareto Chart for Total Weekly Wages per Racial Group")

## SUBSET THE DATA

# Black or white males
black_white <- subset(data,data$race %in% c("black","white"))

# Overlaid histograms of wages for black males & white males
p1 <- ggplot(black_white, aes(x = log(wage), fill = race)) +
  geom_histogram(binwidth = 0.5, alpha = 0.5, position = "identity")+
  ggtitle("Overlaid histograms of wages for black and white males") +
  xlab("Log of Wages") + ylab("# of Employees")

p2 <- ggplot(black_white, aes(x = race, y = log(wage), fill = race)) + 
  geom_boxplot() + ggtitle("Boxplots of wages for black and white males") +
  ylab("Log of Wages") + xlab("Race")

multiplot(p1, p2, cols = 1)


# Black & all other males
black_other <- data
black_other$race <- revalue(black_other$race, 
                            c("white"="all other", "other"="all other"))

# Overlaid histograms of wages for black males & all other males
p1 <- ggplot(black_other, aes(x = log(wage), fill = race)) +
  geom_histogram(binwidth = 0.5, alpha = 0.5, position = "identity")+
  ggtitle("Overlaid histograms of wages for black and all other males") +
  xlab("Log of Wages") + ylab("# of Employees")

p2 <- ggplot(black_other, aes(x = race, y = log(wage), fill = race)) + 
  geom_boxplot() + ggtitle("Boxplots of wages for black and all other males") +
  ylab("Log of Wages") + xlab("Race")

multiplot(p1, p2, cols = 1)



# Split data into training (80%) and testing (20%) subsets
n <- nrow(data)
n.test <- round(0.2*n)
n.train <- n - n.test
set.seed(0)
index <- sample(1:n,n.test,replace = F)
train.data <- data[-index,]
test.data <- data[index,]

# Quality control check
black.train.per <- sum(train.data$race=="black")/nrow(train.data)
black.test.per <- sum(test.data$race=="black")/nrow(test.data)
if(abs(black.train.per - black.test.per) <= 0.01){
  print("Both subsets have similar proportions of black males")
} else{
  print("Error: Training & testing subsets have different data proportions")
}




## RESEARCH QUESTION 1
# Create indicator variable
black_white$black <- I(black_white$race=="black")

# ANOVA regression analysis
fit <- aov(wage ~ black, data=black_white)
print(xtable(summary(fit)))

# p-value
paste("(",round(summary(fit)[[1]][[5]][1],62),")",sep="")

# F-score
paste("(",round(summary(fit)[[1]][[4]][1],2),")",sep="")


## RESEARCH QUESTION 2
# Create indicator variable
black_other$black <- I(black_other$race=="black")

# ANOVA regression analysis
fit <- aov(wage ~ black, data=black_other)
print(xtable(summary(fit)))

# p-value
paste("(",round(summary(fit)[[1]][[5]][1],65),")",sep="")

# F-score
paste("(",round(summary(fit)[[1]][[4]][1],2),")",sep="")


## MODEL BUILDING: BASIC SCATTER PLOTS & BOX PLOTS
# Race vs. wage
p1 <- ggplot(train.data, aes(x = race, y = log(wage), fill = race)) +
  geom_boxplot()+ylab("Log of Wages") + xlab("Race")


# Years of education vs. Wage
p2 <- ggplot(train.data, aes(x = edu, y = log(wage), color = race)) + 
  geom_point(shape=1)+ylab("Log of Wages") + xlab("Years of Education")
# Less years of education have lower wages & less variance
# More years of education have higher wages & more variance

cor(log(train.data$wage),train.data$edu)
# 0.3697235



# Years of job experience vs. wage
p3 <- ggplot(train.data, aes(x = exp, y = log(wage), color = race)) + 
  geom_point(shape=1)+ylab("Log of Wages") + xlab("Years of Job Experience")
# Wages increase then plateau with more job experience

cor(log(train.data$wage),train.data$exp)
# 0.2327511


# City vs. wage
p4 <- ggplot(train.data, aes(x = city, y = log(wage), fill = race)) + 
  geom_boxplot()+ylab("Log of Wages") + xlab("Live in/near City?")
# Those who live in/near city have higher wages


# Region vs. wage
p5 <- ggplot(train.data, aes(x = reg, y = log(wage), fill = race)) + 
  geom_boxplot()+ylab("Log of Wages") + xlab("US Region")
# South has lower wages; other regions have similar spread


# College graduate vs. wage
p6 <- ggplot(train.data, aes(x = deg, y = log(wage), fill = race)) + 
  geom_boxplot()+ylab("Log of Wages") + xlab("Graduated College?")
# College graduates have significantly higher wages


# Commuting distance vs. wage
p7 <- ggplot(train.data, aes(x = com, y = log(wage), color = race)) + 
  geom_point(shape=1)+ylab("Log of Wages") + xlab("Commuting Distance")
# Wages independent of commuting distance

cor(log(train.data$wage),train.data$com)
# 0.001886612

# Number of employees vs. wage
p8 <- ggplot(train.data, aes(x = emp, y = log(wage), color = race)) + 
  geom_point(shape=1)+ylab("Log of Wages") + xlab("# of Employees")
# Wages independent of number of employees

cor(log(train.data$wage),train.data$emp)
# 0.05989805

multiplot(p1, p2, p3, p4, cols = 2)

multiplot(p5, p6, p7, p8, cols = 2)



## MODEL BUILDING: INTERACTION PLOTS

# Education vs. Race (Yes)
dataInt <- ddply(train.data,.(edu,race),summarise, val = mean(log(wage)))
p1 <- ggplot(train.data, aes(x = factor(edu), y = log(wage), colour = race)) + 
  geom_boxplot() + 
  geom_point(data = dataInt, aes(y = val)) +
  geom_line(data = dataInt, aes(y = val, group = race)) + 
  theme_bw() + xlab("Education (years)")+ylab("Log of Wages")


# Education vs. City (Yes)
dataInt <- ddply(train.data,.(edu,city),summarise, val = mean(log(wage)))
p2 <- ggplot(train.data, aes(x = factor(edu), y = log(wage), colour = city)) + 
  geom_boxplot() + 
  geom_point(data = dataInt, aes(y = val)) +
  geom_line(data = dataInt, aes(y = val, group = city)) + 
  theme_bw() + xlab("Education (years)")+ylab("Log of Wages")+ labs(colour = "Live in/near City?")


# Education vs. Region (Yes)
dataInt <- ddply(train.data,.(edu,reg),summarise, val = mean(log(wage)))
p3 <- ggplot(train.data, aes(x = factor(edu), y = log(wage), colour = reg)) + 
  geom_boxplot() + 
  geom_point(data = dataInt, aes(y = val)) +
  geom_line(data = dataInt, aes(y = val, group = reg)) + 
  theme_bw() + xlab("Education (years)")+ylab("Log of Wages")+ labs(colour = "US Region")


# Education vs. Degree (Yes)
dataInt <- ddply(train.data,.(edu,deg),summarise, val = mean(log(wage)))
p4 <- ggplot(train.data, aes(x = factor(edu), y = log(wage), colour = deg)) + 
  geom_boxplot() + 
  geom_point(data = dataInt, aes(y = val)) +
  geom_line(data = dataInt, aes(y = val, group = deg)) + 
  theme_bw() + xlab("Education (years)")+ylab("Log of Wages")+ labs(colour = "Graduated College?")


# Race vs. Region (Yes)
dataInt <- ddply(train.data,.(race,reg),summarise, val = mean(log(wage)))
p5 <- ggplot(train.data, aes(x = factor(race), y = log(wage), colour = reg)) + 
  geom_boxplot() + 
  geom_point(data = dataInt, aes(y = val)) +
  geom_line(data = dataInt, aes(y = val, group = reg)) + 
  theme_bw() + xlab("Race")+ylab("Log of Wages")+ labs(colour = "US Region")

# Race vs. City (No)
dataInt <- ddply(train.data,.(race,city),summarise, val = mean(log(wage)))
p6 <- ggplot(train.data, aes(x = factor(race), y = log(wage), colour = city)) + 
  geom_boxplot() + 
  geom_point(data = dataInt, aes(y = val)) +
  geom_line(data = dataInt, aes(y = val, group = city)) + 
  theme_bw() + xlab("Race")+ylab("Log of Wages")+ labs(colour = "Live in/near City?")


# Race vs. Degree (No)
dataInt <- ddply(train.data,.(race,deg),summarise, val = mean(log(wage)))
p7 <- ggplot(train.data, aes(x = factor(race), y = log(wage), colour = deg)) + 
  geom_boxplot() + 
  geom_point(data = dataInt, aes(y = val)) +
  geom_line(data = dataInt, aes(y = val, group = deg)) + 
  theme_bw() + xlab("Race")+ylab("Log of Wages")+ labs(colour = "Graduated College?")


# City vs. Region (Yes)
dataInt <- ddply(train.data,.(city,reg),summarise, val = mean(log(wage)))
p8 <- ggplot(train.data, aes(x = factor(city), y = log(wage), colour = reg)) + 
  geom_boxplot() + 
  geom_point(data = dataInt, aes(y = val)) +
  geom_line(data = dataInt, aes(y = val, group = reg)) + 
  theme_bw() + xlab("Live in/near City?")+ylab("Log of Wages")+ labs(colour = "US Region")


# City vs. Degree (No)
dataInt <- ddply(train.data,.(city,deg),summarise, val = mean(log(wage)))
p9 <- ggplot(train.data, aes(x = factor(city), y = log(wage), colour = deg)) + 
  geom_boxplot() + 
  geom_point(data = dataInt, aes(y = val)) +
  geom_line(data = dataInt, aes(y = val, group = deg)) + 
  theme_bw() + xlab("Live in/near City?")+ylab("Log of Wages")+ labs(colour = "Graduated College?")

# Region vs. Degree (No)
dataInt <- ddply(train.data,.(reg,deg),summarise, val = mean(log(wage)))
p10 <- ggplot(train.data, aes(x = factor(reg), y = log(wage), colour = deg)) + 
  geom_boxplot() + 
  geom_point(data = dataInt, aes(y = val)) +
  geom_line(data = dataInt, aes(y = val, group = deg)) + 
  theme_bw() + xlab("US Region")+ylab("Log of Wages")+ labs(colour = "Graduated College?")

multiplot(p1, p2, cols = 1)
multiplot(p3, p4, cols = 1)
multiplot(p5, p6, cols = 1)
multiplot(p7, p8, cols = 1)
multiplot(p9, p10, cols = 1)


## MODEL FORMATION

# Rough model 1 (All but 2 variables; no transformations)
m1 <- lm(wage~edu+exp+city+reg+race+deg,data=train.data)

# BoxCox procedure (Transformation of dependent variable)
bac.box = boxcox(wage ~ race + edu + city + reg + deg,data = train.data)
where.max.y = which(bac.box$y == max(bac.box$y))
bac.lambda = bac.box$x[where.max.y]

# BoxCox transformation value
paste(round(bac.lambda,2))

# Rough model 2 (All but 2 variables; dependent variable transformation)
m2 <- lm(log(wage)~edu+exp+city+reg+race+deg,data=train.data)

# Rough model 3 (All but 2 variables; dependent variable transformation; interaction with region)
m3 <- lm(log(wage)~edu+exp+city+reg+race+deg+
           race*reg + city*reg,data=train.data)

# Rough model 4 (All but 2 variables; dependent variable transformation; interactions with region & education)
m4 <- lm(log(wage)~edu+exp+city+reg+race+deg+
           race*reg + city*reg + edu*race + edu*city +
           edu*reg + edu*deg,data=train.data)


## MODEL SELECTION: EVALUATION
# Evaluate on model building set
# Include R^2, R^2_a, AIC, BIC
# Select final model

AIC_values <- c(round(AIC(m1),2), round(AIC(m2),2),
                round(AIC(m3),2),round(AIC(m4),2))

BIC_values <- c(round(BIC(m1),2), round(BIC(m2),2),
                round(BIC(m3),2),round(BIC(m4),2))

R_2_values <- c(round(summary(m1)$r.squared,3), 
                round(summary(m2)$r.squared,3),
                round(summary(m3)$r.squared,3),
                round(summary(m4)$r.squared,3)
)

R_2_adj_values <- c(round(summary(m1)$adj.r.squared,3), 
                    round(summary(m2)$adj.r.squared,3),
                    round(summary(m3)$adj.r.squared,3),
                    round(summary(m4)$adj.r.squared,3)
)

model_values <- data.frame(AIC = AIC_values,
                           BIC = BIC_values,
                           R_2 = R_2_values,
                           adj_R_2 = R_2_adj_values)
row.names(model_values) <- c("Model 1", "Model 2", 
                             "Model 3","Model 4")

print(xtable(model_values))


## FINAL LINEAR REGRESSION MODEL
final_model <- lm(log(wage)~edu+exp+city+reg+race+deg+
                    race*reg + city*reg + edu*race + edu*city +
                    edu*reg + edu*deg,data=train.data)
print(summary(final_model)$call)
print(paste("F statistic value: ",summary(final_model)$fstatistic[[1]]))
print(paste("F statistic numerator: ",summary(final_model)$fstatistic[[2]]))
print(paste("F statistic denominator: ",summary(final_model)$fstatistic[[3]]))
print(xtable(summary(final_model)))

# AIC
paste(round(AIC(final_model),2))

# R^2
paste(round(summary(final_model)$r.squared,3))

# Adjusted R^2
paste(round(summary(final_model)$adj.r.squared,3))


## MODEL DIAGNOSTICS
# Studentized deleted residuals
res <- rstudent(final_model)

# QQ plot of Studentized Deleted Residuals
p1 <- qplot(sample = res, data = train.data)
# Plot straight line
p1 <- p1 + geom_abline(intercept = 0, slope = 1) +
  ggtitle("QQ Plot")

# Histogram of Studentized Deleted Residuals
p2 <- qplot(res, geom="histogram", binwidth=0.25) +
  ggtitle("Histogram") +
  xlab("Studentized Deleted Residuals")

# Line plot of Studentized Deleted Residuals
dat <- data.frame(xvar = 1:n.train, yvar = res)
p3 <- ggplot(dat, aes(x = xvar, y = yvar))+
  geom_point(shape=1)+geom_path(colour="red")+ggtitle("Line Plot")+
  xlab("")+ylab("Deleted Residuals")

# Predicted values
y.hat <- predict(final_model)

# Scatter plot of studentized deleted residuals against predicted values
# Note that both are based on natural log values
dat <- data.frame(xvar = y.hat, yvar = res)
p4 <- ggplot(dat, aes(x = xvar, y = yvar))+
  geom_point(shape=1)+ggtitle("Residual Plot")+
  xlab("Y-hat")+ylab("Deleted Residuals")

multiplot(p1, p2, p3, p4, cols = 2)


## MODEL VALIDATION
# Evaluate final model on model validation dataset
test.predictions <- predict(final_model,test.data)

# Store predicted results and actual results in data frame
# Note that predicted values are in USD, so that units match with wages from test data
actuals_preds.test <- data.frame(cbind(actual=test.data$wage, 
                                       predicted=round(exp(test.predictions),2)))

# Total number of predictors (represents total of all factors)
p.test <- 25

# Calculate SSE, SST, R^2, adjusted R^2, and AIC on test set
SSE.test <- sum((actuals_preds.test$actual - actuals_preds.test$predicted)^2)
SST.test <- sum((actuals_preds.test$actual - mean(actuals_preds.test$actual))^2)

test.r.squared <- 1 - (SSE.test/SST.test)
test.adj.r.squared <- 1 - ((n.test - 1)*(1 - test.r.squared)/(n.test - p.test))
test.AIC <- (n.test*log(SSE.test)) - ((n.test*log(n.test)) - 2*p.test) 



# Mean Square Error (training set)
# Note that predicted values are in USD, so that units match with wages from training data
actuals_preds.train <- data.frame(predicted = exp(y.hat), actual = round(train.data$wage,2))
SSE.train <- sum((actuals_preds.train$actual - actuals_preds.train$predicted)^2)
MSE.train <- SSE.train/(n.train - p.test)

# Mean Square Prediction Error (testing set)
MSPE.test <- SSE.test/n.test

# MSPE
paste(round(MSPE.test,2))

# MSE
paste(round(MSE.train,2))


## INFLUENTIAL OBSERVATIONS
# Calculate DFFITS measure
dist <- dffits(final_model)

# Save data point index and DFFITS measure in same data frame
dat <- data.frame(xvar = 1:n.train, yvar = dist, 
                  race = train.data$race)

# Plot DFFITS measure by index, highlighting race
p1 <- ggplot(dat, aes(x = xvar, y = yvar, color = race))+
  geom_point(shape=1)+ggtitle("DFFITS")+
  xlab("Index")+ylab("DFFITS")

# Add upper and lower thresholds for identifying influential observations
p1 <- p1 +
  geom_abline(intercept = 2*sqrt(p.test/n.train), slope = 0,colour = "black")+
  geom_abline(intercept = -2*sqrt(p.test/n.train), slope = 0,colour = "black")

p1


## MULTICOLLINEARITY
# Compute variance inflation factor of final model
print(xtable(vif(final_model)))
