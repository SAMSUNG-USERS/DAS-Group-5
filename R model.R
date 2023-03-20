library(dplyr)
library(GGally)
library(readr)
library(tidyverse)
library(gridExtra)
library(MASS)
library(pscl)
library(qcc)
dataset<-read.csv("C:/Users/CRsky/Desktop/das_project2/dataset5.csv")
#Delete "Region"
dataset<-dataset[,-2]
#let y be the first col
dataset<-dataset[, c(6, 2, 3, 4, 5, 1, 7, 8, 9, 10)]%>%
  filter(Total.Number.of.Family.members != 15 & Total.Number.of.Family.members != 14)
View(dataset)

#some plots
ggplot(dataset, aes(x= Total.Number.of.Family.members,  y = ..prop.., group=Household.Head.Sex, fill=Household.Head.Sex)) + 
  geom_bar(position="dodge", stat="count") +
  labs(y = "Proportion")

ggplot(dataset, aes(x = Total.Number.of.Family.members, y = ..count.., group = Type.of.Household, fill = Type.of.Household)) + 
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Proportion of Total Number of Family Members by Type of Household",
       y = "Count")

ggplot(dataset, aes(x = Type.of.Household, fill = Household.Head.Sex)) + 
  geom_bar() +
  labs(title = "Type of Household by Household Head's Sex", 
       y = "Count") 

ggplot(data = dataset, aes(x = as.factor(Total.Number.of.Family.members), y = log(Total.Household.Income), fill = Total.Number.of.Family.members)) +
  geom_boxplot() +
  labs(x = "Total.Number.of.Family.members", y = "Total.Household.Income") + 
  theme(legend.position = "none")

ggplot(data = dataset, aes(x = as.factor(Total.Number.of.Family.members), y = House.Age, fill = Total.Number.of.Family.members)) +
  geom_boxplot() +
  labs(x = "Total.Number.of.Family.members", y = "House.Age") + 
  theme(legend.position = "none")

ggplot(dataset, aes(x = Total.Number.of.Family.members, y = ..prop.., group = Household.Head.Sex, fill = Household.Head.Sex)) + 
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "Total Number of Family Members", y = "Proportion") +
  ggtitle("Household Head Gender Comparison") +
  theme(plot.title = element_text(hjust = 0.5))

hist(dataset$Total.Number.of.Family.members,breaks = 15,xlab = "family member count",main="Distribution of Y")
#plot between y and each x
ggpairs(dataset,
        upper=list(continuous=wrap("points", alpha=0.4, color="#d73027")),
        lower="blank", axisLabels="none")+
  theme(strip.text = element_text(size = rel(0.5)))

#Check the data type, change the type of categorical variables to factor
str(dataset)
dataset$Household.Head.Sex<-as.factor(dataset$Household.Head.Sex)
dataset$Type.of.Household<-as.factor(dataset$Type.of.Household)
dataset$Electricity<-as.factor(dataset$Electricity)
#dataset$Total.Number.of.Family.members<-as.factor(dataset$Total.Number.of.Family.members)
View(dataset)
#using poisson regression fit our dataset
model<-glm(Total.Number.of.Family.members ~ Total.Household.Income + Total.Food.Expenditure + Household.Head.Sex 
           +Type.of.Household + House.Floor.Area  + Number.of.bedrooms +Electricity, family = poisson, data = dataset)
summary(model)#AIC: 7153.1

#check whether over dispersion, p-value=0.058174 >0.05, good!
qcc.overdispersion.test(dataset$Total.Number.of.Family.members, type="poisson")

#check  (y-hat(mu)^2 and hat(mu), but in this code response variable should be int not factor
#ggplot(model, aes(x=log(fitted(model)), y=log((dataset$Total.Number.of.Family.members-fitted(model))^2)))+
  #geom_point(col="#f46d43") +
  #geom_abline(slope=1, intercept=0, col="#a6d96a", linewidth=1) +
  #ylab(expression((y-hat(mu))^2)) + xlab(expression(hat(mu)))

#Variables removed by significance level
#Remove this three variables one by one "House.Floor.Are, Number.of.bedrooms and Electricity"
#model1
model1<-glm(Total.Number.of.Family.members ~ Total.Household.Income + Total.Food.Expenditure + Household.Head.Sex 
           +Type.of.Household + House.Floor.Area  + Number.of.bedrooms, family = poisson, data = dataset)
summary(model1)#AIC: 7151.2
#model2
model2<-glm(Total.Number.of.Family.members ~ Total.Household.Income + Total.Food.Expenditure + Household.Head.Sex 
            +Type.of.Household + House.Floor.Area, family = poisson, data = dataset)
summary(model2)#AIC: 7151.7
#model3
model3<-glm(Total.Number.of.Family.members ~ Total.Household.Income + Total.Food.Expenditure + Household.Head.Sex 
            +Type.of.Household , family = poisson, data = dataset)
summary(model3)#AIC: 7152.8


#model: Negative binomial models
model1.nb<-glm.nb(Total.Number.of.Family.members ~ Total.Household.Income + Total.Food.Expenditure + Household.Head.Sex 
            +Type.of.Household + House.Floor.Area  + Number.of.bedrooms,data = dataset)
summary(model1.nb)

model2.nb<-glm.nb(Total.Number.of.Family.members ~ Total.Household.Income + Total.Food.Expenditure + Household.Head.Sex 
                  +Type.of.Household + House.Floor.Area ,data = dataset)
summary(model2.nb)

model3.nb<-glm.nb(Total.Number.of.Family.members ~ Total.Household.Income + Total.Food.Expenditure + Household.Head.Sex 
                  +Type.of.Household ,data = dataset)
summary(model3.nb)

#compare the Poisson and negative binomial models by looking at their deviances and AIC scores
c(model1$deviance, model1$aic)
c(model2$deviance, model2$aic)
c(model3$deviance, model3$aic)

c(model1.nb$deviance, model1.nb$aic)
c(model2.nb$deviance, model2.nb$aic)
c(model3.nb$deviance, model3.nb$aic)
# so we choose model 1
summary(model1)
#Coefficients of the model and interpretation
install.packages("epiDisplay")
library(epiDisplay)
poisgof(model1)#"Goodness-of-fit test for Poisson assumption"

exp(coef(model1))

idr.display(model)

# What other visualization functions are needed ?
# Predicted value?
# What else needs to be added


#some added plots

ggplot(dataset, aes(x = Total.Number.of.Family.members, y = ..prop.., group = Household.Head.Sex, fill = Household.Head.Sex)) + 
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "Total Number of Family Members", y = "Proportion", 
       title = "Proportion of Family Members by Head of Household Gender") +
  ggtitle("Household Head Gender Comparison") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(dataset, aes(x= Total.Number.of.Family.members,  y = ..prop.., group=Type.of.Household, fill=Type.of.Household)) + 
  geom_bar(position="dodge", stat="count") +
  labs(y = "Proportion")



ggplot(dataset, aes(x = Total.Number.of.Family.members, y = ..count.., group = Type.of.Household, fill = Type.of.Household)) + 
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Proportion of Total Number of Family Members by Type of Household",
       y = "Count")


ggplot(dataset, aes(x = Total.Number.of.Family.members, y = ..prop.., group = Household.Head.Sex, fill = Household.Head.Sex)) + 
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "Total Number of Family Members", y = "Proportion") +
  ggtitle("Household Head Gender Comparison") +
  theme(plot.title = element_text(hjust = 0.5))







