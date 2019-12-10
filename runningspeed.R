library (RODBC)
library (ggplot2)
library (tidyverse)
library (scatterplot3d)
library(rgl)
library (knitr)

#Regression analysis using R and data from MySQL database 
#Predict whether or not if less or more body weight affects the running speed of female amateur runners, all aged at 25-years-old.
#Analyze if blood type also has an impact on performance.
#Predict whether it is the combination of weight and blood type that positively or
#negatively impacts performance, with the average running speed of women being
#6.5 miles per hour. For example:
#(If X who is on Y diet and has Z blood type were to run a mile, X would most
#likely run _____mph | average, above average, or below average).
#Data is represented as follows:
#running_speed: Miles per hour
#body_weight: Pounds and ounces

#Establish connection from open database connection source

dbConnection<-odbcConnect("bloodsource")
sqlQuery(dbConnection, "select * from bloodtype")
data<-sqlQuery(dbConnection, "select * from bloodtype")

head(data)

str(data)

#Data Preparation

data$diet<-factor(data$diet)

data$blood_type<-factor(data$blood_type)

data$name<-factor(data$name, na.omit('.'))

data$name

data$'name'<-NULL

summary(data)

data$patient_id

data$body_weight

unique(data$diet)

unique(data$blood_type)

summary(femme_model)

summary(data)

str(data)

bloodtest<-round(nrow(data)*0.7)

testblood<-nrow(data)-(bloodtest)

bloodtest

testblood

#global variable that is set in the back-end
set.seed(123)

training_indices<- sample(seq_len(nrow(data)),size=bloodtest)

training_indices

training<-data[training_indices,]

testing<-data[-training_indices,]

training

testing

#what you eat, diet=independent variable| blood type or weight = independent variable
#affects performance= dependent variable
linear_model<-lm(formula = training$running_speed~.,data=training)

summary(linear_model)

lm_predict<-predict(linear_model, testing)
lm_predict

data$blood_type

#Shows Body Weight vs Running Speed | Downward Trend -the less body weight, the faster the runner
femmeplot<-ggplot(data, aes(x=running_speed, y=body_weight))+
  geom_point()+
  geom_smooth(method = 'lm')

femmeplot

print(femmeplot+labs(y="Body Weight (lbs)", x="Running Speed (mph)",
                     title="The Affect Weight Has on Speed"))

#Shows that the O-, B- and A- blood types have above average speeds
femmebplot<-ggplot(data, aes(x=running_speed, y=blood_type))+
  geom_point()+
  geom_smooth(method = 'lm')

femmebplot

print(femmebplot+labs(y="Blood Type", x="Running Speed (mph)",
                      title="The Affect Blood Type Has on Speed"))

#3D structure shows correlation of weight, blood type, and running speed
plot3d(data=data, data$running_speed,data$blood_type,
       data$body_weight, col="steelblue", size=8,
       xlab = "Running Speed (mph)",
       ylab = "Blood Type",
       zlab = "Body Weight (lbs)")
