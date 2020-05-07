#Simple linear regression

# Importing the dataset
dataset = read.csv('Salary_Data.csv')

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set = data.frame(scale(training_set))
test_set = data.frame(scale(test_set))

#fitting simple linear regression to test data
regressor=lm(formula = Salary~YearsExperience, data=training_set)
summary(regressor)

#predicting the test results
y_pred=predict(regressor,newdata = test_set)

#Visualising the training set results
#install.packages('ggplot2')
library(ggplot2)
ggplot()+
  geom_point(aes(training_set$YearsExperience,training_set$Salary),color='red')+
  geom_line(aes(training_set$YearsExperience,predict(regressor,newdata = training_set)),
            color='blue')+
  ggtitle("Salary vs Experience(Train Data)")+
  xlab('Years of Experience')+
  ylab('Salary')

#Visualising the test set results
ggplot()+
  geom_point(aes(test_set$YearsExperience,test_set$Salary),color='red')+
  geom_line(aes(training_set$YearsExperience,predict(regressor,newdata = training_set)),
            color='blue')+
  ggtitle("Salary vs Experience(Test Data)")+
  xlab('Years of Experience')+
  ylab('Salary')
