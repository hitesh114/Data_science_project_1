# Importing Dataset
Data <- read.csv("cleaned.csv", stringsAsFactors = FALSE)
# Viewing Dataset
View(Data)
# Importing Library
library(ggplot2)
# Plot for sex based on age
ggplot(Data, aes(x = Age_band_of_driver, fill = Sex_of_driver)) + theme_light() + 
  geom_bar() + 
  labs(y = "Count", 
       title = "Road Accident")
# Plot for experience based on sex
ggplot(Data, aes(x = Driving_experience, fill = Sex_of_driver)) + theme_light() + 
  geom_bar() + 
  labs(y = "Count", 
       title = "Road Accident")
# Plot for collision based on Vehicle movement
ggplot(Data, aes(x = Type_of_collision, fill = Vehicle_movement)) + theme_light() + 
  geom_bar() + 
  labs(y = "Count", 
       title = "Road Accident")
# Plot for collision based on Vehicle movement
ggplot(Data, aes(x = Age_band_of_driver, fill = Cause_of_accident)) + theme_light() + 
  geom_bar() + 
  labs(y = "Count", 
       title = "Road Accident")
# Plot for collision based on Vehicle movement
ggplot(Data, aes(x = Accident_severity, fill = Cause_of_accident)) + theme_light() + 
  geom_bar() + 
  labs(y = "Count", 
       title = "Road Accident")

# Density plot
ggplot(Data, aes(x = Age_band_of_driver, fill = Accident_severity)) + theme_bw() + 
  facet_wrap(Sex_of_driver~Driving_experience) + geom_density(alpha = 0.5) + 
  labs(y = "age", x = "survived", 
       title = "Road Accident")

# Slicing Data
cause= Data [,14]
severity= Data [,15]
# Dataset for accidents severity
data = data.frame(cause, severity)
View(data)
# Importing Data Splitting library
library(caTools)

# Generating Random Numbers
set.seed(123)
# Data Splitting
split = sample.split(data$severity, SplitRatio = 0.8)
# Training data
training_set = subset(data, split==TRUE)
View(training_set)
# Testing data
testing_set = subset(data, split==FALSE)
View(testing_set)
# Applying Regression
regressor = lm(formula = severity~cause, data = training_set)
# Prediction
y_pred = predict(regressor, newdata = testing_set)
View(y_pred)
# Visualization
plot(testing_set$cause, testing_set$severity, type = 'p', col = 'blue',
     xlab = 'Cause_of_Accident', ylab = 'Accident_Severity')
lines(testing_set$cause, y_pred, type = 'o', col = 'red')

# Visualization
# Importing Library
library(ggplot2)
ggplot(geom_point(aes(x = testing_set$cause, y = training_set$severity,
                      color = "red"))
       + geom_line(aes(x = training_set$cause, y = y_pred, 
                       color = "blue")) 
       + ggtitle("cause versus severity") 
       + xlab("cause") + ylab("severity")
)
