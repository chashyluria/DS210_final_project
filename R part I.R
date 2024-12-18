# load the dataset into R
data <- read.csv(file.choose())
car_data <- data[1:300, ]
str(car_data)

#run simple linear regression
slr_model <- lm(car_data$mpg ~ car_data$weight, data = car_data)
summary(slr_model)

#run multiple linear regression
mlr_model <- lm(mpg ~ weight + cylinder + modelyear, data = car_data)
summary(mlr_model)