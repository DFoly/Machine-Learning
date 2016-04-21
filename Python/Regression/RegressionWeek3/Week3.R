sales <- read.csv("C:/Users/dfoley/Dropbox/Machine Learning/PythonCoursera/Regression/RegressionWeek3/kc_house_data.csv")
require(dplyr)
sales = sales %>% arrange(sqft_living, price)

polynomial = function(feature, power){
  return(data.frame(sapply(1:power, function(x){(feature)**x})))
}

poly1_data = data.frame(sqft_living=sales$sqft_living)
poly1_data$price = sales$price


model1 = lm(price ~ sqft_living, data = poly1_data)
summary(model1)
model_params = coefficients(model1)
require(ggplot2)

p = predict(model1)
plot(p) ## plots weird but regression should plot normally

ggplot(poly1_data, aes(x=sqft_living, y=price)) + geom_point(alpha=0.25) +
  geom_abline(intercept=model_params[1], slope=model_params[2], color="red") +
  theme_bw()