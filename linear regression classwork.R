install.packages("tidyverse")
library(tidyverse)
library(ggplot2)


# Create a regression plot with weight and mpg

ggplot(data, aes(x = wt, y = mpg)) + geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "turquoise4" )+
  theme_minimal()+
  labs(x = " Weight", y = "mpg", 
       title = "Linear model of weight and mpg")


# Problem 1: Which variables do you think might reasonably predict mpg? Name two and explain why.

View(mtcars)
cor( mtcars)

# number of cylinders, displacement, horsepower, and weight all should effecrt mpg. 
# can also be seen by looking at the corrrelation matrix.

# Problem 2:  Fit a model predicting mpg from weight. Write the regression equation using the model output.
# Interpret the slope in context-- what does it tell you?

ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()

modelWeight <- lm(mpg ~ wt, data = mtcars)

summary(modelWeight)

# regression equation is y = -5.3445x + 37.2851 
# as weight increases by 1 unit, mpg decreases by 5.3445

  

# Problem 3: Plot the model from problem 2. Do the residuals appear randomly scattered (supporting linearity)?
plot(modelWeight)

# Yes, residuals apprear randomly scattered supporting claim of linearity
  
  
# Problem 4: Use the model to predict mpg for a car that weights 3,000 lbs. Report the predicted mpg. 
# Does this value seem to be reasonable? Why or why not?


# regression equation is y = -5.3445x + 37.2851
-5.3445 * 3.0 + 37.2851

# Predicted mpg is 21.2516 and the actual value is also around this
# After examining the scatterplot, yes it does


