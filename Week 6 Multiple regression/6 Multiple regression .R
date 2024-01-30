#1.Introduction

# Here, we shall now examine fitting regression models with more than one
# explanatory variable. This is known as multiple regression.
# When fitting regression models with multiple explanatory variables, the interpretation of an explanatory
# variable is made in association with the other variables.


# 2 Regression modelling with two numerical explanatory variables
install.packages("ISLR")
install.packages("plotly")
library(ggplot2)
library(dplyr)
library(moderndive)
library(ISLR)
library(skimr)
library(plotly)
library(tidyr)


# 2.1 Exploratory data analysis

# Task: Start by subsetting the Credit data set so that we only have the variables we are interested in, that
# is, Balance, Limit and Income. Note, it is best to give your new data set a different name than Credit as
# to not overwrite the original Credit data set. We can the glimpse function to take a look at our new data
# set (named Cred in this case):
Cred<-Credit %>%
  select(Balance,Limit,Income)

glimpse(Cred)

# Now, let’s take a look at summary statistics relating to our data set using the skim function:
Cred %>%
  skim()


# Now that we are looking at the relationship between an outcome variable and multiple explanatory variables,
# we need to examine the correlation between each of them.
Cred %>%
  cor()

# From our correlation table we can see that the correlation between our two explanatory variables is 0.792,
# which is a strong positive linear relationship. Hence, we say there is a high degree of collinearity between
# our explanatory variables.

# Let’s now produce scatterplots of the relationship between the outcome variable and the explanatory variables.
# First, we shall look at the scatterplot of Balance against Limit:
ggplot(Cred, aes(x = Limit, y = Balance)) +
  geom_point() +
  labs(x = "Credit limit (in $)", y = "Credit card balance (in $)",
       title = "Relationship between balance and credit limit") +
  geom_smooth(method = "lm", se = FALSE)

# Now, let’s look at a scatterplot of Balance and Income:
ggplot(Cred, aes(x = Income, y = Balance)) +
  geom_point() +
  labs(x = "Income (in $1000)", y = "Credit card balance (in $)",
       title = "Relationship between balance and income") +
  geom_smooth(method = "lm", se = FALSE)

# The two scatterplots above focus on the relationship between the outcome variable Balance and each of the
# explanatory variables independently. In order to get an idea of the relationship between all three variables
# we can use the plot_ly function within the plotly library to plot a 3-dimensional scatterplot as follows:
plot_ly(Cred, x = ~Income, y = ~Limit, z = ~Balance,
        type = "scatter3d", mode = "markers")

# In Week 3, when we fit our regression model, we were looking at the best-fitting line. However, now that we
# have more than one explanatory variable, we are looking at the best-fitting plane, which is a 3-dimensional
# generalisation of the best-fitting line.


# 2.2 Formal analysis

# Similarly to Week 3, we use the lm function to fit the regression model and the get_regression_table
# function to view our parameter estimates:
Balance.model <- lm(Balance ~ Limit + Income, data = Cred)
get_regression_table(Balance.model)

# 2.3 Assessing model fit

# First, we need to obtain the fitted values and residuals from our regression model:
regression.points <- get_regression_points(Balance.model)
regression.points

# We can assess our first two model assumptions by producing scatterplots of our residuals against each of our
# explanatory variables. First, let’s begin with the scatterplot of the residuals against credit limit:
ggplot(regression.points, aes(x = Limit, y = residual)) +
  geom_point() +
  labs(x = "Credit limit (in $)", y = "Residual", title = "Residuals vs credit limit") +
  geom_hline(yintercept = 0, col = "blue", size = 1)

# Now, let’s plot a scatterplot of the residuals against income:
ggplot(regression.points, aes(x = Income, y = residual)) +
  geom_point() +
  labs(x = "Income (in $1000)", y = "Residual", title = "Residuals vs income") +
  geom_hline(yintercept = 0, col = "blue", size = 1)

# Finally, we can check if the residuals are normally distributed by producing a histogram:
ggplot(regression.points, aes(x = residual)) +
  geom_histogram(color = "white") +
  labs(x = "Residual")


# 3 Regression modelling with one numerical and one categorical explanatory variable

View(evals) #y:score x1:age(numerical)  x2:gender(categorical)

# 3.1 Exploratory data analysis

# Start by subsetting the evals data set so that we only have the variables we are interested in, that is, score,
# age and gender
eval.score<-evals %>%
  select(score,age,gender)
eval.score

eval.score %>%
  skim()

# Now, let’s compute the correlation coefficient between our outcome variable score 
# and our numerical explanatory variable age:
eval.score %>%
  get_correlation(formula = score ~ age)

#The correlation coefficient only exists between numerical variables, which is why we do not include
#our categorical variable gender.

# We can now visualise our data by producing a scatterplot, where seeing as we have the categorical variable
# gender, we shall plot the points using different colours for each gender:
ggplot(eval.score, aes(x = age, y = score, color = gender)) +
  geom_jitter() +
  labs(x = "Age", y = "Teaching Score", color = "Gender") +
  geom_smooth(method = "lm", se = FALSE)


# 3.2 Multiple regression: parallel slopes model

# We can fit the parallel regression lines model as follows:
par.model <- lm(score ~ age + gender, data = eval.score)
get_regression_table(par.model)

# Now, let’s superimpose our parallel regression lines onto the scatterplot of teaching score against age:
coeff <- par.model %>%
  coef() %>%
  as.numeric()

slopes <- eval.score %>%
  group_by(gender) %>%
  summarise(min = min(age), max = max(age)) %>%
  mutate(intercept = coeff[1]) %>%
  mutate(intercept = ifelse(gender == "male", intercept + coeff[3], intercept)) %>%
  gather(point, age, -c(gender, intercept)) %>%
  mutate(y_hat = intercept + age * coeff[2])

ggplot(eval.score, aes(x = age, y = score, col = gender)) +
  geom_jitter() +
  labs(x = "Age", y = "Teaching Score", color = "Gender") +
  geom_line(data = slopes, aes(y = y_hat), size = 1)


# 3.3 Multiple regression: interaction model

# In order to fit an interaction term within our regression model we replace the + sign with the * sign as
# follows:
int.model <- lm(score ~ age * gender, data = eval.score)
get_regression_table(int.model)


# 3.4 Assessing model fit

# Now we have to assess the fit of the model by looking at plots of the residuals. We shall do this for the
# interaction model. First, we need to obtain the fitted values and residuals from the interaction model as
# follows:

regression.points <- get_regression_points(int.model)
regression.points

# Let’s start by looking at a scatterplot of the residuals against the explanatory variable by gender:
ggplot(regression.points, aes(x = age, y = residual)) +
  geom_point() +
  labs(x = "age", y = "Residual") +
  geom_hline(yintercept = 0, col = "blue", size = 1) +
  facet_wrap(~ gender)

# Now, we can plot the residuals against the fitted values:
ggplot(regression.points, aes(x = score_hat, y = residual)) +
  geom_point() +
  labs(x = "Fitted values", y = "Residual") +
  geom_hline(yintercept = 0, col = "blue", size = 1) +
  facet_wrap(~ gender)

# Finally, let’s plot histograms of the residuals to assess whether they are normally distributed with mean
# zero:
ggplot(regression.points, aes(x = residual)) +
  geom_histogram(binwidth = 0.25, color = "white") +
  labs(x = "Residual") +
  facet_wrap(~gender)


# Question: Do the model assumptions hold?




# 4 Tasks

# 4.1 Assess the model assumptions for the parallel regression lines model. Do they appear valid?

# 4.2 Return to the Credit data set and fit a multiple regression model with Balance as the outcome variable,
# and Income and Age as the explanatory variables, respectively. Assess the assumptions of the multiple
# regression model.

# 4.3 Return to the Credit data set and fit a parallel regression lines model with Balance as the outcome
# variable, and Income and Student as the explanatory variables, respectively. Assess the assumptions
# of the fitted model.

# Trickier
# 4.4 Load the library datasets and look at the iris data set of Edgar Anderson containing measurements (in centimetres) on 150 different flowers across three different species of iris. Fit an interaction
# model with Sepal.Width as the outcome variable, and Sepal.Length and Species as the explanatory
# variables. Assess the assumptions of the fitted model.










