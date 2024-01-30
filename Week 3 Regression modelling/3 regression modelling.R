# 3 Simple linear regression with one numerical explanatory variable

install.packages("ggplot2")
install.packages("dplyr")
install.packages("moderndive")
install.packages("gapminder")
install.packages("skimr")
library(ggplot2)
library(dplyr)
library(moderndive)
library(gapminder)
library(skimr)

# 3.1 Exploratory data analysis
View(evals)

evals.scores <- evals %>%
  select(score, bty_avg)  #only really interested in the instructors teaching (score) and beauty (bty_avg) scores

evals.scores  #score:1-5;bty_avg:1-10

#As both variables are numerical, we can compute
#summary statistics for them using the skim function from the skimr package
evals.scores %>%  
  skim()


# 3.2 Correlation（get_correlation function from the moderndive package.）
evals.scores %>%
  get_correlation(formula = score ~ bty_avg)

# The next step in our exploratory data analysis is to visualise the data using appropriate plotting techniques.
# Here, a scatterplot is appropriate since both score and bty_avg are numerical variables:

ggplot(evals.scores, aes(x = bty_avg, y = score)) +
  geom_point()

###over plotting
ggplot(evals.scores, aes(x = bty_avg, y = score)) +
  geom_point()+
  geom_jitter(width = 0.1, height = 0.1)


#3.3 Formal analysis

# This can be done in R using the lm function to fit a linear model to the data
model <- lm(score ~ bty_avg, data = evals.scores)
model

# we can superimpose our best-fitting line onto our scatterplot to see how it fits through the points
# using the geom_smooth function

ggplot(evals.scores, aes(x = bty_avg, y = score)) +
  geom_point() +
  labs(x = "Beauty Score", y = "Teaching Score",
       title = "Relationship of teaching and beauty scores") +
  geom_smooth(method = "lm", se = FALSE)

# To obtain the fitted values and residuals for all instructors within the data set we can use the
# get_regression_points function:
regression.points <- get_regression_points(model)
regression.points

# 3.4 Assessing model fit

# Assumption 1:
# The deterministic part of the model captures all the non-random structure in the data, i.e. the residuals
# have mean zero.


# we should be able to check that the explanatory variable has a linear relationship
# with the outcome variable (score). We can plot the residuals against our explanatory variable
ggplot(regression.points, aes(x = bty_avg, y = residual)) +
  geom_point() +
  labs(x = "Beauty Score", y = "Residual") +
  geom_hline(yintercept = 0, col = "blue", size = 1)



# Assumption 2:
# The scale of the variability of the residuals is constant at all values of the explanatory variables.

# We can examine our first two assumptions by also plotting the residuals against the fitted values
ggplot(regression.points, aes(x = score_hat, y = residual)) +
  geom_point() +
  labs(x = "Fitted values", y = "Residual") +
  geom_hline(yintercept = 0, col = "blue", size = 1)


# Assumption 3:
# The residuals are normally distributed.

# To assess our third assumption that the residuals are normally distributed we can simply plot a histogram
# of the residuals:
ggplot(regression.points, aes(x = residual)) +
  geom_histogram(binwidth = 0.25, color = "white") +
  labs(x = "Residual")


# Assumption 4:
# The residuals are independent.

# Assumption 5:
# The values of the explanatory variables are recorded without error.


# 4 Simple linear regression with one categorical explanatory variable

# 4.1 Exploratory data analysis
View(gapminder)

gapminder2007 <- gapminder %>%
  filter(year == 2007) %>%
  select(country, continent, lifeExp)

glimpse(gapminder2007)

# Similarly to our previous exploratory data analysis, we can obtain summary statistics using the
# skim function. First, let’s take a look at the life expectancy (lifeExp) and continent variables:
gapminder2007 %>%
  select(continent, lifeExp) %>%
  skim()

# We can summarise any differences in life expectancy by continent by taking a look at the median and mean
# life expectancies of each continent using the group_by and summarize functions

lifeExp.continent <- gapminder2007 %>%
  group_by(continent) %>%
  summarize(median = median(lifeExp), mean = mean(lifeExp))
lifeExp.continent 

# Boxplots are often used when examining the distribution of a numerical outcome variable across different
# levels of a categorical variable:
ggplot(gapminder2007, aes(x = continent, y = lifeExp)) +
  geom_boxplot() +
  labs(x = "Continent", y = "Life expectancy (years)",
       title = "Life expectancy by continent")

# 4.2 Formal analysis

# Now let us fit our regression model to the data, where lifeExp is our outcome variable y and continent is
# our categorical explanatory variable x:
lifeExp.model <- lm(lifeExp ~ continent, data = gapminder2007)
lifeExp.model


# 4.3 Assessing model fit

# Let’s explore the gapminder2007 data set in order to understand how they work.
gapminder2007

# Here, we see the life expectancy of each country and the continent they are from. For example, let’s remember
# the life expectancies of Afghanistan (43.8) and Bahrain (75.6). Now, we can obtain the fitted values and
# residuals in the same way we did previously:
regression_points <- get_regression_points(lifeExp.model)
regression_points

#For assessing the assumptions surrounding the residuals for a categorical explanatory variable, we can plot
#the residuals for each continent:
ggplot(regression_points, aes(x = continent, y = residual)) +
  geom_jitter(width = 0.1) +
  labs(x = "Continent", y = "Residual") +
  geom_hline(yintercept = 0, col = "blue")


#To check that the residual errors are normally distributed, we plot a histogram of them:
ggplot(regression_points, aes(x = residual)) +
  geom_histogram(binwidth = 5, color = "white") +
  labs(x = "Residual")



#TASK 1
# Examine the relationship between teaching score and age in the evals data set. What is the value of
# the correlation coefficient? How would you interpret this verbally? Finally, produce a scatterplot of
# teaching score and age.


#TASK 2
# Perform a formal analysis of the relationship between teaching score and age by fitting a simple linear
# regression model. Superimpose your best-fitting line onto your scatterplot from Task 2.


#TASK 3
# Assess the model assumptions from Task 2 by plotting the residuals against the explanatory variable
# and fitted values, respectively. Also, plot a histogram of the residuals to assess whether they are
# normally distributed.

#TASK 4
# Perform the same analysis we did on life expectancy from the gapminder data set in 2007. However,
# subset the data for the year 1997. Are there any differences in the results across this 10 year period?









