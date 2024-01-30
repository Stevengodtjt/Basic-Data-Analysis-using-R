library(dplyr)
library(ggplot2)
library(moderndive)
library(gapminder)
library(sjPlot)
library(stats)
library(readr)
library(janitor)

# 1. Logistic regression with one numerical explanatory variable

# 1.1 Teaching evaluation scores

evals.gender <- evals %>%
  select(gender, age)

# Now, let’s look at a boxplot of age by gender to get an initial impression of the data:
ggplot(data = evals.gender, aes(x = gender, y = age, fill = gender)) +
  geom_boxplot() +
  labs(x = "Gender", y = "Age") +
  theme(legend.position = "none")

# Here we can see that the age of male teaching instructors tends to be higher than that of their female
# counterparts. Now, let’s fit a logistic regression model to see whether age is a significant predictor of the
# odds of a teaching instructor being male or female.



# 1.2 Log-odds
model <- glm(gender ~ age, data = evals.gender, family = binomial(link = "logit"))

#Now, let’s take a look at the summary produced from our logistic regression model:
model %>%
  summary()

# Firstly, the baseline category for our binary response is female. This is due to the default baseline in R
# being taken as the one which comes first alphabetically, which can be seen from the levels function:
levels(evals.gender$gender)


# we are also interested in producing a 95% confidence interval for these log-odds
mod.coef.logodds <- model %>%
  summary() %>%
  coef()
mod.coef.logodds


age.logodds.lower <- (mod.coef.logodds["age", "Estimate"] - 1.96 * mod.coef.logodds["age", "Std. Error"])
age.logodds.lower

age.logodds.upper <- (mod.coef.logodds["age", "Estimate"] + 1.96 * mod.coef.logodds["age", "Std. Error"])
age.logodds.upper

# Hence the point estimate for the log-odds is 0.06, 
# which has a corresponding 95% confidence interval of (0.04,0.08)
plot_model(model, show.values = TRUE, transform = NULL,
           title = "Log-Odds (Male instructor)", show.p = FALSE)

# Further details on using plot_model can be found here. Now, 
# let’s add the estimates of the log-odds to our data set:
evals.gender <- evals.gender %>%
  mutate(logodds.male = predict(model))
evals.gender 


# 1.3 Odds
model %>%
  coef() %>%
  exp()


# We can obtain a 95% confidence interval for the odds by simply exponentiating the lower and
# upper bounds of our log-odds interval:

age.odds.lower <- exp(age.logodds.lower)
age.odds.lower 
age.odds.upper <- exp(age.logodds.upper)
age.odds.upper


plot_model(model, show.values = TRUE,
           title = "Odds (Male instructor)", show.p = FALSE, axis.lim = c(1, 1.5))

# Now, let’s add the estimates of the odds to our data set:
evals.gender <- evals.gender %>%
  mutate(odds.male = exp(logodds.male))
evals.gender

# 1.4 Probabilities

# which can be computed in R as follows:
p.num <- (exp(mod.coef.logodds["(Intercept)", "Estimate"] + mod.coef.logodds["age", "Estimate"] * 52))
p.denom <- 1 + p.num
p.num / p.denom

# The plogis() function from the stats library can also be used to obtain probabilities from the log-odds:

plogis(mod.coef.logodds["(Intercept)", "Estimate"] + mod.coef.logodds["age", "Estimate"] * 52)

# Let’s add the probabilities to our data, which is done using the fitted() function:
evals.gender <- evals.gender %>%
  mutate(probs.male = fitted(model))

# Finally, we can plot the probability of being male using the geom_smooth() function by giving method =
#   "glm" and methods.args = list(family = "binomial") as follows:

ggplot(data = evals.gender, aes(x = age, y = probs.male)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(x = "Age", y = "Probability of instructor being male")

# The plot_model() function from the sjPlot package can also produce the estimated probabilities by age
# as follows:
plot_model(model, type = "pred", title = "",
           axis.title = c("Age", "Prob. of instructor being male"))

# 2 Logistic regression with one categorical explanatory variable

evals.ethnic <- evals %>%
  select(gender, ethnicity)
evals.ethnic

# Now, let’s look at a barplot of the proportion of males and females by ethnicity to get an initial impression
# of the data.

evals.ethnic %>%
  tabyl(ethnicity, gender) %>%
  adorn_percentages() %>%
  adorn_pct_formatting() %>%
  adorn_ns() # To show original counts


ggplot(evals.ethnic, aes(x = gender, group = ethnicity)) +
  geom_bar(aes(y = ..prop.., fill = ethnicity), stat = "count", position = "dodge") +
  labs(y = "Proportion", fill = "Ethnicity")


# 2.1 Log-odds

# The logistic regression model is given by:
model.ethnic <- glm(gender ~ ethnicity, data = evals.ethnic,
                    family = binomial(link = "logit"))

model.ethnic %>%
  summary()

# Again, the baseline category for our binary response is female. 
# Also, the baseline category for our explanatory variable is minority, 
# which, like gender, is done alphabetically by default by R:
levels(evals.ethnic$gender)
levels(evals.ethnic$ethnicity)

# we are also interested in producing a 95% confidence interval for these log-odds. 
# This can be done as follows:
mod.ethnic.coef.logodds <- model.ethnic %>%
  summary() %>%
  coef()

ethnic.logodds.lower <- (mod.ethnic.coef.logodds["ethnicitynot minority", "Estimate"] - 1.96 *
                           mod.ethnic.coef.logodds["ethnicitynot minority", "Std. Error"])
ethnic.logodds.lower

ethnic.logodds.upper <- (mod.ethnic.coef.logodds["ethnicitynot minority", "Estimate"] + 1.96 *
                           mod.ethnic.coef.logodds["ethnicitynot minority", "Std. Error"])
ethnic.logodds.upper

# Hence the point estimate for the log-odds is 0.66, 
# which has a corresponding 95% confidence interval of (0.13,1.2).
plot_model(model.ethnic, show.values = TRUE, transform = NULL,
           title = "Log-Odds (Male instructor)", show.p = FALSE)

# Now, let’s add the estimates of the log-odds to our data set:
evals.ethnic <- evals.ethnic %>%
  mutate(logodds.male = predict(model.ethnic))
evals.ethnic

# 2.2 Odds

# On the odds scale the regression coefficients are given by
model.ethnic %>%
  coef() %>%
  exp()

# Now, the odds-ratio of an instructor being male in the not minority compared to the minority ethnic
# group is found as follows:

# We can obtain a 95% confidence interval for the odds by simply exponentiating the lower and upper bounds
# of the log-odds interval:
ethnic.odds.lower <- exp(ethnic.logodds.lower)
ethnic.odds.lower

ethnic.odds.upper <- exp(ethnic.logodds.upper)
ethnic.odds.upper

# Hence the point estimate for the odds-ratio is 1.94, which has a corresponding 95% confidence interval of
# (1.14, 3.31). Again, we can display this graphically using the plot_model function from the sjPlot package:
plot_model(model.ethnic, show.values = TRUE,
           title = "Odds (Male instructor)", show.p = FALSE)


# Now, let’s add the estimates of the odds to our data set:
evals.ethnic <- evals.ethnic %>%
  mutate(odds.male = exp(logodds.male))
evals.ethnic


# 2.3 Probabilities

# The probabilities of an instructor being male given they are in the minority and not minority groups are
plogis(mod.ethnic.coef.logodds["(Intercept)", "Estimate"])

plogis(mod.ethnic.coef.logodds["(Intercept)", "Estimate"] +
         mod.ethnic.coef.logodds["ethnicitynot minority", "Estimate"])

# Hence, the probabilities of an instructor being male given they are in the minority and not minority ethnic
# groups are 0.437 and 0.602, respectively.

# Let’s add the probabilities to our data:
evals.ethnic <- evals.ethnic %>%
  mutate(probs.male = fitted(model.ethnic))

evals.ethnic

# Finally, we can use the plot_model() function 
# from the sjPlot package to produce the estimated probabilities by ethnicity as follows:

plot_model(model.ethnic, type = "pred", title = "",
           axis.title = c("Ethnicity", "Prob. of instructor being male"))

# 3 Further Tasks

# 3.1 Yanny or Laurel?

# This auditory illusion first appeared on the internet in May 2018. An explanation of why people hear different
# things can be found in this short video, just one of many internet sources discussing the phenomenon. The
# main reason behind the difference appears to be that as we age we lose the ability to hear certain sounds. To
# see if we could find evidence of such an age effect, we asked students and staff at the School of Mathematics
# and Statistics at the University of Glasgow to fill out a survey on what they hear. Below you can see
# summaries of the responses.

# The proportions hearing Yanny and Laurel are very similar to each other, and there are some respondents
# who hear both or even something completely different. This may be because people do not listen to the
# audio file using the same device, something we couldn’t control for in the survey. Ignoring the responses
# other than Yanny or Laurel, we have 53 observations.
# Download the data (yanny.csv) from Moodle and fit a logistic regression model with hear as the binary
# response variable, and age and gender as the explanatory variables. What are your findings?

yanny <- read_csv("C:/Users/Steven god tjt/Desktop/课程/2022.1sms2/Data analysis/Week 9 Generalised Linear Models/yanny.csv")
yanny <- yanny %>%
  select(hear, gender, age)
yanny$hear <- as.factor(yanny$hear)
yanny$gender <- as.factor(yanny$gender)

ggplot(data = yanny, aes(x = hear, y = age, fill = hear)) +
  geom_boxplot() +
  labs(x = "What do you hear?", y = "Age") +
  theme(legend.position = "none")

# We see in the boxplot that the people who hear Yanny are, on average, younger, however there is some
# overlap in the IQR’s.
yanny %>%
  tabyl(gender, hear) %>%
  adorn_percentages() %>%
  adorn_pct_formatting() %>%
  adorn_ns() # To show original counts

ggplot(data = yanny, aes(x = hear, group = gender)) +
  geom_bar(aes(y = ..prop.., fill = gender), stat = "count", position = "dodge") +
  labs(x = "What do you hear?", y = "Proportion")

# There is a slightly smaller proportion of men hearing Yanny, but the proportions are very similar overall.

mod.yanny <- glm(hear ~ age + gender, data = yanny, family = binomial(link = "logit"))
mod.yanny %>%
  summary()


mod.yanny <- glm(hear ~ age, data = yanny, family = binomial(link = "logit"))
mod.yanny %>%
  summary()

# Notice that the coefficient of age is negative, suggesting that older people are less likely to hear Yanny.
# However, the coefficient of age is not significant (p-value of 0.16). Still, if we wanted to use the estimated
# coefficient to quantify the effect of age, we would need to look at exp(-0.04812) = 0.953. This suggests that
# for two people who differ by one year in age, the older person’s odds of hearing Yanny are 0.953 times those
# of the younger person. If we want to look at a ten-year age difference then the odds multiplier becomes
# exp(0.04812 * 10) = 1.618. Hence, for two people who differ by 10 years in age, the older person’s odds of
# hearing Yanny are 1.618 times those of the younger person.

plot_model(mod.yanny, show.values = TRUE,
           title = "Odds (Age)", show.p = TRUE)

plot_model(mod.yanny, type = "pred", title = "",
           axis.title = c("Age", "Probability of hearing Yanny"))


# 3.2 Titanic

# On 15th April 1912, during its maiden voyage, the Titanic sank after colliding with an iceberg, killing 1502
# out of 2224 passengers and crew. One of the reasons that the shipwreck led to such loss of life was that there
# were not enough lifeboats for the passengers and crew. Although there was some element of luck involved
# in surviving the sinking, some groups of people were more likely to survive than others, such as women,
# children, and the upper-class.

# Download the data (titanic.csv) from Moodle for n = 891 passengers aboard the Titanic and fit a logistic
# regression model with survived as the binary response variable, and age, gender, and passenger.class
# as the explanatory variables. What are your findings?

titanic <- read_csv("C:/Users/Steven god tjt/Desktop/课程/2022.1sms2/Data analysis/Week 9 Generalised Linear Models/titanic.csv")
titanic <- titanic %>%
  select(survived, age, gender, passenger.class)

titanic$survived <- as.factor(titanic$survived)
levels(titanic$survived) <- c("Died", "Survived")
titanic$gender <- as.factor(titanic$gender)
titanic$passenger.class <- as.factor(titanic$passenger.class)


ggplot(data = titanic, aes(x = survived, y = age, fill = survived)) +
  geom_boxplot() +
  labs(x = "Survived the Titanic?", y = "Age") +
  theme(legend.position = "none")

# We see in the boxplot that there is very little difference in the age of passengers who died or survived the
# sinking of the Titanic.

titanic %>%
  tabyl(gender, survived) %>%
  adorn_percentages() %>%
  adorn_pct_formatting() %>%
  adorn_ns() # To show original counts

ggplot(data = titanic, aes(x = survived, group = gender)) +
  geom_bar(aes(y = ..prop.., fill = gender), stat = "count", position = "dodge") +
  labs(x = "Survived the Titanic?", y = "Proportion")

# There is a clear pattern here with the proportion surviving much higher for females than for males.

titanic %>%
  tabyl(passenger.class, survived) %>%
  adorn_percentages() %>%
  adorn_pct_formatting() %>%
  adorn_ns() # To show original counts

ggplot(data = titanic, aes(x = survived, group = passenger.class)) +
  geom_bar(aes(y = ..prop.., fill = passenger.class),
           stat = "count", position = "dodge") +
  labs(x = "Survived the Titanic?", y = "Proportion")

# The largest group of passengers who died were third class passengers, while among those who survived the
# largest group was first class passengers.

mod.titanic <- glm(survived ~ gender + passenger.class + age, data = titanic,
                   family = binomial(link = "logit"))
mod.titanic %>%
  summary()

# We see that the coefficient for males (gendermale) is negative, indicating a lower chance of survival for male
# passengers. Similarly, the coefficients for second (passenger.class2) and third (passenger.class3) class
# passengers are negative, with the magnitude of the third class coefficient larger than that of the second class
# coefficient. This suggests that second class passengers chances of survival were worse in comparison with
# first class passengers, and that third class passengers chances of survival were even worse. Finally the age
# coefficient is negative, suggesting that older people were less likely to survive.

plot_model(mod.titanic, show.values = TRUE,
           title = "", show.p = FALSE, value.offset = 0.25)

# We interpret the odds ratios as follows: men’s odds of survival were 0.07 times those of women, third class
# passengers’ odds of survival were 0.10 times those of first class passengers, and second class passengers’ odds
# of survival were 0.33 times those of first class passengers. Finally, for each year increase in the passenger’s
# age, their odds of survival decrease (by a factor of 0.97).







  


































