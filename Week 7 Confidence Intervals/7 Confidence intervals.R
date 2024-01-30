install.packages("infer")
library(dplyr)
library(ggplot2)
library(janitor)
library(moderndive)
library(infer)


#1.Bootstrapping

# The moderndive package contains a sample of 40 pennies collected and minted in the United States. 
# Let’s explore this sample data first:
orig_pennies_sample

# (1).Exploratory data analysis

# First, let’s visualise the values in this sample as a histogram:
ggplot(orig_pennies_sample, aes(x = age_in_2011)) +
  geom_histogram(bins = 10, color = "white") +
  labs(x = "Age of penny in 2011.")

# We see a roughly symmetric distribution here that has quite a few values near 20 years in age with only a
# few larger than 40 years or smaller than 5 years. If orig_pennies_sample is a representative sample from
# the population, we’d expect the age of all US pennies collected in 2011 to have a similar shape, a similar
# spread, and similar measures of central tendency like the mean.

# So where does the mean value fall for this sample? This point will be known as our point estimate and
# provides us with a single number that could serve as the guess to what the true population mean age might
# be. Recall how to find this using the dplyr package:
x_bar <- orig_pennies_sample %>%
  summarize(stat = mean(age_in_2011))
x_bar  
  
  
# (2).The bootstrapping process
bootstrap_sample1 <- orig_pennies_sample %>%
  rep_sample_n(size = 40, replace = TRUE, reps = 1)
  
# Let’s visualise what this new bootstrap sample looks like: 
ggplot(bootstrap_sample1, aes(x = age_in_2011)) +
  geom_histogram(bins = 10, color = "white") +
  labs(x = "Age of penny in 2011.")  
  
# We now have another sample from what we could assume comes from the population of interest. We can
# similarly calculate the sample mean of this bootstrap sample, called a bootstrap statistic.
bootstrap_sample1 %>%
  summarize(stat = mean(age_in_2011))

# We can see that this sample mean is different to the x_bar value we calculated earlier for the
# orig_pennies_sample data.


# The process of sampling with replacement is how we can use the original sample to take a guess as to what
# other values in the population may be. Sometimes in these bootstrap samples, we will select lots of larger
# values from the original sample, sometimes we will select lots of smaller values, and most frequently we
# will select values that are near the center of the sample. 

#Let’s explore what the distribution of values of age_in_2011 
#for six different bootstrap samples looks like to further understand this variability.
six_bootstrap_samples <- orig_pennies_sample %>%
  rep_sample_n(size = 40, replace = TRUE, reps = 6)

ggplot(six_bootstrap_samples, aes(x = age_in_2011)) +
  geom_histogram(bins = 10, color = "white") +
  facet_wrap(~ replicate) +
  labs(x = "Age of penny in 2011.")

# We can also look at the six different means using dplyr syntax:
six_bootstrap_samples %>%
  group_by(replicate) %>%
  summarize(stat = mean(age_in_2011))


# 2.The infer package for statistical inference

# (1). Specify variables

# To begin to create a confidence interval for the population mean age of US pennies in 2011, we start by using
# specify() to choose which variable in our orig_pennies_sample data we’d like to work with. This can be
# done in one of two ways:

orig_pennies_sample %>%
  specify(response = age_in_2011)  # method 1. Using the response argument:


orig_pennies_sample %>%
  specify(formula = age_in_2011 ~ NULL) #method 2. Using formula notation:


# (2).Generate replicates

# Let’s generate 1000 bootstrap samples:
thousand_bootstrap_samples <- orig_pennies_sample %>%
  specify(response = age_in_2011) %>%
  generate(reps = 1000)

thousand_bootstrap_samples

# We can use the dplyr count function to help us understand what the thousand_bootstrap_samples data
# frame looks like:
thousand_bootstrap_samples %>%
  count(replicate)

# Notice that each replicate has 40 entries here. Now that we have 1000 different bootstrap samples, our
# next step is to calculate the bootstrap statistics for each sample.


# (3).Calculate summary statistics

# we now want to calculate the mean age_in_2011 for each
# bootstrap sample. To do so, we use the stat argument and set it to "mean" below. The stat argument
# has a variety of different options here and we will see further examples of this throughout the remaining
# chapters.
bootstrap_distribution <- orig_pennies_sample %>%
  specify(response = age_in_2011) %>%
  generate(reps = 1000) %>%
  calculate(stat = "mean")

bootstrap_distribution


####### Observed statistic / point estimate calculations

# Just as group_by() %>% summarize() produces a useful workflow in dplyr, we can also use specify()
# %>% calculate() to compute summary measures on our original sample data
orig_pennies_sample %>%
  summarize(stat = mean(age_in_2011))

#This can also be done by skipping the generate step in the pipeline feeding specify directly into calculate:
orig_pennies_sample %>%
  specify(response = age_in_2011) %>%
  calculate(stat = "mean")


# (4).Visualise the results
bootstrap_distribution %>%
  visualize()

# 3.Constructing confidence intervals

bootstrap_distribution %>%
  visualize(obs_stat= x_bar)

# We can also compute the mean of the bootstrap distribution of means to see how it compares to x_bar:
bootstrap_distribution %>%
  summarize(mean_of_means = mean(stat))

# (1).The percentile method

bootstrap_distribution %>%
  get_ci(level = 0.95, type = "percentile")


percentile_ci <- bootstrap_distribution %>%  #default: level=0.95 type="percentile"
  get_ci()
percentile_ci

# We can use the visualize function to view this using the endpoints and direction arguments, 
# setting direction to "between" (between the values) and endpoints to be those
# stored with name percentile_ci.
bootstrap_distribution %>%
  visualize(endpoints = percentile_ci, direction = "between")


# (2). The standard error method

# Note that the center of the confidence interval (the point_estimate) must be provided for the standard
# error confidence interval.
standard_error_ci <- bootstrap_distribution %>%
  get_ci(type = "se", point_estimate = x_bar)
standard_error_ci 

bootstrap_distribution %>%
  visualize(endpoints = standard_error_ci, direction = "between")


# 4. Interpreting the confidence interval

# So let’s assume that pennies is our population of interest (i.e. a population with N = 800 units). We can
# therefore calculate the population mean age of pennies in 2011, denoted by the Greek letter µ, by calculating
# the mean of age_in_2011 for the pennies data frame.
pennies_mu <- pennies %>%
  summarize(overall_mean = mean(age_in_2011)) %>%
  pull() # we use this to extract a single value from the data frame

pennies_mu

# Note that the value µ = 21.15 (i.e.the mean of pennies calculated above) 
# does fall in this confidence interval. So in this instance, 
# the confidence interval based on orig_pennies_sample was a good estimate of µ.

# If we had a different sample of size 40 and constructed a confidence interval using the same method, would
# we be guaranteed that it contained the population parameter value µ as well? Let’s try it out:
orig_pennies_sample2 <- pennies %>%
  sample_n(size = 40)

orig_pennies_sample2 


# We next create an infer pipeline to generate a percentile-based 95% confidence interval for µ:
percentile_ci2 <- orig_pennies_sample2 %>%
  specify(formula = age_in_2011 ~ NULL) %>%
  generate(reps = 1000) %>%
  calculate(stat = "mean") %>%
  get_ci()

percentile_ci2  #This new confidence interval also contains the value of µ


# 5.Comparing two proportions

mythbusters_yawn

# We can use the janitor package to get a glimpse into this data in a table format:
mythbusters_yawn %>%
  tabyl(group, yawn) %>%
  adorn_percentages() %>%
  adorn_pct_formatting() %>%
  # To show original counts
  adorn_ns()
  

# (1). Compute the point estimate
mythbusters_yawn %>%
  specify(formula = yawn ~ group)  

# Note that the success argument must be specified in situations such as this 
# where the response variable has only two levels. 
mythbusters_yawn %>%
  specify(formula = yawn ~ group, success = "yes")

# We next want to calculate the statistic of interest for our sample. This corresponds to the difference in the
# proportion of successes.
mythbusters_yawn %>%
  specify(formula = yawn ~ group, success = "yes") %>%
  calculate(stat = "diff in props")

# To further check to make sure that R knows exactly what we are after, we need
# to provide the order in which R should subtract these proportions of successes.

obs_diff <- mythbusters_yawn %>%
  specify(formula = yawn ~ group, success = "yes") %>%
  calculate(stat = "diff in props", order = c("seed", "control"))
obs_diff

# (2). Bootstrap distribution

head(mythbusters_yawn)

# The sample_n function can perform this bootstrapping procedure and is similar
# to the rep_sample_n function in infer, except that it is not repeated but rather only performs one sample
# with or without replacement.
head(mythbusters_yawn) %>%
  sample_n(size = 6, replace = TRUE)

# We can see that in this bootstrap sample generated from the first six rows of mythbusters_yawn, we have
# some rows repeated. The same is true when we perform the generate step in infer:
bootstrap_distribution <- mythbusters_yawn %>%
  specify(formula = yawn ~ group, success = "yes") %>%
  generate(reps = 1000) %>%
  calculate(stat = "diff in props", order = c("seed", "control"))

bootstrap_distribution %>%
  visualize(bins = 20)

# Let’s use the percentile-based
# method to compute a 95% confidence interval for the true difference in the proportion of those that yawn
# with and without a seed presented
bootstrap_distribution %>%
  get_ci(type = "percentile", level = 0.95)







