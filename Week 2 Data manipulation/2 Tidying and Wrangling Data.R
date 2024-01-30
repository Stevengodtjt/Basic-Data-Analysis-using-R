#PART 1:preparation

library(dplyr)  #provides functions for data wrangling or manipulation using a consistent ‘grammar’.
library(tidyr)  #helps us create tidy data, which we will now introduce
library(ggplot2)
library(readr)
library(stringr)

install.packages("nycflights13")
library(nycflights13)
install.packages("fivethirtyeight")
library(fivethirtyeight)   #The final two libraries contain interesting data sets that we shall examine

#PART2: What is tidy data

# Beyond just being organised, having tidy data means that
# your data follows a standardised format

#In tidy data
# 1. Each variable forms a column.
# 2. Each observation forms a row.
# 3. Each type of observational unit forms a table.


# Task1:
# Consider the following data frame of average number of servings of beer, spirits, and wine consumption
# in three countries as reported in the FiveThirtyEight article Dear Mona Followup: Where Do People Drink
# The Most Beer, Wine And Spirits?



#PART3: Observational units
dim(flights)

head(flights)

glimpse(flights)  # Lists the variables in an object with their first few values

# The organisation of this data follows the third tidy data property: observations corresponding to the same
# observational unit are saved in the same data frame.

# Task2: 
# For each of the data sets listed above (other than flights), identify the observational unit and how
# many of these are described in each of the data sets.




#PART4:Identification vs measurement variables

glimpse(airports)
#(1) The variables faa and name are what we will call identification variables:

#(2) The remaining variables (lat, lon, alt, tz, dst, tzone) are often called measurement or
# characteristic variables:

# Task3: 
#   What properties of the observational unit do each of lat, lon, alt, tz, dst, and tzone describe for
# the airports data frame?

# Task4: 
# From the data sets listed above, find an example where combinations of variables are needed to
# uniquely identify each observational unit.



#PART5: Importing spreadsheets into R

#5.1: Method 1——from the console
dem_score<-read_csv("https://moderndive.com/data/dem_score.csv")
dem_score
dem_score[dem_score$country=="China",]

#5.2: Method 2——Using Rstudio's interface

# Files -> Import Dataset -> From Excel...

# Task5: 
# Read in the life expectancy data stored at https://moderndive.com/data/le_mess.csv, either using
# the R console or RStudio’s interface.



#PART6: Converting into tidy data format

guat_dem <- dem_score %>%
  filter(country == "Guatemala")
guat_dem 



guat_tidy <- gather(data = guat_dem,
                    key = year,
                    value = democracy_score,
                    - country)
guat_tidy   #ppt

# We can now create a plot showing how democracy score in Guatemala has changed from 1952 to 1992 using
# a linegraph and ggplot2.
ggplot(data = guat_tidy, mapping = aes(x = year, y = democracy_score)) +
  geom_line() +
  labs(x = "year")

str(guat_tidy)

# year variable in guat_tidy is stored as a character vector,
#ggplot not knowing exactly how to plot a line using a categorical variable
#fix it by using parse_number(readr package)

ggplot(data = guat_tidy, mapping = aes(x = parse_number(year), y = democracy_score)) +
  geom_line() +
  labs(x = "year", y = "Democracy score",
       title = "Guatemala's democracy score ratings from 1952 to 1992")

# Task6: 
# Convert the dem_score data frame into a tidy data frame and assign the name of dem_score_tidy
# to the resulting long-formatted data frame.

# Task 7: 
# Convert the life expectancy data set you created in a previous task into a tidy data frame.
 


#PART7: Introduction to data wrangling

# we will use tools from the dplyr package to perform data wrangling which
# includes transforming, mapping and summarising variables.


#7.1 The pipe %>%

# the pipe operator allows us to chain together dplyr data wrangling functions
 

#7.2 Data wrangling verbs

# All of the verbs are used similarly where you: take a data frame, pipe it using the %>% syntax into one of
# the verbs above followed by other arguments specifying which criteria you would like the verb to work with
# in parentheses.


#PART8: Filter obesrvations using filter

# The filter function allows you to specify criteria about values of a variable in your data set and then
# chooses only those rows that match that criteria.

#(1)
portland_flights <- flights %>% 
  filter(dest == "PDX")      # focusing only on flights from New York City
                             # to Portland, Oregon. The dest code (or airport code) for Portland, Oregon is PDX

portland_flights[,-(6:12)]   # We do not display columns 6-12 so we can see the destination (dest) variable.

#(2)
btv_sea_flights_fall <- flights %>%
  filter(origin == "JFK", (dest == "BTV" | dest == "SEA"), month >= 10)   #To see many of these in action, let’s select all flights that left JFK airport heading to Burlington, Vermont
                                                                          # (BTV) or Seattle, Washington (SEA) in the months of October, November, or December
btv_sea_flights_fall[,-(6:12)]

#(3)
not_BTV_SEA <- flights %>%
  filter(!(dest == "BTV" | dest == "SEA"))  # Another example uses ! to pick rows that do not match a condition. The ! can be read as not. Here, we
                                            # are selecting rows corresponding to flights that did not go to Burlington, VT or Seattle, WA
not_BTV_SEA[,-(6:12)]






# Task8: 
# What is another way of using the not operator ! to filter only the rows that are not going to
# Burlington, VT nor Seattle, WA in the flights data frame?


#PART9: Summarize variables using summarize
head(weather)
summary_temp <- weather %>%
  summarize(mean = mean(temp), std_dev = sd(temp))  #NA
summary_temp

#setting the na.rm argument to TRUE (rm is short for remove).
#This will remove any NA missing values and only return the summary value for all non-missing values.
summary_temp <- weather %>%
  summarize(mean = mean(temp, na.rm = TRUE), std_dev = sd(temp, na.rm = TRUE)) 

# Task9: 
# Say a doctor is studying the effect of smoking on lung cancer for a large number of patients who have
# records measured at five year intervals. She notices that a large number of patients have missing data points
# because the patient has died, so she chooses to ignore these patients in her analysis. What is wrong with
# this doctor’s approach?

# Task10: 
# Modify summary_temp from above to also use the n summary function: summarize(count = n()).
# What does the returned value correspond to?

# Task11: 
# Why does the code below not work? Run the code line by line instead of all at once, and then look
# at the data. In other words, run summary_temp <- weather %>% summarize(mean = mean(temp, na.rm= TRUE)) first.

summary_temp <- weather %>%
  summarize(mean = mean(temp, na.rm = TRUE)) %>%
  summarize(std_dev = sd(temp, na.rm = TRUE))


#PART10: Group rows using group_by

#eg:we are interested in the mean and standard deviation of temperatures but grouped by month
#To be more specific: we want the mean and standard deviation of temperatures

head(weather)

summary_monthly_temp <- weather %>%
  group_by(month) %>%
  summarize(mean = mean(temp, na.rm = TRUE),
            std_dev = sd(temp, na.rm = TRUE))

summary_monthly_temp  # each row in summary_monthly_temp represents a summary of different rows in weather,
                      # the observational units have changed.


summary_monthly_temp <- weather %>%
  group_by(month) %>%
  ungroup() %>%      # ungroup: have all summarisations be for all data in a single group
  summarize(mean = mean(temp, na.rm = TRUE),
            std_dev = sd(temp, na.rm = TRUE))
summary_monthly_temp



by_origin <- flights %>%   #suppose we would like to get a sense for how many flights departed 
                           # each of the three airports in New York City
  group_by(origin) %>%
  summarize(count = n())

by_origin

#10.1: Grouping by more than one variable

by_origin_monthly <- flights %>%   #wanted to know the number of flights leaving
                                   #each of the three New York City airports for each month
  group_by(origin, month) %>%
  summarize(count = n())

by_origin_monthly 


#Q1: what if we reverse the order of the grouping
by_monthly_origin <- flights %>%
  group_by(month, origin) %>%
  summarize(count = n())
by_monthly_origin  # the values are actually the same, just presented in a different order.

#Q2:why do we group_by(origin, month) and not group_by(origin) and then group_by(month)?
by_origin_monthly_incorrect <- flights %>%
  group_by(origin) %>%
  group_by(month) %>%
  summarize(count = n())

by_origin_monthly_incorrect  # What happened here is that the second group_by(month) overrode the first group_by(origin), so that in
                             # the end we are only grouping by month.

by_origin_monthly_comparsion <- flights %>%
  group_by(month) %>%
  summarize(count = n())
by_origin_monthly_comparsion

# Task12: 
# Recall from Week 1 when we looked at plots of temperatures by months in NYC. What does the
# standard deviation column in the summary_monthly_temp data frame 
# tell us about temperatures in New York City throughout the year?
  
# Task13: Write code to produce the mean and standard deviation temperature for each day in 2013 for NYC?
  
# Task14: 
# Recreate by_monthly_origin, but instead of grouping via group_by(origin, month), 
# group variables in a different order group_by(month, origin). What differs in the resulting data set?
  
# Task15: How could we identify how many flights left each of the three airports for each carrier?
  
# Task16: How does the filter operation differ from a group_by followed by a summarize?


#PART 11: Create new variables/change old variables using mutate

head(flights)

flights <- flights %>%   
  mutate(gain = dep_delay - arr_delay)
# Passengers are often frustrated when their flights depart late,
# but change their mood a bit if pilots can make up some time during the flight to get them to
# their destination close to when they expected to land. This is commonly referred to as “gain”
flights[,"gain"]

# Let’s look at summary measures of this gain variable and plot it in the form of a histogram:
gain_summary <- flights %>%
  summarize(
    min = min(gain, na.rm = TRUE),
    q1 = quantile(gain, 0.25, na.rm = TRUE),
    median = quantile(gain, 0.5, na.rm = TRUE),
    q3 = quantile(gain, 0.75, na.rm = TRUE),
    max = max(gain, na.rm = TRUE),
    mean = mean(gain, na.rm = TRUE),
    sd = sd(gain, na.rm = TRUE),
    missing = sum(is.na(gain)))
    
gain_summary

ggplot(data = flights, mapping = aes(x = gain)) +
  geom_histogram(color = "white", bins = 20)

#We can also create multiple columns at once and even refer to columns that were just created in a new column.
flights <- flights %>%
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours
  )

# Task17: 
# What do positive values of the gain variable in flights correspond to? What about negative values?
# And what about a zero value?

# Task18: 
# Could we create the dep_delay and arr_delay columns by simply subtracting dep_time from
# sched_dep_time and similarly for arrivals? Try the code out and explain any differences between the
# result and what actually appears in flights.

# Task19: 
# What can we say about the distribution of gain? Describe it in a few sentences using the plot and
# the gain_summary data frame values.


#PART12: Reorder the data frame using arrange

# Let’s suppose we were interested in determining the most frequent destination airports 
# from New York City in 2013:
head(flights)

freq_dest <- flights %>%
  group_by(dest) %>%
  summarize(num_flights = n())
freq_dest  #by default the values of dest are displayed in alphabetical order here.


freq_dest %>%
  arrange(num_flights)  #We are interested in finding those airports that appear most:

freq_dest %>%          #descending(降序)
  arrange(desc(num_flights))


#PART 13: Joining data frames

airlines
str(airlines)
glimpse(flights)

#13.1: Joing by "key" variables(airlines vs flights)
flights_joined <- flights %>%
  inner_join(airlines, by = "carrier")

flights
flights_joined 
#We observe that the flights and flights_joined are identical 
#except that flights_joined has an additional variable name whose values were drawn from airlines.

#13.2: Joining by "key" variables with different names

# eg: interested in all the destinations of flights from NYC in 2013
# • “What cities are these airports in?”
# • “Is ORD Orlando?”
# • “Where is FLL?”

airports

# • airports the airport code is in the variable faa
# • flights the airport code is in the variable origin
flights %>%
  inner_join(airports, by = c("dest" = "faa"))  #看图

# Let’s construct the sequence of commands that computes the number of flights from NYC to each destination,
# but also includes information about each destination airport:
named_dests <- flights %>%
  group_by(dest) %>%
  summarize(num_flights = n()) %>%
  arrange(desc(num_flights)) %>%
  inner_join(airports, by = c("dest" = "faa")) %>%
  rename(airport_name = name)

named_dests


#13.3: Joining by multiple “key” variables

# eg:weather vs flights(year, month, day, hour, and origin)
str(weather)
str(flights)

flights_weather_joined <- flights %>%
  inner_join(weather, by = c("year", "month", "day", "hour", "origin"))
flights_weather_joined

# Task20: 
# Looking at the first figure in this section, when joining flights and weather (or, in other words,
# matching the hourly weather values with each flight), why do we need to join by all of year, month, day,
# hour, and origin, and not just hour?

#PART14: Other verbs

glimpse(flights)

# only want to consider two of these variables, say carrier and flight.
flights %>%
  select(carrier, flight)

# remove the year variable from our data set since it won’t be helpful for analysis in this case.(all 2013)
flights_no_year <- flights %>%
  select(-year)
flights_no_year

#Or we could specify a ranges of columns:
flight_arr_times <- flights %>%
  select(month:dep_time, arr_time:sched_arr_time)
flight_arr_times

# Let’s suppose we would like the hour, minute, and time_hour variables, which appear at the end of the
# flights data set, to actually appear immediately after the day variable:
flights_reorder <- flights %>%
  select(month:day, hour:time_hour, everything())
flights_reorder

# Lastly, the helper functions starts_with,ends_with, 
# and contains can be used to choose variables / column names that match those conditions:
flights_begin_a <- flights %>%
  select(starts_with("a"))  #a开头
flights_begin_a

flights_delays <- flights %>%
  select(ends_with("delay")) #delay结束
flights_delays

flights_time <- flights %>%
  select(contains("time"))   #包含time
flights_time 


#14.2： Rename variables using rename

flights_time <- flights %>%
  select(contains("time")) %>%
  rename(departure_time = dep_time,  #departure_time替换dep_time,arrival_time替换arr_time
         arrival_time = arr_time)

flights_time 

#14.3: Find the top number of values using top_n

# use the top_n function which automatically tells us the most frequent num_flights
named_dests
named_dests%>%
  top_n(n=10,wt=num_flights)  #ranking 前10的flights

#We can go one step further and tie together the group_by and summarize functions
#we used to find the most frequent flights:
ten_freq_dests <- flights %>%
  group_by(dest) %>%
  summarize(num_flights = n()) %>%
  arrange(desc(num_flights))%>%
  top_n(n = 10) 
ten_freq_dests 

# Task21: 
# What are some ways to select all three of the dest, air_time, and distance variables from flights?
# Give the code showing how to do this in at least three different ways.

# Task22: 
# How could one use starts_with, ends_with, and contains to select columns from the flights data
# frame? Provide three different examples in total: one for starts_with, one for ends_with, and one for
# contains.

# Task23: 
# Create a new data frame that shows the top 5 airports with the largest average arrival delays from
# NYC in 2013.



#TASK ONE:
# An airline industry measure of a passenger airline’s capacity is the available seat miles, which is equal to
# the number of seats available multiplied by the number of miles or kilometers flown. So for example say an
# airline had 2 flights using a plane with 10 seats that flew 500 miles and 3 flights using a plane with 20 seats
# that flew 1000 miles, the available seat miles would be 2 × 10 × 500 + 3 × 20 × 1000 = 70,000 seat miles.
# Using the data sets included in the nycflights13 package, compute the available seat miles for each airline
# sorted in descending order. After completing all the necessary data wrangling steps, the resulting data frame
# should have 16 rows (one for each airline) and 2 columns (airline name and available seat miles).

# Step 1: To compute the available seat miles for a given flight, we need the distance variable from the
# flights data frame and the seats variable from the planes data frame, necessitating a join by the key
# variable tailnum. To keep the resulting data frame easy to view, we’ll select only these two variables and
# carrier.

# Step 2: Now for each flight we can compute the available seat miles ASM by multiplying the number of seats
# by the distance via a mutate.

# Step 3: Next we want to sum the ASM for each carrier. We achieve this by first grouping by carrier and
# then summarising using the sum function.

# Step 4: However, if it was the case that some carriers had certain flights with missing NA values, the resulting
# table above would also return NA’s (NB: this is not the case for this data). We can eliminate these by adding
# the na.rm = TRUE argument to sum, telling R that we want to remove the NA’s in the sum.

# Step 5: Finally, arrange the data in descending order of ASM.


#TASK TWO:
# In this task we will work with the data set analysed and reported in the 2016 article from FiveThirtyEight.com
# entitled Some People Are Too Superstitious To Have A Baby On Friday The 13th. The data set is called
# US_births_2000_2014 and is within the fivethirtyeight package.

# 1. Create an object called US_births_2013 which focuses only on data corresponding to 2013 births.

# 2. By only choosing birth data for the years 2010, 2011, 2012, and 2014 create a new data frame called
# US_births_small and check that this resulting data frame has 1461 rows. Note that there are many
# different ways to do this, but try and come up with three different ways using:
# • the “or” operator | 
# • the %in% operator
# • the “not” operator !
# or combinations of them.

# 3. Suppose we are interested in choosing rows for only weekdays (not Saturdays or Sundays) for
# day_of_week in year 2013. Write the code to do so and give the name US_births_weekdays_2013
# to the resulting data frame. Note that you may want to run US_births_2000_2014 %>%
# distinct(day_of_week) to identify the specific values of day_of_week.

# 4. Using what you covered in Week 1, produce an appropriate plot looking at the pattern of births on all
# weekdays in 2013 coloured by the particular day of the week.

# 5. The plot in the previous task has shown there are some outliers in the data for US births on weekdays
# in 2013. We can use the summarize function to get an idea for how these outliers may affect the
# shape of the births variable in US_births_weekdays_2013. Write some code to calculate the mean
# and median values for all weekday birth totals in 2013. Store this aggregated data in the data frame
# birth_summ. What do these values suggest about the effects of the outliers?

# 6. Instead of looking at the overall mean and median across all of 2013 weekdays, calculate the mean
# and median for each of the five different weekdays throughout 2013. Using the same names for the
# columns as in the birth_summ data frame in the previous exercise, create a new data frame called
# birth_day_summ.

# 7. Using the aggregated data in the birth_day_summ data frame, produce this barplot.



#TASK THREE
# In this task we will work with the data set analysed and reported in the 2014 article from FiveThirtyEight.com
# entitled 41 Percent Of Fliers Think You’re Rude If You Recline Your Seat. The data set is called flying
# and is within the fivethirtyeight package.

# 1. Write code to determine the proportion of respondents in the survey that responded with Very when
# asked if a passenger reclining their seat was rude. You should determine this proportion across the
# different levels of age and gender resulting in a data frame of size 8 x 3. Assign the name prop_very
# to this calculated proportion in this aggregated data frame.


# 2. Using the aggregated data you’ve created, produce two bar plots (one stacked, the other side-by-side)
# to show the differences between the sexes of the proportion of people who believe reclining your seat
# is ‘very’ rude, within each age group. Also, consider
# • What stands out to you as you review these proportions?
# • What gender and age-range pairings have the highest and lowest proportions thinking reclining
# airline seats is very rude in this survey?









