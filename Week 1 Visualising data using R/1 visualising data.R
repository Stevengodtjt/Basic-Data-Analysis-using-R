install.packages("ggplot2")
install.packages("nycflights13")  #contains data on flights from New York City in 2013 that we shall be examining

library(ggplot2)
library(nycflights13)

#1.Viewing the data
head(flights,n=3)

dim(flights)  #336776行,19列

# To reduce the amount of data we will be working with and make things a little easier, let’s only look at
# Alaska Airlines flights leaving from New York City in 2013

Alaska<-flights[flights$carrier=="AS",]
head(Alaska)

#2.Scatterplots散点图

#aim:observe the relationship between departure and arrival delays.
ggplot(data=Alaska,mapping=aes(x=dep_delay,y=arr_delay)) #set up plotting region
ggplot(data=Alaska,mapping=aes(x=dep_delay,y=arr_delay))+geom_point() #include the points

ggplot(data=Alaska,mapping=aes(x=dep_delay,y=arr_delay))+
  geom_point()+
  labs(x="Depature delay(minutes)",y="Arrival delay(minutes)",title="Alaska Airlines flights leaving NYC in 2013")
      
#change the axes labels


#2.1：overplotting

#method1: 
ggplot(data=Alaska,mapping=aes(x=dep_delay,y=arr_delay))+
  geom_point(alpha=0.2)+
  labs(x="Depature delay(minutes)",y="Arrival delay(minutes)",title="Alaska Airlines flights leaving NYC in 2013")

#method2:

#eg:
jitter.example<-matrix(0,nrow=10,ncol=2)
jitter.example

jitter.example<-as.data.frame(jitter.example)
str(jitter.example)

ggplot(data = jitter.example, mapping = aes(x = V1, y = V2)) +  #plot the example
  geom_point()

# If we shift each of the points slightly using jittering
# we will be able to see them more clearly:
ggplot(data = jitter.example, mapping = aes(x = V1, y = V2)) +
  geom_jitter(width = 0.1, height = 0.1)


#jittered scatterplot of Alaska
ggplot(data=Alaska,mapping=aes(x=dep_delay,y=arr_delay))+
  geom_jitter(width=30,height=30)+
  labs(x="Depature delay(minutes)",y="Arrival delay(minutes)",title="Alaska Airlines flights leaving NYC in 2013")


#3.Histograms直方图

# let’s take a look at the weather data set that is within the nycflights13 library. This data set
# contains hourly weather data from three airports (LGA, JFK and EWR) in New York City in 2013.

head(weather,n=3)  #EWR:纽瓦克机场

# To create a histogram using ggplot we use the geom_histogram command
ggplot(data=weather,mapping=aes(x=temp))+
  geom_histogram()

#30 bins to create histogram,可自行调节箱子个数和颜色
ggplot(data=weather,mapping=aes(x=temp))+
  geom_histogram(bins=60,color="white") #color->outline

#除此之外，还可以调整bins的宽度
ggplot(data = weather, mapping = aes(x = temp)) +
  geom_histogram(binwidth = 5, color = "white")

#类似scatterplot，可以加标题和坐标轴名字
ggplot(data = weather, mapping = aes(x = temp)) +
  geom_histogram(binwidth = 5, color = "white")+
  labs(x="Temperature(hourly)",y="count",title="Hourly temperatures from NYC in 2013")

#4.Boxplots箱线图（another way to look at the distribution)

# • the minimum value;最小值
# • the first quartile or 25th percentile;下四分位数
# • the median (or 2nd quartile / 50th percentile);中位数
# • the third quartile or 75th percentile;上四分位数
# • and the maximum value.最大值

#keeping with the weather data
summary(weather$temp)  # 1 missing value(NA)


# we could look at how the hourly temperature changes by month,
# where month is our categorical, or grouping, variable

weather$month
factor(weather$month)

#To create boxplots using ggplot we use the geom_boxplot function.
ggplot(data = weather, mapping = aes(x = factor(month), y = temp)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "Month", y = "Temperature (Hourly)",
       title = "Hourly temperatures from NYC in 2013 by month") +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))


#5.Barplots 条形图

# 1 dimension(flights numbers vs airline carrier)

# Let’s take a look at the distribution of 
# airline carriers（航空公司） within the flights data that flew out of New York City in 2013.
carrier.freq <- table(flights$carrier)
str(carrier.freq) #转换
carrier.freq <- as.data.frame(carrier.freq)
colnames(carrier.freq) <- c("carrier", "number")
carrier.freq

#To create barplots using ggplot we use the geom_col function:
ggplot(data = carrier.freq, mapping = aes(x = carrier, y = number)) +
  geom_col()

# 2 dimensions(flights numbers vs airline carrier& airport)
carrier.origin <- table(flights$origin, flights$carrier)
carrier.origin <- as.data.frame(carrier.origin)
colnames(carrier.origin) <- c("origin", "carrier", "number")
carrier.origin 

# pass the additional fill argument to the aes function
# Including the fill argument lets ggplot plot know that we
# want to split the barplot according to an additional categorical variable, which is origin in this case.

ggplot(data = carrier.origin, mapping = aes(x = carrier, y = number, fill = origin)) +
  geom_col() +
  labs(x = "Carrier", y = "Count",
       title = "Carriers who flew out of New York City in 2013")  #stacked barplot堆积

ggplot(data = carrier.origin, mapping = aes(x = carrier, y = number, fill = origin)) +
  geom_col(position = "dodge") +
  labs(x = "Carrier", y = "Count",
       title = "Carriers who flew out of New York City in 2013")  #side-by-side (or dodged) barplot 并行


ggplot(data = carrier.origin, mapping = aes(x = carrier, y = number, fill = origin)) +
  geom_col() +
  facet_wrap(~ origin, ncol = 1) +  #根据origin分类，画在同一列
  labs(x = "Carrier", y = "Count",
       title = "Carriers who flew out of New York City in 2013") #faceted barplot 小平面图


#6.Linegraphs线状图(time series data)----how variables change via time

# Let’s again look at the hourly temperature data,
# but this time only for Newark International Airport in January.
Newark.Jan <- weather[weather$origin == "EWR" & weather$month == 1, ]
Newark.Jan

# To produce linegraphs using ggplot we use the geom_line function
ggplot(data = Newark.Jan, mapping = aes(x = time_hour, y = temp)) +
  geom_line() +
  labs(x = "Time (Hours)", y = "Temperature",
       title = "Hourly Temperature at Newark Airport in January 2013")







