library('tidyverse')
library('refinr')
library('ggplot2')
bike_files = list.files(path = "." ,pattern=".csv",full=TRUE)
bike_data = map(bike_files,read_csv)
notallNA<-function(x) !all(is.na(x))
bike_data <- map(bike_data, select_if, notallNA)
bike_tidy<-map(bike_data, gather, value="rider_count", key="counter", -Date)
allbikes<-bind_rows(bike_tidy)
allbikes = allbikes %>% filter(!is.na(Date))

# bike data - work with x

x = allbikes %>% separate(Date, c('Day','Dateth','Month','Year'))
x$Month = match(x$Month, month.abb)
x$Month = sprintf("%02d",x$Month)
x$Dateth = as.integer(x$Dateth)
x$Dateth = sprintf("%02d",x$Dateth)
x = x %>% mutate(Date = paste(Year,Month,Dateth))
x$Date = gsub(" ", "", x$Date, fixed = TRUE)
daily_bike = x %>% group_by(Date) %>% summarise(total_daily_riders = sum(rider_count, na.rm = TRUE))
daily_bike$Date = as.integer(daily_bike$Date)

# Read rain data 

rain_files = list.files(path = '.', pattern = '.txt', full = TRUE)
rain_data = map(rain_files, read_csv)
rain_data = bind_rows(rain_data)[c(-5,-6)]
colnames(rain_data) = c("Station", "Date", "Time", "Amount")
daily_rain = rain_data %>% group_by(Date) %>% summarise(total_daily_rain = sum(Amount))
daily_rain = daily_rain[-1097,] # remove 1st Jan 2019

# column bind daily_bike and daily_rain
daily_both = inner_join(daily_bike, daily_rain, by = "Date")
daily_both$Date = as.character(daily_both$Date)
daily_both$Date = as.Date(daily_both$Date, "%Y%m%d")
daily_both = daily_both %>% mutate(day_of_week = weekdays(Date))
daily_both["week"] = floor_date(daily_both$Date, "week")

# plot over days over time (weekly data)
weekly_data = daily_both %>% group_by(week) %>% summarise(total_weekly_riders = mean(total_daily_riders),
                                                          total_weekly_rain = mean(total_daily_rain))
weekly_data = weekly_data[c(-1,-158),]

p = ggplot()+
  geom_line(data = weekly_data, aes(x = week, y = total_weekly_riders/1000), color = "black")+
  geom_line(data = weekly_data, aes(x = week, y = total_weekly_rain), color = "blue")

print(p)

# plot over days of the week
day_of_week_data = daily_both %>% group_by(day_of_week) %>% summarise(mean_riders_day = mean(total_daily_riders))
day_of_week_data$day_of_week = factor(day_of_week_data$day_of_week, levels = c("Monday","Tuesday","Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

ggplot(data = day_of_week_data, aes(x = day_of_week, y = mean_riders_day))+
  geom_bar(stat = "identity")
