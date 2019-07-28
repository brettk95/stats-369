library('tidyverse')
library(refinr)
bike_files = list.files(path = "." ,pattern=".csv",full=TRUE)
bike_data = map(bike_files,read_csv)
notallNA<-function(x) !all(is.na(x))
bike_data <- map(bike_data, select_if, notallNA)
bike_tidy<-map(bike_data, gather, value="rider_count", key="counter", -Date)
allbikes<-bind_rows(bike_tidy)
allbikes %>% group_by(counter) %>% count() 

### Load the Data
### actual Code

allbikes$counter[allbikes$counter == "Curran Street Total"] = "Curran St total"
allbikes$counter[allbikes$counter == "Curran Street Total Cyclists"] = "Curran St total"

allbikes %>% group_by(counter) %>% summarise(totals = sum(rider_count)) %>% arrange(desc(totals))
view(allbikes %>% group_by(counter) %>% summarise(na = all(is.na(rider_count))))
