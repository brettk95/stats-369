library("readr")
library("tidyr")
library("dplyr")

# load the data
household_df = read.csv("C:\\Users\\Brett\\Documents\\github\\compsci_369\\household-data-for-gathering.csv", row.names = 'X')

# do some data manipulation to get rid of rows with all 0's
household_df = household_df %>% filter(!Measure %in% c('Median Self-employment Income','Median Government Transfer Income'))


## use gather() function on data to make it skinny

# the columsn that we want are:
colnames(household_df)[4:24]

household_df_gathered = household_df %>% gather(colnames(household_df)[4:24], key = "Year", value = "Value")

head(household_df_gathered)