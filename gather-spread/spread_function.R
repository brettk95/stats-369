library("tidyr")
library("readr")

household_df = read.csv("C:\\Users\\Brett\\Documents\\github\\compsci_369\\household-data-for-spreading.csv")

household_df = household_df[-6]

x = spread(household_df, key = "Year", value = "Value")

write.csv(x, file = "C:\\Users\\Brett\\Documents\\github\\compsci_369\\household-data-for-gathering.csv")