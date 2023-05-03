#line to test gitignore

#excercise 2, esseitially the same as tollskit_demo


now <- Sys.time()

later <-Sys.time()

time_difference <- as.numeric(difftime(now, later))

time_difference



numbers <- 1:10
numbers

library("dplyr")

lead(numbers)

lag(numbers, n= 5)


wildschwein <- tibble(
  TierID = c(rep("Hans", 5), rep("Klara", 5)),
  DatetimeUTC = rep(as.POSIXct("2015-01-01 00:00:00", tz = "UTC") + 0:4 * 15 * 60, 2)
)

wildschwein

wildschwein <- group_by(wildschwein, TierID)

# This is the 'short version' where wildschwein does not have to be mentionead all the time (see other file)
mutate(wildschwein, diff = as.numeric(difftime(lead(DatetimeUTC, DatetimeUTC))))


