#Task 1
library("readr") 
library("sf") 
library("ggplot2")

wildschwein_BE <- read_delim("datasets/wildschwein_BE_2056.csv", ",")

# make spatial object
wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)

wildschwein_BE

wildschwein_BE <- mutate(wildschwein_BE, diff = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC), units = "secs"))
wildschwein_BE       

wildschwein_BE$TierName |> unique()

# Task 2

ggplot(wildschwein_BE, aes(DatetimeUTC, TierName)) +
  geom_point()
# lots of points, but look like a line as they overlap

wildschwein_BE <- wildschwein_BE |>
  group_by(TierName) |>
  mutate(diff_s = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC)))

hist(wildschwein_BE$diff_s)

ggplot(wildschwein_BE, aes(diff_s/60)) +
  geom_histogram(binwidth = 1) +
  lims(x = c(0, 5000/60)) +
  scale_y_log10()

wildschwein_BE |>
  filter(DatetimeUTC < "2014-08-24") |>
  ggplot(aes(DatetimeUTC, diff_s), colour = TierName) +
  geom_point() +
  geom_line()
# we do this, to find out when the pigs move and when we get the most datapoints


## Task 3
## Find links to geodetic distance converstion

N1 <- 1204752
N2 <- 1204863
E1 <- 2570409
E2 <- 2570402

wildschwein_BE <- wildschwein_BE |>
  group_by(TierName) %>%
  mutate(steplength = sqrt((E - lead(E))^2 + (N - lead(N))^2))

wildschwein_BE <- wildschwein_BE |>
  mutate(speed_ms = steplength/diff_s)


