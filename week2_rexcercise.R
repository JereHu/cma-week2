#Task 1
library("readr") 
library("sf") 
library("ggplot2")
library("tmap")

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

# Task 4
# Cross-scale movement analysis

caro <- read_delim("datasets/caro60.csv")
caro <- st_as_sf(caro, coords = c("E", "N"), crs = 2056, remove = FALSE)

caro_3 <- caro[seq(from = 1, to = nrow(caro), by=3),]
caro_6 <- caro[seq(from = 1, to = nrow(caro), by=6),]
caro_9 <- caro[seq(from = 1, to = nrow(caro), by=9),]
# nrow(caro)
# nrow(caro_3)
# nrow(caro_6)
# nrow(caro_9)

#caro timelag, steplentgh, speed
caro <- caro |>
  mutate(diff_s = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")))
caro <- caro |>
  mutate(steplength = sqrt((E - lead(E))^2 + (N - lead(N))^2))
caro <- caro %>%
  mutate(speed_ms = steplength/diff_s)

#caro3 timelag, steplentgh, speed
caro_3 <- caro_3 |>
  mutate(diff_s = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")))
caro_3 <- caro_3 |>
  mutate(steplength = sqrt((E - lead(E))^2 + (N - lead(N))^2))
caro_3 <- caro_3 %>%
  mutate(speed_ms = steplength/diff_s)

#caro_6 timelag, steplentgh, speed
caro_6 <- caro_6 |>
  mutate(diff_s = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")))
caro_6 <- caro_6 |>
  mutate(steplength = sqrt((E - lead(E))^2 + (N - lead(N))^2))
caro_6 <- caro_6 %>%
  mutate(speed_ms = steplength/diff_s)

#caro_9 timelag, steplentgh, speed
caro_9 <- caro_9 |>
  mutate(diff_s = as.numeric(difftime(lead(DatetimeUTC), DatetimeUTC, units = "secs")))
caro_9 <- caro_9 |>
  mutate(steplength = sqrt((E - lead(E))^2 + (N - lead(N))^2))
caro_9 <- caro_9 %>%
  mutate(speed_ms = steplength/diff_s)

# lineplots
colors <- c("1 minute" = "red", "3 minutes" = "green", "6 minutes" = "lightblue", "9 minutes" = "violet")

ggplot(data = caro) +
  geom_line(data=caro_3, aes(x=DatetimeUTC, y=speed_ms, color = "3 minutes"))+
  geom_line(data = caro_6, aes(x=DatetimeUTC, y=speed_ms, color="6 minutes"))+
  geom_line(data = caro_9, aes(x=DatetimeUTC, y=speed_ms, color = "9 minutes"))+
  geom_line(data = caro, aes(x=DatetimeUTC, y=speed_ms, color = "1 minute"))+
  labs(x="Time", y="Speed (m/s)") +
  scale_color_manual(values = colors)+
  ggtitle("Comparing derived speed at different sampling intervals")
# interpretation: The higher the time granularity, the higher peaks in speed we get.
# Since we get less datapoints, the speed smoothes out (as pig stays within a pen and has high chance of returning to a spot close to the one before)


# mapping caro 3
ggplot(data = caro, aes(x=E, y=N))+
  geom_sf(aes(color = "1 minute"),  alpha = 0.3)+
  geom_path(aes(color = "1 minute"),  alpha = 0.3)+
  geom_sf(caro_3, mapping = aes(color = "3 minutes"), alpha = 1)+
  geom_path(caro_3, mapping = aes(color = "3 minutes"), alpha = 1)+
  labs(color = "Trajectory")+
  ggtitle("Comparing original- with 3 minutes-resampled data")+
  coord_sf(datum = 2056)

# mapping caro 6
ggplot(data = caro, aes(x=E, y=N))+
  geom_sf(aes(color = "1 minute"),  alpha = 0.3)+
  geom_path(aes(color = "1 minute"),  alpha = 0.3)+
  geom_sf(caro_6, mapping = aes(color = "6 minutes"), alpha = 1)+
  geom_path(caro_6, mapping = aes(color = "6 minutes"), alpha = 1)+
  ggtitle("Comparing original- with 6 minutes-resampled data")+
  coord_sf(datum = 2056)

# mapping caro 9
ggplot(data = caro, aes(x=E, y=N))+
  geom_sf(aes(color = "1 minute"),  alpha = 0.3)+
  geom_path(aes(color = "1 minute"),  alpha = 0.3)+
  geom_sf(caro_9, mapping = aes(color = "6 minutes"), alpha = 1)+
  geom_path(caro_9, mapping = aes(color = "6 minutes"), alpha = 1)+
  ggtitle("Comparing original- with 9 minutes-resampled data")+
  coord_sf(datum = 2056)

# Task 7
posmo <- read_delim("datasets/posmo_2023-04-10T00_00_00+02_00-2023-04-28T23_59_59+02_00.csv")
posmo_sf <- st_as_sf(posmo, coords = c("lon_x", "lat_y"), crs = 4326, remove = FALSE)

st_crs(posmo_sf)
posmo_sf <- st_transform(posmo_sf, crs = 2056)
st_crs(posmo_sf)

posmo_sf <- posmo_sf |> 
  group_by("weekday")

ggplot(data = posmo_sf)+
  geom_sf(aes(colour = weekday))
