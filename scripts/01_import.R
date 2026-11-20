# 01_import.R
# Reads the raw files from data/, cleans them, and saves usable CSVs.

library(tidyverse)
library(sf)
library(osrm)

# Starting with importing and cleaning SEPTA Trenton Line morning schedule
septa_trips      <- read_csv("data/septa_trips.txt", show_col_types = FALSE)
septa_stops      <- read_csv("data/septa_stops.txt", show_col_types = FALSE)
septa_stop_times <- read_csv("data/septa_stop_times.txt", show_col_types = FALSE)
septa_calendar   <- read_csv("data/septa_calendar.txt", show_col_types = FALSE)

# Keep Monday-Friday Trenton Line ("TRE") trips
weekday_days <- filter(septa_calendar, monday == 1)
tre_trips    <- filter(septa_trips, route_id %in% c("TRE", "TRENTON"), service_id %in% weekday_days$service_id)
tre_trips    <- select(tre_trips, trip_id)

# Match stop times with station names
stops_clean <- select(septa_stops, stop_id, stop_name)
tre_stops   <- inner_join(septa_stop_times, tre_trips, by = "trip_id")
tre_stops   <- left_join(tre_stops, stops_clean, by = "stop_id")

# Keep Trenton departures and split "HH:MM:SS" into hours and minutes
trenton_depart <- filter(tre_stops, str_detect(stop_name, "Trenton"))
trenton_depart <- separate(trenton_depart, departure_time, into = c("dep_hr", "dep_min", "dep_sec"), sep = ":")
trenton_depart <- mutate(trenton_depart, depart_mins = as.numeric(dep_hr) * 60 + as.numeric(dep_min))
trenton_depart <- select(trenton_depart, trip_id, depart_mins)

# Keep Philadelphia 30th Street arrivals and split "HH:MM:SS" into hours and minutes
philly_arrive <- filter(tre_stops, str_detect(stop_name, "30th St"))
philly_arrive <- separate(philly_arrive, arrival_time, into = c("arr_hr", "arr_min", "arr_sec"), sep = ":")
philly_arrive <- mutate(philly_arrive, arrive_mins = as.numeric(arr_hr) * 60 + as.numeric(arr_min))
philly_arrive <- select(philly_arrive, trip_id, arrive_mins)

# Combine departures and arrivals; keep morning trains (6:00 AM = 360 to 9:00 AM = 540)
clean_septa_trenton <- inner_join(trenton_depart, philly_arrive, by = "trip_id")
clean_septa_trenton <- mutate(clean_septa_trenton, septa_ride_min = arrive_mins - depart_mins)
clean_septa_trenton <- filter(clean_septa_trenton, septa_ride_min > 0, depart_mins >= 360, depart_mins <= 540)
clean_septa_trenton <- arrange(clean_septa_trenton, depart_mins)

write_csv(clean_septa_trenton, "data/clean_septa_trenton.csv")
typical_train_min <- median(clean_septa_trenton$septa_ride_min)

# Now I need to clean the census data
raw_census <- read_csv("data/census_mercer_commute.csv", show_col_types = FALSE)

# Keep the row for average commute time and the Estimate columns for each town
clean_census_commute <- rename(raw_census, variable = `Label (Grouping)`)
clean_census_commute <- filter(clean_census_commute, str_detect(variable, "Mean travel time to work"))
clean_census_commute <- select(clean_census_commute, contains("!!Total!!Estimate"))

# Build a standard two-column table from the town column names and row 1 numbers
clean_census_commute <- tibble(
  raw_town            = names(clean_census_commute),
  census_baseline_min = as.numeric(clean_census_commute[1, ])
)

# Split "Ewing township, Mercer County, New Jersey..." at the commas to keep just the town name
clean_census_commute <- separate(clean_census_commute, raw_town, into = c("town", "county", "extra"), sep = ", ")
clean_census_commute <- mutate(
  clean_census_commute,
  town = str_to_title(town),
  census_baseline_min = as.numeric(census_baseline_min)
)
clean_census_commute <- select(clean_census_commute, town, census_baseline_min)
clean_census_commute <- arrange(clean_census_commute, town)

write_csv(clean_census_commute, "data/clean_census_commute.csv")


# Load NJGIN and calculate driving times
raw_nj_towns    <- st_read("data/njgin_municipal_boundaries.geojson", quiet = TRUE)
mercer_towns_sf <- filter(raw_nj_towns, COUNTY == "MERCER")
mercer_towns_sf <- mutate(mercer_towns_sf, town = str_to_title(MUN_LABEL))
mercer_towns_sf <- mutate(mercer_towns_sf, town = if_else(town == "Trenton", "Trenton City", town))
mercer_towns_sf <- select(mercer_towns_sf, town, geometry)
mercer_towns_sf <- arrange(mercer_towns_sf, town)

# Find the center point of each town and set coordinates for both train stations
town_centers <- st_centroid(mercer_towns_sf)

destinations <- tibble(
  place = c("Trenton_Station", "Philly_30th_St"),
  lon   = c(-74.7542, -75.1823),
  lat   = c(40.2189, 39.9566)
)
destinations <- st_as_sf(destinations, coords = c("lon", "lat"), crs = 4326)

# Calculate driving minutes using OpenStreetMap and add them as columns with mutate()
drive_matrix <- osrmTable(src = town_centers, dst = destinations, measure = "duration")

clean_mercer_driving_times <- mutate(
  mercer_towns_sf,
  drive_to_trenton_min = round(drive_matrix$durations[, 1], 1),
  drive_to_philly_min  = round(drive_matrix$durations[, 2], 1)
)
clean_mercer_driving_times <- st_drop_geometry(clean_mercer_driving_times)

write_csv(clean_mercer_driving_times, "data/clean_mercer_driving_times.csv")


# I am going to merge these now into one data sheet now
clean_mercer_master <- left_join(clean_mercer_driving_times, clean_census_commute, by = "town")
clean_mercer_master <- mutate(
  clean_mercer_master,
  parking_buffer_min      = 10,
  septa_ride_min          = typical_train_min,
  total_train_commute_min = round(drive_to_trenton_min + parking_buffer_min + septa_ride_min, 1),
  mercer_median_train_min = round(median(total_train_commute_min), 1),
  faster_option           = if_else(total_train_commute_min < drive_to_philly_min,
                                    "Train is Faster", "Driving is Faster")
)

write_csv(clean_mercer_master, "data/clean_mercer_master.csv")
write_csv(clean_mercer_master, "app/data/clean_mercer_master.csv")





