install.packages(c("sf","tidyverse", "gganimate", "osrm", "tmap","viridis", "remotes", "terra", "ggplot2"))

library(terra)
library(ggplot2)
library(sf)
library(osrm)
library(dplyr)
library(lubridate)
library(viridis)
library(gganimate)
library(tmap)



## Get bike origin and destination data
# https://bikeshare.metro.net/about/data/
data <- read.csv("data/data_in/metro-trips-2025-q2.csv")
data$start_timestamp <- as.POSIXct(data$start_time, format = "%m/%d/%Y %H:%M")
data$ymd <- format(data$start_timestamp, "%Y-%m-%d")
data$hour <- as.numeric(format(data$start_timestamp, "%H"))

# Filter for trips between 6am (hour >= 6) and 11pm (hour < 23)
filtered <- subset(data, 
                   hour >= 6 & hour < 23 & 
                     ymd == as.Date("2025-04-01") & 
                     !is.na(start_lon) & !is.na(start_lat) & 
                     !is.na(end_lon) & !is.na(end_lat))


str(filtered)


# Create sf for start points
start_points <- st_as_sf(
  filtered,
  coords = c("start_lon", "start_lat"),
  crs = 4326
)

# Create sf for end points
end_points <- st_as_sf(
  filtered,
  coords = c("end_lon", "end_lat"),
  crs = 4326
)

## osrm routing routes in LA
#(route <- osrmRoute(src = start_points[8, ], dst = end_points[8, ], osrm.profile = "bike"))

# Create a mapping of route_id to start_timestamp from filtered data
start_timestamp_map <- data.frame(
  route_id = seq_len(nrow(filtered)),
  start_timestamp = filtered$start_timestamp
)

routes_list <- lapply(seq_len(nrow(start_points)), function(i) {
  route <- osrmRoute(
    src = start_points[i, ],
    dst = end_points[i, ],
    osrm.profile = "bike"
  )
  route
})


# Combine all routes into a single data frame
routes_df <- bind_rows(routes_list)


# add a column for the route index
routes_df$route_id <- rep(seq_along(routes_list), sapply(routes_list, nrow))

# Join the start_timestamp to routes_df
routes_df <- routes_df %>%
  left_join(start_timestamp_map, by = "route_id")


plot(st_geometry(routes_df))



# 1a. Explode LINESTRINGs into vertices
# st_cast preserves attributes from routes_df, including route_id and start_timestamp
# points <- st_cast(routes_df, "POINT", warn = FALSE)

# 1b. sample routes every 5 metres, then cast to points
routes_sampled <- routes_df %>%
  st_segmentize(dfMaxLength = units::set_units(3, m)) %>%
  st_cast("POINT", warn = FALSE)


points <- routes_sampled


# 2. Add sequential 'time' column for each route ---
points_with_time <- points %>%
  group_by(route_id) %>%
  mutate(time = row_number()) %>%
  ungroup()

# 3. Add timestamp column (relative to actual start_timestamp from data)
# Adjust the multiplier to make animation slower
# minutes(time - 1) = 1 min per point (original speed)
# minutes((time - 1) * 2) = 2 min per point (2x slower)
# minutes((time - 1) * 5) = 5 min per point (5x slower)
points_with_time <- points_with_time %>%
  group_by(route_id) %>%
  mutate(
    time = row_number(),
    timestamp = start_timestamp + minutes(time - 1)
  ) %>%
  ungroup()


# extract coordinates
points_df <- points_with_time %>%
  st_coordinates() %>%
  as.data.frame() %>%
  bind_cols(points_with_time %>% st_drop_geometry())  

# Create animation with shadow_wake
animation <- ggplot(points_df) +
  
  # lines
  #geom_path(aes(x = X, y = Y, group = route_id),
            #color = "white", linewidth = 0.2, alpha = 0.1) +
  
  # points
  geom_point(aes(x = X, y = Y, group = route_id),
             color = "#93bcfb", fill = "#93bcfb", shape = 21, size = 1.2, alpha = 0.3) +
  
  # navy background
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#040512", color = NA),
    plot.background  = element_rect(fill = "#040512", color = NA),
    axis.ticks = element_blank(),
    axis.text  = element_blank(),
  ) +
  transition_time(timestamp) +
  shadow_wake(wake_length = 0.2) # Adjust wake_length to control trail

# Create animation
gganimate::animate(animation, 
                   nframes = 1000, 
                   height = 1920, 
                   width = 1920,
                   fps = 30, 
                   renderer = gifski_renderer(loop = TRUE))

# Save gif
gganimate::anim_save('data/data_out/routes_gg_animation_wake_blue.gif')


# Save mp4
gganimate::animate(animation, 
                   nframes = 1000, 
                   height = 1920, 
                   width = 1920,
                   fps = 30, 
                   renderer = av_renderer(file = 'data/data_out/routes_gg_animation_wake_blue.mp4'))