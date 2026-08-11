# Find the distance from a sampling location to the 
# California coastline, find proportion of marine prey of each sample, and beta regression of distance to coast and host.
# 20 Aug 2025
# G Lewin
############################################

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(geosphere)
library(tidyverse)
library(here)



# Example code to find the distance from a sampling location to the 
# California coastline.
# 6 Aug 2025
############################################

### This is the example code from . I wanted to put in all my points for each sample instead of just one point...

#Sampling location. !!! Make sure the CRS is the same for your GPS records !!!
#my_point <- st_sfc(st_point(c(-120.458529, 34.514782)), crs = 4326)  # example: Jack Dangermond Headquarters
my_point <- st_sfc(st_point(c(-120.4561, 34.51859)), crs = 4326) 

#California coastline
coastline <- ne_download(scale = "large", type = "coastline", category = "physical", returnclass = "sf")



#Crop to West Coast/California region to reduce size coast line file
bbox_california <- st_bbox(c(xmin = -123, xmax = -118, ymin = 32, ymax = 43), crs = st_crs(4326))
coastline_ca <- st_crop(coastline, bbox_california)

#find the closest point on the coastline to my_point
nearest_coast_point <- st_nearest_points(my_point, coastline_ca) #will give multiple possible based on multiple lines that make up the coastline

# find the length of nearest_coast_point 
distance_meters <- min(st_length(nearest_coast_point))





# try to adapt for each sample point at Dangermond -- This is my attempt, but using min(st_length) at the end only returns one value... because it's getting the minimum of the whole column. So I checked what chatGPT had to offer.

# read in data
gav_raw <- read_csv(here("data", "grouped_prey_sites_host.csv"))

gav_dangermond <- gav_raw |> 
  filter(site == "Dangermond") |> 
  filter(lat != "NA")

#convert to sf object (creates geometry column)
gav_points <-  st_as_sf(
  gav_dangermond,
  coords = c("lon", "lat"),
  crs = 4326
)

#California coastline
coastline <- ne_download(scale = 10, type = "coastline", category = "physical", returnclass = "sf")

#california <- ne_states(country = "United States of America")

#Crop to West Coast/California region to reduce size coast line file
bbox_california <- st_bbox(c(xmin = -123, xmax = -118, ymin = 32, ymax = 43), crs = st_crs(4326))
coastline_ca <- st_crop(coastline, bbox_california)

# 3. Reproject to a projected CRS (meters)
gav_points_proj <- st_transform(gav_points, 3310)     # California Albers
coastline_proj  <- st_transform(coastline_ca, 3310)

# 4. For each point, find the nearest connecting line
nearest_coast_point_dangermond <- st_nearest_points(gav_points_proj, coastline_proj)

#find the closest point on the coastline to my_point
#nearest_coast_point_dangermond <- st_nearest_points(gav_points, coastline_ca) #will give multiple possible based on multple lines that make up the coastline

# find the length of nearest_coast_point 
distance_meters_dangermond <- st_length(nearest_coast_point_dangermond)

distance_meters_dangermond <- min(st_length(nearest_coast_point_dangermond))


# *******BELOW IS THE METHOD I DECIDED ON USING ******
# this uses st_nearest_feature and then applies st_nearest_points... 
# It gives a slightly (but similar) distance from 's code... Also is the transformation into CRS with meters necessary??


#------------------------------------------------------------
# All samples

# 1. Convert samples to sf points
gav <- gav_raw |> 
  filter(lat != "NA")

gav_points <- st_as_sf(
  gav,
  coords = c("lon", "lat"),
  crs = 4326
)



# 2. Get coastline and crop to California
coastline <- ne_download(scale = "large", type = "coastline", category = "physical", returnclass = "sf")
bbox_california <- st_bbox(c(xmin = -123, xmax = -118, ymin = 32, ymax = 43), crs = st_crs(4326))
coastline_ca <- st_crop(coastline, bbox_california)

# 3. Reproject both layers to a projected CRS in meters
gav_points_proj <- st_transform(gav_points, 3310)     # California Albers
coastline_proj  <- st_transform(coastline_ca, 3310)

#------------------------------------------------------------
# 4. For each sample (row), find the nearest coastline feature
nearest_index <- st_nearest_feature(gav_points_proj, coastline_proj)

#5. 
nearest_lines <- mapply(
  function(i, j) st_nearest_points(gav_points_proj[i, ], coastline_proj[j, ]),
  i = seq_len(nrow(gav_points_proj)),
  j = nearest_index,
  SIMPLIFY = FALSE
)

# Flatten the list of length-1 sfc objects into one sfc
nearest_lines <- do.call(c, nearest_lines)

# Now set CRS
st_crs(nearest_lines) <- st_crs(gav_points_proj)

nearest_lines <- st_sfc(nearest_lines, crs = st_crs(gav_points_proj))

# 6. Measure distance of each nearest line
distances_meters <- st_length(nearest_lines)

#------------------------------------------------------------
# 7. Add results back to dataframe
gav$dist_to_coast_m <- as.numeric(distances_meters)

# optional: keep nearest line geometries for plotting
gav_with_lines <- st_sf(
  gav,
  geometry = st_geometry(gav_points_proj)
) %>%
  mutate(nearest_line = nearest_lines)

#------------------------------------------------------------
# Final simple output (per sample_id)
gav_out <- gav %>%
  select(sample_id, host, diet_category, site, lon, lat, dist_to_coast_m)

#change cop and ncos labels to be the same 
gav <- gav |> 
  mutate(site = case_when(
    site == "Coal Oil Point" ~ "COP/NCOS",
    site == "North Campus Open Space" ~ "COP/NCOS",
    TRUE ~ site
  ))

# Final simple output (per sample_id)
gav_out <- gav %>%
  select(sample_id, host, diet_category, site, lon, lat, dist_to_coast_m)



gav_dangermond <- gav |> 
  filter(site == "Dangermond")

gav_cop_ncos <- gav_out |> 
  filter(site == "COP/NCOS")

all_mean_sd_dist <- gav_out |> 
  filter(site != "Hollister") |> 
  group_by(site, host) |> 
  summarise (mean = mean(dist_to_coast_m),
             sd = sd(dist_to_coast_m),
             n_samples = n(),
             min = min(dist_to_coast_m),
             max = max(dist_to_coast_m))

write_csv(all_mean_sd_dist, file = here("metabarcoding_scripts_GL", "outputs", "distance_coast_mean_sd_all_gav.csv"))


perc_less_than_100m <- gav_dangermond %>%
  summarise(perc = mean(dist_to_coast_m <= 100) * 100) %>%
  pull(perc)

perc_less_3km_dang <- gav_dangermond %>%
  summarise(perc = mean(dist_to_coast_m <= 3000) * 100) %>%
  pull(perc)

dang_mean_sd_dist <- gav_dangermond |> 
  group_by(host) |> 
  summarise (mean = mean(dist_to_coast_m),
             sd = sd(dist_to_coast_m))

cop_ncos_mean_sd_dist <- gav_cop_ncos |> 
  group_by(host) |> 
  summarise (mean = mean(dist_to_coast_m),
             sd = sd(dist_to_coast_m))
  



#check to see how the n_samples in the mean/sd is different than the orig
n_gav_raw <- gav_raw |> 
  filter(site != 'Hollister') |> 
  mutate(site = case_when(
    site == "Coal Oil Point" ~ "COP/NCOS",
    site == "North Campus Open Space" ~ "COP/NCOS",
    TRUE ~ site
  )) |> 
  group_by(site, host) |> 
  summarise(n = n())

write_csv(n_gav_raw, file = here("metabarcoding_scripts_GL","outputs","sample_counts_site_host.csv"))



######### find proportion of marine species in each sample. I've taken this from other code I've written, but applied it to simplified version of gav_dangermond dataframe (with character columns taken out.)

# Define marine species to group
# take out "green_algae", "sea_butterfly_snail",
marine_species <- c("american_shad", "brandts_cormorant", "california_sea_lion", 'pelagic_cormorant',  "guadelupe_fur_seal", "harbor_seal", "clam",  "sharpnose_anchovy")

# define terrestrial species
#take out "fly","frog","mold","fungi","flesh_fly","brown_dog_tick","rat"
# terrestrial_species <- c("deer_mouse", "anura", "bottas_pocket_gopher", "california_vole", "brush_rabbit", "american_robin", "woodrat", "desert_woodrat", "cattle","montane_vole", "wild_boar", "california_sea_lion", "western_harvest_mouse", "cricket", "dung_beetle", "chicken", "cow", "grey_fox",  "sciuridae", "golden_mantled_ground_squirrel", "yellow_bellied_marmot","corvidae", "song_sparrow", "wild_turkey", "california_ground_squirrel", "western_skink", "mule_deer","sparrow",  "cactus_mouse", "spotted_towhee", "southern_grasshopper_mouse", "european_earwig", "scarab_beetle","jerusalem_cricket", "california_pocket_mouse","southern_alligator_lizard", "gopher_snake",  "townsends_vole", "vole","common_carpet_beetle", "hairy_rove_beetle", "california_quail", "acorn_woodpecker", "downy_woodpecker", "big_eared_woodrat", "striped_skunk","pale_kangaroo_mouse", "gilberts_skink", "american_badger", "harvester_ant","california_scrub_jay", "red_fox",  "stonefly", "western_rattlesnake", "house_mouse","black_rat") 

# for species level only
terrestrial_species <- c("bottas_pocket_gopher", "california_vole", "brush_rabbit", "american_robin", "desert_woodrat", "cattle", "montane_vole", "wild_boar", "california_sea_lion", "western_harvest_mouse", "dung_beetle", "chicken", "grey_fox", "golden_mantled_ground_squirrel", "yellow_bellied_marmot", "song_sparrow", "wild_turkey", "california_ground_squirrel", "western_skink", "mule_deer", "cactus_mouse", "spotted_towhee", "southern_grasshopper_mouse", "european_earwig", "scarab_beetle","jerusalem_cricket", "california_pocket_mouse","southern_alligator_lizard", "gopher_snake", "townsends_vole","common_carpet_beetle", "hairy_rove_beetle", "california_quail", "acorn_woodpecker", "downy_woodpecker", "big_eared_woodrat", "striped_skunk","pale_kangaroo_mouse", "gilberts_skink", "american_badger", "harvester_ant","california_scrub_jay", "red_fox", "stonefly", "western_rattlesnake", "house_mouse","black_rat", "western_deer_mouse") 

#remove all columns that aren't prey species names
gav_dangermond_prop <- gav_dangermond |> 
  select(-c(host, marine_present, terrestrial_present, diet_category, site, lon, lat, dist_to_coast_m))

# 1. Identify which columns correspond to marine species
marine_cols <- intersect(names(gav_dangermond_prop), marine_species)

# 2. Identify which columns correspond to all species (marine + terrestrial)
all_species_cols <- setdiff(names(gav_dangermond_prop), c("sample_id"))

# 3. Compute proportion of marine species per sample
gav_dangermond_prop <- gav_dangermond_prop |> 
  rowwise() %>%
  mutate(
    total_species = sum(c_across(all_of(all_species_cols))),
    marine_species_count = sum(c_across(all_of(marine_cols))),
    prop_marine = ifelse(total_species > 0,
                         marine_species_count / total_species,
                         0)   # or 0, depending on what makes sense
  ) %>%
  ungroup()

# # 3. Compute proportion of marine species per sample
# gav_dangermond_prop <- gav_dangermond_prop |> 
#   rowwise() %>%
#   mutate(
#     total_species = sum(c_across(all_of(all_species_cols))),                 # total species detected
#     marine_species_count = sum(c_across(all_of(marine_cols))),              # marine species detected
#     prop_marine = marine_species_count / total_species                      # proportion marine
#   ) %>%
#   ungroup() |> 
#   filter(prop_marine != "NaN")





###### combine proportions and distance to coast

# Make sure gav_dangermond_out has sample_id and dist_to_coast_m
dist_df <- gav_dangermond %>%
  select(sample_id, host, dist_to_coast_m)

# Left join by sample_id
gav_dangermond_prop <- left_join(gav_dangermond_prop, dist_df, by = "sample_id")

# order from clostest to farthest from the coast
gav_dangermond_prop <- gav_dangermond_prop %>%
  arrange(dist_to_coast_m)


ggplot(gav_dangermond_prop, aes(x = dist_to_coast_m, y = prop_marine)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Distance to Coast (m)", y = "Proportion of Marine Species") +
  theme_minimal()

ggplot(gav_dangermond_prop, aes(x = dist_to_coast_m, y = prop_marine, color = host)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(x = "Distance to Coast (m)", y = "Proportion of Marine Species") +
  theme_minimal()






write_csv(gav_dangermond_prop, file = here("metabarcoding_scripts_GL", "outputs","gav_dangermond_dist_marine_prop.csv"))


################
# try to do the above with the total data set and then split it up by site later.

######## find proportion of marine species in each sample. I've taken this from other code I've written, but applied it to simplified version of gav_dangermond dataframe (with character columns taken out.)

# Define marine species to group

marine_species <- c("american_shad", "brandts_cormorant", "california_sea_lion", 'pelagic_cormorant', "guadelupe_fur_seal", "harbor_seal", "clam", "sharpnose_anchovy")

# define terrestrial species
# terrestrial_species <- c("deer_mouse", "anura", "bottas_pocket_gopher", "california_vole", "brush_rabbit", "american_robin", "woodrat", "desert_woodrat", "cattle","montane_vole", "wild_boar", "california_sea_lion", "western_harvest_mouse", "cricket", "dung_beetle", "chicken", "cow", "grey_fox", "sciuridae","golden_mantled_ground_squirrel", "yellow_bellied_marmot","corvidae", "song_sparrow", "wild_turkey", "california_ground_squirrel", "western_skink", "mule_deer","sparrow", "cactus_mouse", "spotted_towhee", "southern_grasshopper_mouse", "european_earwig", "scarab_beetle","jerusalem_cricket", "california_pocket_mouse","southern_alligator_lizard", "gopher_snake", "townsends_vole", "vole","common_carpet_beetle", "hairy_rove_beetle", "california_quail", "acorn_woodpecker", "downy_woodpecker", "big_eared_woodrat", "striped_skunk","pale_kangaroo_mouse", "gilberts_skink", "american_badger", "harvester_ant","california_scrub_jay", "red_fox", "stonefly", "western_rattlesnake", "house_mouse","black_rat") 

# for species level only
terrestrial_species <- c("bottas_pocket_gopher", "california_vole", "brush_rabbit", "american_robin", "desert_woodrat", "cattle", "montane_vole", "wild_boar", "california_sea_lion", "western_harvest_mouse", "dung_beetle", "chicken", "grey_fox", "golden_mantled_ground_squirrel", "yellow_bellied_marmot", "song_sparrow", "wild_turkey", "california_ground_squirrel", "western_skink", "mule_deer", "cactus_mouse", "spotted_towhee", "southern_grasshopper_mouse", "european_earwig", "scarab_beetle","jerusalem_cricket", "california_pocket_mouse","southern_alligator_lizard", "gopher_snake", "townsends_vole","common_carpet_beetle", "hairy_rove_beetle", "california_quail", "acorn_woodpecker", "downy_woodpecker", "big_eared_woodrat", "striped_skunk","pale_kangaroo_mouse", "gilberts_skink", "american_badger", "harvester_ant","california_scrub_jay", "red_fox", "stonefly", "western_rattlesnake", "house_mouse","black_rat", "western_deer_mouse") 

#remove all columns that aren't prey species names
gav_prop <- gav |> 
  select(-c(host, marine_present, terrestrial_present, diet_category, site, lon, lat, dist_to_coast_m))

# 1. Identify which columns correspond to marine species
marine_cols <- intersect(names(gav_prop), marine_species)

# 2. Identify which columns correspond to all species (marine + terrestrial)
all_species_cols <- setdiff(names(gav_prop), c("sample_id"))


gav_prop <- gav_prop |> 
  rowwise() %>%
  mutate(
    total_species = sum(c_across(all_of(all_species_cols))),
    marine_species_count = sum(c_across(all_of(marine_cols))),
    prop_marine = ifelse(total_species > 0,
                         marine_species_count / total_species,
                         0)   # or 0, depending on what makes sense
  ) %>%
  ungroup()

# # 3. Compute proportion of marine species per sample
# gav_prop <- gav_prop |> 
#   rowwise() %>%
#   mutate(
#     total_species = sum(c_across(all_of(all_species_cols))),                 # total species detected
#     marine_species_count = sum(c_across(all_of(marine_cols))),              # marine species detected
#     prop_marine = marine_species_count / total_species                      # proportion marine
#   ) %>%
#   ungroup() |> 
#   filter(prop_marine != "NaN")





###### combine proportions and distance to coast

# Make sure gav_dangermond_out has sample_id and dist_to_coast_m
dist_df <- gav_out %>%
  select(sample_id, host, site, lat, lon, dist_to_coast_m)

# Left join by sample_id
gav_prop <- left_join(gav_prop, dist_df, by = "sample_id")

# order from closest to farthest from the coast
gav_prop <- gav_prop %>%
  arrange(dist_to_coast_m)


#split up by site -- Dangermond
gav_dangermond_prop <- gav_prop |> 
  filter(site == 'Dangermond')

#split up by site -- COP/NCOS
gav_cop_ncos_prop <- gav_prop |> 
  filter(site == 'COP/NCOS')

# plot all points
ggplot(gav_prop, aes(x = dist_to_coast_m, y = prop_marine)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Distance to Coast (m)", y = "Proportion of Marine Species") +
  theme_minimal()

# plot Dangermond distance to coast
ggplot(gav_dangermond_prop, aes(x = dist_to_coast_m, y = prop_marine)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Distance to Coast (m)", y = "Proportion of Marine Diet Items", 
       title = 'Proportion of Marine Diet Items vs. Distance to Coast - Dangermond') +
  theme_minimal()

ggplot(gav_cop_ncos_prop, aes(x = dist_to_coast_m, y = prop_marine)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Distance to Coast (m)", y = "Proportion of Marine Diet Items",
       title = 'Proportion of Marine Diet Items vs. Distance to Coast - COP/NCOS') +
  theme_minimal()

# ggplot(gav_prop, aes(x = dist_to_coast_m, y = prop_marine, color = host)) +
#   geom_point(size = 2, alpha = 0.7) +
#   geom_smooth(method = "loess", se = TRUE) +
#   labs(x = "Distance to Coast (m)", y = "Proportion of Marine Species") +
#   theme_minimal()




write_csv(gav_dangermond_prop, file = here("metabarcoding_scripts_GL", "outputs", "gav_dangermond_dist_marine_prop.csv"))

write_csv(gav_cop_ncos_prop, file = here('metabarcoding_scripts_GL', "outputs", "gav_cop_ncos_dist_marine_prop.csv"))



################





