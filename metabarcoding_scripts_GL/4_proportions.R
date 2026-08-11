

library(tidyverse) 
library(here)
library(janitor)
library(patchwork)
library(geodata)
library(sf)
library(RColorBrewer)

#added 6/5/2026##
## count species, genus, family, orders ##
taxonomy_counts <- read_csv(here("data", "metabarcoding_data", "prey-taxonomy-counts.csv"))

length(unique(taxonomy_counts$species))

length(unique(taxonomy_counts$`common name`))

bobcat_diet_summary <- taxonomy_counts |>
  filter(Label == "Bobcat Wild") |>
  summarise(
    n_species = n_distinct(species),
    n_genus   = n_distinct(Genus),
    n_family  = n_distinct(Family),
    n_order   = n_distinct(order)
  )

bobcat_diet_summary

coyote_diet_summary <- taxonomy_counts |>
  filter(Label %in% c("Coyote Wild", "Coyote Urban")) |>
  summarise(
    n_species = n_distinct(species),
    n_genus   = n_distinct(Genus),
    n_family  = n_distinct(Family),
    n_order   = n_distinct(order)
  )

coyote_diet_summary

taxonomy_counts <- taxonomy_counts |>
  mutate(Label = trimws(Label))
  
boar_diet_summary <- taxonomy_counts |>
  filter(Label %in% "Wild Boar Wild") |>
  summarise(
    n_species = n_distinct(species),
    n_genus   = n_distinct(Genus),
    n_family  = n_distinct(Family),
    n_order   = n_distinct(order)
  )

boar_diet_summary




############ 8. Proportions #################

# this code defines diet categories: terrestrial, marine, both, or none
# finds proportions of samples that fall into those categories
# plots proportions
# runs chi squared test of independence
# outputs stats summary

# read in data file
#gaviota_pres_abs <- read_csv(here("metabarcoding_scripts_GL", "gaviota_pres_abs_broadtax.csv"))

gaviota_pres_abs <- read_csv(here("metabarcoding_scripts_GL", "gaviota_pres_abs_common_name.csv")) |> 
  #rename(clam = 'inflated ark clam') |> 
  filter(!SampleID %in% c('H.08.01.23.C1', 'H.08.01.23.H1')) |> 
  select(-ravinia) |>
  mutate(coyote = 
           case_when(SampleID == "D.07.24.23.B2" ~ 0, #change DSB2 (D.07.24.23.B2) to bobcat host, and take out coyote prey (now just has wild boar at 1%filter)
                     TRUE ~ coyote)) |> 
#take out wild boar when only in 0.5% filter  
  mutate(
    `wild boar` = case_when(
      SampleID %in% c(
        "D.06.26.23.C7",
        "D.06.26.23.C1",
        "D.08.14.23.C4",
        "D.06.25.23.C2",
        "D.07.11.23.B2",
        "D.09.09.22.H5",
        "D.00.00.22.H2", #Bobcat eating wild boar (from EH's samples so these were all pig.)
        "N.07.23.23.C1" #NCOS coyote
      ) ~ 0,
      TRUE ~ `wild boar`
    )
  ) |> 
# take out guadelupe fur seal, red fox
  mutate(`guadelupe fur seal` = case_when(
    `guadelupe fur seal` == 1 ~ 0,
    TRUE ~ `guadelupe fur seal`
  )) |> 
  mutate(`red fox` = 
           case_when(`red fox` == 1 ~ 0,
                     TRUE ~ `red fox`))

#take out anything that's not down to species level
# gaviota_pres_abs <- gaviota_pres_abs |> 
#   select(-`deer mouse`, -anura, -woodrat, -cricket, -sciuridae, -corvidae, -vole, -sparrow)


# used for Q4
gav_coords <- read_csv(here("metabarcoding_scripts_GL", "gaviota_with_coordinates.csv"))


# Define marine species to group
#marine_species <- c("brandts_cormorant", "california_sea_lion", "american_shad", "harbor_seal", "guadelupe_fur_seal")


#took out "green_algae", and "sea_butterfly_snail",
#marine_species <- c("american_shad", "brandts_cormorant", "california_sea_lion", 'pelagic_cormorant',  "guadelupe_fur_seal", "harbor_seal", "clam", "sharpnose_anchovy")

# define terrestrial species
# terrestrial_species <- c("deer_mouse", "anura", "bottas_pocket_gopher", "california_vole", "brush_rabbit", "american_robin", "woodrat", "desert_woodrat", "cattle", "montane_vole", "wild_boar", "california_sea_lion", "western_harvest_mouse", "cricket", "dung_beetle", "chicken", "grey_fox", "sciuridae","golden_mantled_ground_squirrel", "yellow_bellied_marmot","corvidae", "song_sparrow", "wild_turkey", "california_ground_squirrel", "western_skink", "mule_deer","sparrow", "cactus_mouse", "spotted_towhee", "southern_grasshopper_mouse", "european_earwig", "scarab_beetle","jerusalem_cricket", "california_pocket_mouse","southern_alligator_lizard", "gopher_snake", "townsends_vole", "vole","common_carpet_beetle", "hairy_rove_beetle", "california_quail", "acorn_woodpecker", "downy_woodpecker", "big_eared_woodrat", "striped_skunk","pale_kangaroo_mouse", "gilberts_skink", "american_badger", "harvester_ant","california_scrub_jay", "red_fox", "stonefly", "western_rattlesnake", "house_mouse","black_rat") 


# for species level only
# terrestrial_species <- c("bottas_pocket_gopher", "california_vole", "brush_rabbit", "american_robin", "desert_woodrat", "cattle", "montane_vole", "wild_boar", "california_sea_lion", "western_harvest_mouse", "dung_beetle", "chicken", "grey_fox", "golden_mantled_ground_squirrel", "yellow_bellied_marmot", "song_sparrow", "wild_turkey", "california_ground_squirrel", "western_skink", "mule_deer", "cactus_mouse", "spotted_towhee", "southern_grasshopper_mouse", "european_earwig", "scarab_beetle","jerusalem_cricket", "california_pocket_mouse","southern_alligator_lizard", "gopher_snake", "townsends_vole","common_carpet_beetle", "hairy_rove_beetle", "california_quail", "acorn_woodpecker", "downy_woodpecker", "big_eared_woodrat", "striped_skunk","pale_kangaroo_mouse", "gilberts_skink", "american_badger", "harvester_ant","california_scrub_jay", "red_fox", "stonefly", "western_rattlesnake", "house_mouse","black_rat", "western_deer_mouse") 

prey_columns <- setdiff(names(gaviota_pres_abs), c("SampleID"))  # exclude metadata columns
cat("c(", paste0('"', prey_columns, '"', collapse = ", "), ")") 

prey_columns <- prey_columns |> make_clean_names()

prey_columns <- make_clean_names(c( "coyote", "deer mouse", "frog", "botta's pocket gopher", "brandt's cormorant", "pelagic cormorant", "california vole", "brush rabbit", "american robin", "woodrat", "domestic cattle", "wild boar", "california sea lion", "western harvest mouse", "cricket", "dung beetle", "chicken", "grey fox", "california ground squirrel", "common raven", "song sparrow", "wild turkey", "skilton's skink", "bobcat", "mule deer", "white-crowned sparrow", "spotted towhee", "earwig", "heerman's kangaroo rat", "california alligator lizard", "gopher snake", "carpet beetle", "rove beetle", "california quail", "american shad", "sharpnose anchovy", "acorn woodpecker", "downy woodpecker", "clam", "striped skunk", "american badger", "harvestor ant", "western scrub jay", "harbor seal", "puma", "guadelupe fur seal", "red fox", "stonefly", "western rattlesnake", "house mouse", "black rat" ))

#for 1% filter
#prey_columns <- make_clean_names(c( "coyote", "deer mouse", "botta's pocket gopher", "brandt's cormorant", "pelagic cormorant", "california vole", "brush rabbit", "woodrat", "domestic cattle", "wild boar", "california sea lion", "western harvest mouse", "cricket", "dung beetle", "chicken", "grey fox", "california ground squirrel", "common raven", "song sparrow", "wild turkey", "skilton's skink", "bobcat", "mule deer", "white-crowned sparrow", "earwig", "heerman's kangaroo rat", "california alligator lizard", "gopher snake", "carpet beetle", "california quail", "american shad", "sharpnose anchovy", "acorn woodpecker", "downy woodpecker", "clam", "striped skunk", "american badger", "harvestor ant", "western scrub jay", "harbor seal", "puma", "red fox", "stonefly", "western rattlesnake", "house mouse", "black rat" ))

# take out grey fox
terrestrial_species <- make_clean_names(c("coyote", "deer mouse", "frog", "botta's pocket gopher", "california vole", "brush rabbit", "american robin", "woodrat", "domestic cattle", "wild boar",  "western harvest mouse", "cricket", "dung beetle", "chicken", "california ground squirrel", "common raven", "song sparrow", "wild turkey", "skilton's skink", "bobcat", "mule deer", "white-crowned sparrow", "spotted towhee", "earwig", "heerman's kangaroo rat", "california alligator lizard", "gopher snake", "carpet beetle", "rove beetle", "california quail", "acorn woodpecker", "downy woodpecker",  "striped skunk", "american badger", "harvestor ant", "western scrub jay", "puma", "red fox", "stonefly", "western rattlesnake", "house mouse", "black rat" ))

#terrestrial_species <- make_clean_names(c("coyote", "deer mouse", "frog", "botta's pocket gopher", "california vole", "brush rabbit", "american robin", "woodrat", "domestic cattle", "wild boar",  "western harvest mouse", "cricket", "dung beetle", "chicken", "grey fox", "california ground squirrel", "common raven", "song sparrow", "wild turkey", "skilton's skink", "bobcat", "mule deer", "white-crowned sparrow", "spotted towhee", "earwig", "heerman's kangaroo rat", "california alligator lizard", "gopher snake", "carpet beetle", "rove beetle", "california quail", "acorn woodpecker", "downy woodpecker",  "striped skunk", "american badger", "harvestor ant", "western scrub jay", "puma", "red fox", "stonefly", "western rattlesnake", "house mouse", "black rat" ))

#for 1% filter
#terrestrial_species <- make_clean_names(c("coyote", "deer mouse", "botta's pocket gopher", "california vole", "brush rabbit", "woodrat", "domestic cattle", "wild boar",  "western harvest mouse", "cricket", "dung beetle", "chicken", "grey fox", "california ground squirrel", "common raven", "song sparrow", "wild turkey", "skilton's skink", "bobcat", "mule deer", "white-crowned sparrow", "earwig", "heerman's kangaroo rat", "california alligator lizard", "gopher snake", "carpet beetle", "california quail", "acorn woodpecker", "downy woodpecker",  "striped skunk", "american badger", "harvestor ant", "western scrub jay", "puma", "red fox", "stonefly", "western rattlesnake", "house mouse", "black rat" ))

marine_species <- make_clean_names(c("brandt's cormorant", "pelagic cormorant", "california sea lion","american shad", "sharpnose anchovy", "harbor seal", "guadelupe fur seal", "clam"))

#for 1% filter
#marine_species <- make_clean_names(c("brandt's cormorant", "pelagic cormorant", "california sea lion","american shad", "sharpnose anchovy", "harbor seal", "clam")) 

# Assign categories






  # gav_coords |> filter(!common_name %in% marine_species) |> 
  # select(common_name) |> 
  # unique() |> 
  # filter(!common_name %in% c("coyote", "bobcat")) |> 
  # clean_names()

#terrestrial_species <- c("small_rodent", "brush_rabbit", "wild_boar", "cattle", "terrestrial_bird", "grey_fox", "mule_deer", "gilberts_skink", "american_badger", "puma", "striped_skunk", "red_fox", "anura")

# create marine, terrestrial, both diet category
# Create new column 'marine' = 1 if ANY marine species present, else 0

gaviota_pres_abs_host <- gaviota_pres_abs |>
  janitor::clean_names() |>
  # tibble::rownames_to_column(var = "SampleID") |>
  rowwise() |>
  mutate(
    host = case_when(
      coyote == 1 ~ 'coyote',
      puma == 1 ~ 'puma',
      bobcat == 1 & coyote == 1 ~ 'coyote',
      coyote == 1 & wild_boar == 1 ~ 'coyote',
      mule_deer == 1 & coyote == 1 ~ 'coyote',
      
      bobcat == 1 & coyote == 0 ~ 'bobcat',
      wild_boar == 1 & coyote == 0 ~ 'wild boar',
      mule_deer == 1 & coyote == 0 ~ 'mule deer',
      domestic_cattle == 1 & coyote == 0 ~ 'cattle',
      TRUE ~ NA_character_
    )) |> 
  
  #make values of species columns 0 if they are the host.
    mutate(coyote = case_when(
      host %in% "coyote" ~ 0,
      TRUE ~ coyote
    )) |> 
      mutate(bobcat = case_when(
        host %in% "bobcat" ~ 0,
        TRUE ~ bobcat
      )) |> 
      mutate(mule_deer = case_when(
        host %in% "mule deer" ~ 0,
        TRUE ~ mule_deer
      )) |> 
      mutate(wild_boar = case_when(
        host %in% "wild boar" ~ 0,
        TRUE ~ wild_boar
      )) |> 
    mutate(puma = case_when(
      host %in% "puma" ~ 0,
      TRUE ~ puma
    )) |> 
    filter(host != "cattle") |> 
  #take out green algae and sea butterfly snail
  #select(-green_algae, -sea_butterfly_snail)
  
  # take out all instances of coyote eating bobcat
  mutate(bobcat = 
           case_when(host == 'coyote' ~ 0,
                     TRUE ~ bobcat))


write_csv(gaviota_pres_abs_host, file = "data/gaviota_pres_abs_host.csv")






###########







#check how many counts of things I'd be taking out by only taking species level:
# dont_have_species_level <- gaviota_pres_abs_host %>%
#   select(deer_mouse, anura, woodrat, cricket, sciuridae, corvidae, vole, sparrow) %>%
#   rowwise() %>%
#   mutate(count = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
#   ungroup() %>%
#   summarise(total_count = sum(count, na.rm = TRUE)) %>%
#   pull(total_count)





# calculate averages prey items per host

# took out "sea butterfly snail" and "green algae", changed "inflated ark clam" to 'clam'
# take out cow, frog, mold, fungi, brown dog tick
# prey_columns <- make_clean_names(c("coyote", "deer mouse", "anura", "botta's pocket gopher", "brandt's cormorant", "cormorant", "california vole", "brush rabbit", "american robin", "woodrat", "desert woodrat", "cattle","montane vole", "wild boar", "california sea lion", "western harvest mouse", "cricket", "dung beetle","chicken", "grey fox", "sciuridae","golden-mantled ground squirrel", "yellow-bellied marmot", "corvidae", "song sparrow", "wild turkey", "california ground squirrel", "western skink","bobcat", "mule deer","sparrow", "cactus mouse", "spotted towhee", "southern grasshopper mouse", "european earwig", "scarab beetle","jerusalem cricket", "california pocket mouse","southern alligator lizard","gopher snake", "townsend's vole", "vole", "common carpet beetle", "hairy rove beetle", "california quail","american shad","sharpnose anchovy", "acorn woodpecker", "downy woodpecker","clam", "big-eared woodrat", "striped skunk","pale kangaroo mouse", "gilbert's skink", "american badger", "harvester ant","california scrub jay", "harbor seal", "puma","guadelupe fur seal","red fox", "stonefly", "western rattlesnake", "house mouse","black rat"))


# #for species only  
# prey_columns <- make_clean_names(c("coyote", "botta's pocket gopher", "brandt's cormorant", "pelagic cormorant", "california vole", "brush rabbit", "american robin", "desert woodrat","western deer mouse", "cattle","montane vole", "wild boar", "california sea lion", "western harvest mouse", "dung beetle","chicken", "grey fox", "golden-mantled ground squirrel", "yellow-bellied marmot", "song sparrow", "wild turkey", "california ground squirrel", "western skink","bobcat", "mule deer", "cactus mouse", "spotted towhee", "southern grasshopper mouse", "european earwig", "scarab beetle","jerusalem cricket", "california pocket mouse","southern alligator lizard","gopher snake", "townsend's vole", "common carpet beetle", "hairy rove beetle", "california quail","american shad","sharpnose anchovy", "acorn woodpecker", "downy woodpecker","clam", "big-eared woodrat", "striped skunk","pale kangaroo mouse", "gilbert's skink", "american badger", "harvester ant","california scrub jay", "harbor seal", "puma","guadelupe fur seal","red fox", "stonefly", "western rattlesnake", "house mouse","black rat"))

#make prey count column in new dataframe
gaviota_pres_abs_prey_count <- gaviota_pres_abs_host %>%
  rowwise() %>%
  mutate(prey_count = sum(c_across(all_of(prey_columns)), na.rm = TRUE)) %>%
  ungroup()

# Then, compute the average prey count per host
avg_prey_per_host <- gaviota_pres_abs_prey_count %>%
  group_by(host) %>%
  summarise(
    mean_prey_items = mean(prey_count),
    sd_prey_items = sd(prey_count),
    n_samples = n()
  )

prey_summary_by_host <- gaviota_pres_abs_prey_count %>%
  group_by(host) %>%
  summarise(
    total_samples = n(),
    samples_with_prey = sum(prey_count > 0, na.rm = TRUE),
    percent_with_prey = (samples_with_prey / total_samples) * 100,
    total_prey_items = sum(prey_count, na.rm = TRUE)
  )


########## make stacked bar chart 

gav_coords_sites <- gav_coords |> 
  rename(sample_id = ScatID) |> 
  select(sample_id, site, lon, lat)

# de-duplicate gav_coords. This has 244 observations. Grouped_prey only has 234 observations because I took out ones that are not the target species, weird ones, etc.
gav_coords_unique <- gav_coords_sites %>% distinct(sample_id, .keep_all = TRUE)

# add sites and lat, lon to grouped prey dataframe
gav_host_sites <- gaviota_pres_abs_host %>%
  left_join(gav_coords_unique, by = "sample_id") |> 
  mutate(host = case_when(
    host == "coyote" & site == "Dangermond" ~ "Coyote\nWild",
    host == "coyote" & site %in% c("Coal Oil Point", "North Campus Open Space") ~ "Coyote\nUrban",
    host == "bobcat" ~ "Bobcat\nWild",
    host == "wild boar" ~ "Wild Boar\nWild",
    TRUE ~ host
  ))

# #take out instances of wild coyote eating wild boar (7), red fox (1), guadelupe fur seal (1), and bobcat (5) (#'s are instances of this) 
# #before removal, total prey count at Dangermond = 239. AFTER, total prey count at Dangermond = 225.
# gav_host_sites <- gav_host_sites |>
#   mutate(
#     wild_boar = case_when(
#       host == "Coyote\nWild" & site == "Dangermond" ~ 0,
#       TRUE ~ wild_boar
#     )) |> 
#   mutate(
#     bobcat = case_when(
#       host == "Coyote\nWild" & site == "Dangermond" ~ 0,
#       TRUE ~ bobcat
#     )) |> 
#   mutate(
#     red_fox = case_when(
#       host == "Coyote\nWild" & site == "Dangermond" ~ 0,
#       TRUE ~ red_fox
#     )) |> 
#   mutate(
#     guadelupe_fur_seal = case_when(
#       host == "Coyote\nWild" & site == "Dangermond" ~ 0,
#       TRUE ~ guadelupe_fur_seal
#     ))
#     
# df <- gav_host_sites |> 
#   filter(host == 'Bobcat\nWild')

#make dataframe with host, prey, count
gaviota_prey_count_long <- gav_host_sites %>%
  rowwise() %>%
  mutate(prey_count = sum(c_across(all_of(prey_columns)), na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_longer(
    cols = all_of(prey_columns),
    names_to = "prey",
    values_to = "count"
  ) %>%
  filter(count > 0) %>%  # optional: only keep prey actually present
  select(host = host, prey, count) |> 
  group_by(host, prey) |> 
  summarise(count = n()) |> 
  mutate(total = sum(count)) |> 
  mutate(prop = count/total) |> 
  mutate(prey = case_when(
    prey == "common_raven" ~ "crow",
    prey == "cricket" ~ "jerusalem_cricket",
    prey == "domestic_cattle" ~ "cattle",
    prey == "harvestor_ant" ~ "harvester_ant",
    TRUE ~ prey)) |> 
  mutate(prey = str_replace_all(prey, "_", " "))



  


#make species category list
species_category <- list(
  terrestrial_bird = c(
    "american robin", "crow", "song sparrow", "wild turkey", 
    "california quail", "acorn woodpecker", "downy woodpecker", "western scrub jay", "chicken", "white crowned sparrow", 
    "spotted towhee"
  ),
  small_mammal = c(
    "deer mouse", "bottas pocket gopher", "california vole", "brush rabbit", 
    "woodrat", "western harvest mouse", "heermans kangaroo rat", "house mouse", "black rat", "california ground squirrel", "striped skunk"
  ),
  reptile_amphibian = c(
    "frog", "skiltons skink", "california alligator lizard", "gopher snake", "western rattlesnake"
  ),
  marine_species = c(
    "california sea lion", "harbor seal", "guadelupe fur seal", "american shad", "sharpnose anchovy", 
    "brandts cormorant", "pelagic cormorant", "clam"
  ),
  insects = c(
    "jerusalem cricket", "dung beetle", "earwig", "carpet beetle", "rove beetle", "harvester ant", "stonefly"
  ),
  other = c(
    "coyote", "cattle", "wild boar", "grey fox", 
    "bobcat", "mule deer", "american badger", "red fox", "puma"
  )
)

# species_category <- lapply(species_category, function(x) {
#   make_clean_names(x)
# })



#--- 2️⃣ Flatten into named vector mapping prey -> category ---
species_category_vec <- unlist(lapply(names(species_category), function(cat) {
  setNames(rep(cat, length(species_category[[cat]])), species_category[[cat]])
}))

# --- 3️⃣ Add species_category to dataframe ---
gaviota_prey_count_long <- gaviota_prey_count_long %>%
  mutate(#prey = make_clean_names(prey),
         species_category = species_category_vec[prey])

# --- 4️⃣ Order prey factor by category order, marine on top ---
category_order <- c("marine_species", "terrestrial_bird", "small_mammal", "reptile_amphibian", "insects", "other")

prey_levels_ordered <- gaviota_prey_count_long %>%
  distinct(prey, species_category) %>%
  arrange(factor(species_category, levels = category_order), prey) %>%
  pull(prey) |> 
  unique()

gaviota_prey_count_long <- gaviota_prey_count_long %>%
  mutate(prey = factor(prey, levels = prey_levels_ordered))

# --- 5️⃣ Create gradient colors for each category ---
category_colors <- list(
  terrestrial_bird = colorRampPalette(brewer.pal(9, "Greens"))(length(species_category$terrestrial_bird)),
  small_mammal = colorRampPalette(brewer.pal(9, "Oranges"))(length(species_category$small_mammal)),
  reptile_amphibian = colorRampPalette(brewer.pal(9, "YlGnBu"))(length(species_category$reptile_amphibian)),
  insects = colorRampPalette(brewer.pal(9, "Purples"))(length(species_category$insects)),
  other = colorRampPalette(brewer.pal(9, "Greys"))(length(species_category$other)),
  marine_species = colorRampPalette(brewer.pal(9, "Blues"))(length(species_category$marine_species))
)

# 5. Create color palettes
category_colors <- list(
  terrestrial_bird = colorRampPalette(c("#FCC0DA", "#FF0000"))(length(species_category$terrestrial_bird)),
  small_mammal = colorRampPalette(c("#B4E6B1", "#005713"))(length(species_category$small_mammal)),
  reptile_amphibian = colorRampPalette(c("#F1B8FF", "#620C97"))(length(species_category$reptile_amphibian)),
  insects = colorRampPalette(c("#EB9D65", "#914C00"))(length(species_category$insects)),
  other = colorRampPalette(c("#F9FA93", "#FF7518"))(length(species_category$other)),
  marine_species = colorRampPalette(c("#BAFBFF", "#0C6297"))(length(species_category$marine_species))
)


# Flatten to single named vector for ggplot
prey_colors <- unlist(lapply(names(category_colors), function(cat) {
  setNames(category_colors[[cat]], species_category[[cat]])
}))

gaviota_prey_count_long$host <- factor(
  gaviota_prey_count_long$host,
  levels = c("Coyote\nUrban", "Coyote\nWild", "Bobcat\nWild", "Wild Boar\nWild")  # change to your desired order
)

# --- 6️⃣ Plot stacked bar plot ---
all_prey_plot <- ggplot(gaviota_prey_count_long, aes(x = host, y = prop, fill = prey)) +
  #geom_bar(stat = "identity") +
  geom_histogram(stat = "identity")+
  scale_fill_manual(values = prey_colors) +
  labs(
    x = "",
    y = "Proportion of Prey Occurences",
    fill = "Prey Species"
    #title = "Prey Composition per Predator"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

all_prey_plot


ggsave(plot = all_prey_plot, filename = here("figs", "all_prey_plot.png"), dpi = 600, units = "in", height = 5, width = 10)

ggsave(plot = all_prey_plot, filename = here("figs", "all_prey_plot.png"), dpi = 600, units = "in", height = 5, width = 9)

ggsave(plot = all_prey_plot, filename = here("figs", "all_prey_plot_legend.png"), dpi = 600, units = "in", height = 5, width = 5.4)

######## condence to just color by group #######

category_colors_groups <- list(
  terrestrial_bird = colorRampPalette(c("#FFCAAF", "#FFCAAF"))(length(species_category$terrestrial_bird)),
  small_mammal = colorRampPalette(c("#DE9E36", "#DE9E36"))(length(species_category$small_mammal)),
  reptile_amphibian = colorRampPalette(c("#62A87C", "#62A87C"))(length(species_category$reptile_amphibian)),
  insects = colorRampPalette(c("#9C6F65", "#9C6F65"))(length(species_category$insects)),
  other = colorRampPalette(c("#854D8D", "#854D8D"))(length(species_category$other)),
  marine_species = colorRampPalette(c("#267790", "#267790"))(length(species_category$marine_species))
)

# Flatten to single named vector for ggplot
prey_colors_group <- unlist(lapply(names(category_colors_groups), function(cat) {
  setNames(category_colors_groups[[cat]], species_category[[cat]])
}))

gaviota_prey_count_long$host <- factor(
  gaviota_prey_count_long$host,
  levels = c("Coyote\nUrban", "Coyote\nWild", "Bobcat\nWild", "Wild Boar\nWild")  # change to your desired order
)



# --- 6️⃣ Plot stacked bar plot ---
all_prey_plot_group <- ggplot(gaviota_prey_count_long, aes(x = host, y = prop, fill = prey)) +
  #geom_bar(stat = "identity") +
  geom_histogram(stat = "identity")+
  scale_fill_manual(values = prey_colors_group) +
  labs(
    x = "",
    y = "Proportion of Prey Occurences",
    fill = "Prey Species"
    #title = "Prey Composition per Predator"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

all_prey_plot_group



##### trying with hatches ######

# Install if needed: install.packages("ggpattern")
library(ggpattern)
library(RColorBrewer)
library(dplyr)

# --- Helper to create color + pattern pairs ---
make_category_palette_pattern <- function(light_color, dark_color, species_list) {
  n <- length(species_list)
  n_colors <- ceiling(n / 2)
  
  # generate base colors
  base_colors <- colorRampPalette(c(light_color, dark_color))(n_colors)
  
  # assign each color twice (solid + stripe)
  colors  <- rep(base_colors, each = 2)[seq_len(n)]
  pattern <- rep(c("none", "stripe"), length.out = n)
  
  data.frame(
    prey    = species_list,
    color   = colors,
    pattern = pattern,
    stringsAsFactors = FALSE
  )
}

# --- 1️⃣ Define category order ---
category_order <- c("marine_species", "terrestrial_bird", "small_mammal",
                    "reptile_amphibian", "insects", "other")

# --- 2️⃣ Order prey factor by category ---
prey_levels_ordered <- gaviota_prey_count_long %>%
  distinct(prey, species_category) %>%
  arrange(factor(species_category, levels = category_order), prey) %>%
  pull(prey) |> 
  unique()

gaviota_prey_count_long <- gaviota_prey_count_long %>%
  mutate(prey = factor(prey, levels = prey_levels_ordered))

# --- 4️⃣ Keep only species present in the data (DO THIS FIRST) ---
prey_in_data <- prey_levels_ordered

marine_species <- intersect(species_category$marine_species, prey_in_data)
terrestrial_bird <- intersect(species_category$terrestrial_bird, prey_in_data)
small_mammal <- intersect(species_category$small_mammal, prey_in_data)
reptile_amphibian <- intersect(species_category$reptile_amphibian, prey_in_data)
insects <- intersect(species_category$insects, prey_in_data)
other <- intersect(species_category$other, prey_in_data)

# --- 3️⃣ Build pattern + color table ---
pattern_df <- rbind(
  make_category_palette_pattern("#BAFBFF", "#0C6297", species_category$marine_species),
  #make_category_palette_pattern("#fcb3b3", "#f20202", species_category$terrestrial_bird),
  make_category_palette_pattern("#FFC9D9", "#fa0202", species_category$terrestrial_bird),
  make_category_palette_pattern("#B4E6B1", "#005713", species_category$small_mamma),
  make_category_palette_pattern("#F1B8FF", "#620C97", species_category$reptile_amphibian),
  make_category_palette_pattern("#EB9D65", "#914C00", species_category$insects),
  make_category_palette_pattern("#F9FA93", "#FF7518", species_category$other)
)

# # --- 4️⃣ Keep only species present in the data ---
# prey_in_data <- prey_levels_ordered
# pattern_df <- pattern_df %>% filter(prey %in% prey_in_data)

# --- 5️⃣ Force the pattern_df to match the factor ordering exactly ---
pattern_df <- pattern_df[match(prey_in_data, pattern_df$prey), ]

# --- 6️⃣ Build named vectors ---
prey_colors   <- setNames(pattern_df$color, pattern_df$prey)
prey_patterns <- setNames(pattern_df$pattern, pattern_df$prey)

# --- 7️⃣ Plot with ggpattern ---
all_prey_plot <- ggplot(
  gaviota_prey_count_long,
  aes(x = host, y = prop, fill = prey, pattern = prey)
) +
  geom_bar_pattern(
    stat = "identity",
    pattern_colour = "white",
    pattern_fill = 'white',
    pattern_angle = 45,
    pattern_density = 0.1,
    pattern_spacing = 0.02,
    pattern_key_scale_factor = 0.6
  ) +
  scale_fill_manual(values = prey_colors) +
  scale_pattern_manual(values = prey_patterns) +
  guides(
    fill = guide_legend(
      override.aes = list(pattern = unname(prey_patterns))
    ),
    pattern = "none"
  ) +
  labs(
    x = "",
    y = "Proportion of Prey Occurences",
    fill = "Prey Species"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

all_prey_plot

#ggsave(plot = all_prey_plot, filename = here("figs", "all_prey_plot_hashes.png"), dpi = 600, units = "in", height = 5, width = 10)

ggsave(plot = all_prey_plot, filename = here("figs", "all_prey_plot_hashes.png"), dpi = 600, units = "in", height = 5, width = 9)

#ggsave(plot = all_prey_plot, filename = here("figs", "all_prey_plot_legend_hashes.png"), dpi = 600, units = "in", height = 5, width = 5.4)

###### end trying with hatches ######


##### try with 3 patterns: stripe diagonal, stripe vertical, none #####


# # ------------------------
# # 2️⃣ Function to create palette with 3 patterns
# # ------------------------
# make_palette_pattern <- function(light_color, dark_color, species_list) {
#   n <- length(species_list)
#   n_colors <- ceiling(n / 3)
#   
#   # Gradient colors
#   base_colors <- colorRampPalette(c(light_color, dark_color))(n_colors)
#   colors <- rep(base_colors, each = 3)[seq_len(n)]
#   
#   # Patterns: diagonal stripe (45°), vertical stripe (90°), solid (0°)
#   pattern <- rep(c("stripe", "stripe", "none"), length.out = n)
#   pattern_angle <- rep(c(45, 90, 0), length.out = n)
#   
#   data.frame(
#     prey = species_list,
#     color = colors,
#     pattern = pattern,
#     pattern_angle = pattern_angle,
#     stringsAsFactors = FALSE
#   )
# }
# 
# # ------------------------
# # 3️⃣ Build palette for all species
# # ------------------------
# pattern_df <- rbind(
#   make_palette_pattern("#BAFBFF", "#0C6297", marine_species),
#   make_palette_pattern("#FCC0DA", "#FF0000", terrestrial_bird),
#   make_palette_pattern("#B4E6B1", "#005713", small_mammal),
#   make_palette_pattern("#F1B8FF", "#620C97", reptile_amphibian),
#   make_palette_pattern("#EB9D65", "#914C00", insects),
#   make_palette_pattern("#F9FA93", "#FF7518", other)
# )
# 
# # ------------------------
# # 4️⃣ Merge palette into main dataset
# # ------------------------
# gaviota_prey_count_long <- gaviota_prey_count_long %>%
#   mutate(prey = as.character(prey)) %>%
#   left_join(pattern_df, by = "prey") %>%
#   mutate(prey = factor(prey, levels = prey_levels_ordered))  # enforce stacking order
# 
# # ------------------------
# # 5️⃣ Named vectors for scales
# # ------------------------
# prey_colors <- setNames(pattern_df$color, pattern_df$prey)
# prey_patterns <- setNames(pattern_df$pattern, pattern_df$prey)
# 
# # ------------------------
# # 6️⃣ Plot
# # ------------------------
# all_prey_plot <- ggplot(
#   gaviota_prey_count_long,
#   aes(
#     x = host,
#     y = prop,
#     fill = prey,
#     pattern = pattern,
#     pattern_angle = pattern_angle
#   )
# ) +
#   geom_bar_pattern(
#     stat = "identity",
#     pattern_colour = "white",
#     pattern_fill = "white",
#     pattern_density = 0.2,
#     pattern_spacing = 0.03,
#     pattern_key_scale_factor = 0.6
#   ) +
#   scale_fill_manual(values = prey_colors) +
#   scale_pattern_manual(values = prey_patterns) +
#   guides(
#     fill = guide_legend(override.aes = list(pattern = pattern_df$pattern)),
#     pattern = "none"
#   ) +
#   labs(
#     x = "",
#     y = "Proportion of Prey Occurrences",
#     fill = "Prey Species"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# all_prey_plot
##########




# 
# ggplot(gaviota_prey_count_long, aes(x = host, y = count, fill = prey)) +
#   geom_bar(stat = "identity") +                # stacked by default
#   #scale_fill_manual(values = prey_colors) +   # custom colors
#   labs(
#     x = "Host",
#     y = "Number of Prey Items",
#     fill = "Prey Species",
#     title = "Prey Composition per Host"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   )






#make groups of marine, terrestrial, and mixed prey items

marine_species <- gsub(" ", "_", marine_species)
terrestrial_species <- gsub(" ", "_", terrestrial_species)

grouped_prey_marine_mixed_terr <- gaviota_pres_abs_host |> 
  mutate(
    marine_present = any(c_across(all_of(marine_species)) == 1),
    terrestrial_present = any(c_across(all_of(terrestrial_species)) == 1),
    
    # terrestrial_present = case_when(mule_deer == 1 & any(c_across(all_of(marine_species)) == 1) ~ FALSE,
    #,          TRUE ~ terrestrial_present),
    
    diet_category = case_when(
      marine_present & !terrestrial_present ~ "marine",
      terrestrial_present & !marine_present ~ "terrestrial",
      marine_present & terrestrial_present ~ "mixed",
      TRUE ~ "none"
      
    )
  ) |> 
  mutate(diet_category = factor(diet_category, levels = c("marine", "mixed", "terrestrial", "none"))) |> 
  ungroup() 

# make grouped_prey with just marine/mixed and terrestrial categories
grouped_prey_marine_terr <- gaviota_pres_abs |>
  janitor::clean_names() |>
  # tibble::rownames_to_column(var = "SampleID") |>
  rowwise() |>
  mutate(
    host = case_when(
      coyote == 1 ~ 'coyote',
      bobcat == 1 & coyote == 1 ~ 'coyote',
      coyote == 1 & wild_boar == 1 ~ 'coyote',
      mule_deer == 1 & coyote == 1 ~ 'coyote',
      
      bobcat == 1 & coyote == 0 ~ 'bobcat',
      wild_boar == 1 & coyote == 0 ~ 'wild boar',
      mule_deer == 1 & coyote == 0 ~ 'mule deer',
      domestic_cattle == 1 & coyote == 0 ~ 'domestic cattle',
      TRUE ~ NA_character_
    )) |> 
  
  #make values of species columns 0 if they are the host.
  mutate(coyote = case_when(
    host %in% "coyote" ~ 0,
    TRUE ~ coyote
  )) |> 
  mutate(bobcat = case_when(
    host %in% "bobcat" ~ 0,
    TRUE ~ bobcat
  )) |> 
  mutate(mule_deer = case_when(
    host %in% "mule deer" ~ 0,
    TRUE ~ mule_deer
  )) |> 
  mutate(wild_boar = case_when(
    host %in% "wild boar" ~ 0,
    TRUE ~ wild_boar
  )) |> 
  
  filter(host != "domestic cattle") |>
  mutate(
    marine_present = any(c_across(all_of(marine_species)) == 1),
    terrestrial_present = any(c_across(all_of(terrestrial_species)) == 1),
    
    # terrestrial_present = case_when(mule_deer == 1 & any(c_across(all_of(marine_species)) == 1) ~ FALSE,
    #                                 TRUE ~ terrestrial_present),
    
    diet_category = case_when(
      marine_present ~ "marine or mixed",
      terrestrial_present & !marine_present ~ "terrestrial",
      TRUE ~ "none"
      
    )
  ) |> 
  ungroup()

## take out "none" category so that it is just showing marine, mixed, and terrestrial
grouped_prey_without_none <- grouped_prey_marine_mixed_terr |>
  filter(diet_category != "none")

# take out none category so it's just showing marine/mixed and terrestrial
grouped_prey_marine_terr_without_none <- grouped_prey_marine_terr |> 
  filter(diet_category != "none")


# Summarize counts
summary_prey_prop_marine_mixed_terr <- grouped_prey_marine_mixed_terr |> 
  count(host, diet_category) |> 
  group_by(host) |> 
  mutate(prop = n / sum(n)*100) |> # proportion as percentage
  ungroup() 

summary_prey_prop_marine_terr <- grouped_prey_marine_terr |> 
  count(host, diet_category) |> 
  group_by(host) |> 
  mutate(prop = n / sum(n)*100) |> # proportion as percentage
  ungroup() 

summary_prey_prop_without_none <- grouped_prey_without_none |> 
  count(host, diet_category) |> 
  group_by(host) |> 
  mutate(prop = n / sum(n)*100) |> # proportion as percentage
  ungroup() 

summary_prey_marine_terr_without_none <- grouped_prey_marine_terr_without_none |> 
  count(host, diet_category) |> 
  group_by(host) |> 
  mutate(prop = n / sum(n)*100) |> # proportion as percentage
  ungroup() 


# |> 
#   filter(host %in%  c("coyote", "bobcat"))

##############################################################################
# plots with marine, mixed, terrestrial, none
##############################################################################

ggplot(summary_prey_prop_marine_mixed_terr, aes(x = host, y = prop, fill = diet_category)) +
  geom_col(position = "stack") +
  # geom_text(aes(label = paste0(round(prop, 1), "%")),
  #           position = position_stack(vjust = 0.5),  # center text in each segment
  #           size = 3, color = "white") +   
  
  geom_text(
    aes(label = paste0(n, "\n", round(prop, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3, color = "white" ) +   # tweak text size & color

  labs(
    x = "Predator Species",
    y = "Percentage of Individual Samples (%)",
    fill = "Diet Category"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_flip()  # flip for easier reading (optional)
ggsave(filename = "figs/fig1.png", dpi = 600, width = 8.5, height = 5, units = "in")


##### to add numbers of host to plot
# Calculate totals and create labels
totals <- summary_prey_prop_marine_mixed_terr |> 
  group_by(host) |> 
  summarise(total_n = sum(n))

label_with_totals <- totals |> 
  mutate(host_label = paste0(host, "\n(n = ", total_n, ")")) |> 
  select(host, host_label)

# Join first to add label column, then create factor
summary_prey_prop_marine_mixed_terr_joined <- summary_prey_prop_marine_mixed_terr |> 
  left_join(label_with_totals, by = "host") 


# Plot with new labels on y axis
ggplot(summary_prey_prop_marine_mixed_terr_joined, aes(x = host_label, y = prop, fill = diet_category)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = paste0(n, "\n", round(prop, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3, color = "white"
  ) +
  labs(
    x = "Predator Species",
    y = "Percentage of Individual Samples (%)",
    fill = "Diet Category",
    title = "Q1: Which Species Forage Coasts?"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_flip() +
  theme(axis.title.y = element_blank())


######################################################################################################
# plots with just marine/mixed and terrestrial categories
######################################################################################################


ggplot(summary_prey_prop_marine_terr, aes(x = host, y = prop, fill = diet_category)) +
  geom_col(position = "stack") +
  # geom_text(aes(label = paste0(round(prop, 1), "%")),
  #           position = position_stack(vjust = 0.5),  # center text in each segment
  #           size = 3, color = "white") +   
  
  geom_text(
    aes(label = paste0(n, "\n", round(prop, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3, color = "white" ) +   # tweak text size & color
  
  labs(
    x = "Predator Species",
    y = "Percentage of Individual Samples (%)",
    fill = "Diet Category",
    title = "Q1: Which species forage coasts?"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_flip()  # flip for easier reading (optional)


##### to add numbers of host to plot
# Calculate totals and create labels
totals <- summary_prey_prop_marine_terr |> 
  group_by(host) |> 
  summarise(total_n = sum(n))

label_with_totals <- totals |> 
  mutate(host_label = paste0(host, "\n(n = ", total_n, ")")) |> 
  select(host, host_label)

# Join first to add label column, then create factor
summary_prey_prop_marine_terr_joined <- summary_prey_prop_marine_terr |> 
  left_join(label_with_totals, by = "host") 


# Plot with new labels on y axis
ggplot(summary_prey_prop_marine_terr_joined, aes(x = host_label, y = prop, fill = diet_category)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = paste0(n, "\n", round(prop, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3, color = "white"
  ) +
  labs(
    x = "Predator Species",
    y = "Percentage of Individual Samples (%)",
    fill = "Diet Category",
    title = "Q1: Which Species Forage Coasts?"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_flip() +
  theme(axis.title.y = element_blank())


######################################################################################################
# plots with 'none' diet category taken out. Just marine, mixed, and terrestrial categories
######################################################################################################

# plot without (n = #samples)
ggplot(summary_prey_prop_without_none, aes(x = host, y = prop, fill = diet_category)) +
  geom_col(position = "stack") +
  # geom_text(aes(label = paste0(round(prop, 1), "%")),
  #           position = position_stack(vjust = 0.5),  # center text in each segment
  #           size = 3, color = "white") +   
  
  geom_text(
    aes(label = paste0(n, "\n", round(prop, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3, color = "white" ) +   # tweak text size & color
  
  labs(
    x = "Predator Species",
    y = "Percentage of Individual Samples (%)",
    fill = "Diet Category",
    title = "Q1: Which species forage coasts?"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_flip()  # flip for easier reading (optional)


##### to add numbers of host to plot (n = #samples)
# Calculate totals and create labels
totals <- summary_prey_prop_without_none |> 
  group_by(host) |> 
  summarise(total_n = sum(n))

label_with_totals <- totals |> 
  mutate(host_label = paste0(host, "\n(n = ", total_n, ")")) |> 
  select(host, host_label)

# Join first to add label column, then create factor
summary_prey_prop_without_none_joined <- summary_prey_prop_without_none |> 
  left_join(label_with_totals, by = "host") 


# Plot with new labels on y axis
ggplot(summary_prey_prop_without_none_joined, aes(x = host_label, y = prop, fill = diet_category)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = paste0(n, "\n", round(prop, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3, color = "white"
  ) +
  labs(
    x = "Predator Species",
    y = "Percentage of Individual Samples (%)",
    fill = "Diet Category",
    title = "Q1: Which Species Forage Coasts?"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_flip() +
  theme(axis.title.y = element_blank())

######################################################################################################
# plots with 'none' and 'mixed' diet category taken out. Just marine/mixed and terrestrial categories
######################################################################################################

# plot without (n = #samples)
ggplot(summary_prey_marine_terr_without_none, aes(x = host, y = prop, fill = diet_category)) +
  geom_col(position = "stack") +
  # geom_text(aes(label = paste0(round(prop, 1), "%")),
  #           position = position_stack(vjust = 0.5),  # center text in each segment
  #           size = 3, color = "white") +   
  
  geom_text(
    aes(label = paste0(n, "\n", round(prop, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3, color = "white" ) +   # tweak text size & color
  
  labs(
    x = "Predator Species",
    y = "Percentage of Individual Samples (%)",
    fill = "Diet Category",
    title = "Q1: Which species forage coasts?"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_flip()  # flip for easier reading (optional)


##### to add numbers of host to plot (n = #samples)
# Calculate totals and create labels
totals <- summary_prey_marine_terr_without_none |> 
  group_by(host) |> 
  summarise(total_n = sum(n))

label_with_totals <- totals |> 
  mutate(host_label = paste0(host, "\n(n = ", total_n, ")")) |> 
  select(host, host_label)

# Join first to add label column, then create factor
summary_prey_marine_terr_without_none_joined <- summary_prey_marine_terr_without_none |> 
  left_join(label_with_totals, by = "host") 


# Plot with new labels on y axis
ggplot(summary_prey_marine_terr_without_none_joined, aes(x = host_label, y = prop, fill = diet_category)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = paste0(n, "\n", round(prop, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3, color = "white"
  ) +
  labs(
    x = "Predator Species",
    y = "Percentage of Individual Samples (%)",
    fill = "Diet Category",
    title = "Q1: Which Species Forage Coasts?"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_flip() +
  theme(axis.title.y = element_blank())


#####################################################
# Chi Squared tests and pairwise tables
####################################################

##########################################################################
### Chi squared test of independence for marine, mixed, terrestrial, none
##########################################################################
contingency_table <- grouped_prey_marine_mixed_terr |> 
  # filter(host %in% c("coyote", "bobcat")) |> 
  count(host, diet_category) |> 
  tidyr::pivot_wider(names_from = diet_category, values_from = n, values_fill = 0) |> 
  column_to_rownames("host")

contingency_table

chisq.test(contingency_table)

## pairwise comparisons for chi squared test
# Get unique host species
contingency_table <- contingency_table |> 
  tibble::rownames_to_column(var = "host")
hosts <- unique(contingency_table$host)

# Create empty list to store results
results <- list()

# Loop through all pairwise combinations of host species
for (i in 1:(length(hosts) - 1)) {
  for (j in (i + 1):length(hosts)) {
    # Subset just those two hosts
    pair_data <- contingency_table %>%
      filter(host %in% c(hosts[i], hosts[j])) %>%
      column_to_rownames("host")
    
    # Chi-squared test
    test_result <- chisq.test(pair_data)
    
    # Store result
    results[[paste(hosts[i], "vs", hosts[j])]] <- test_result$p.value
  }
}

# Adjust p-values using Bonferroni correction
adjusted_pvals <- p.adjust(unlist(results), method = "bonferroni")

# Show results
pairwise_results <- data.frame(
  comparison = names(results),
  raw_p = unlist(results),
  adjusted_p = adjusted_pvals
)

print(pairwise_results)
#some p values are NaN because the mule deer and wild boar only have terrestrial diet items


##########################################################################
### Chi squared test of independence for marine/mixed, terrestrial, none
##########################################################################
contingency_table <- grouped_prey_marine_terr |> 
  # filter(host %in% c("coyote", "bobcat")) |> 
  count(host, diet_category) |> 
  tidyr::pivot_wider(names_from = diet_category, values_from = n, values_fill = 0) |> 
  column_to_rownames("host")

contingency_table

chisq.test(contingency_table)

## pairwise comparisons for chi squared test
# Get unique host species
contingency_table <- contingency_table |> 
  tibble::rownames_to_column(var = "host")
hosts <- unique(contingency_table$host)

# Create empty list to store results
results <- list()

# Loop through all pairwise combinations of host species
for (i in 1:(length(hosts) - 1)) {
  for (j in (i + 1):length(hosts)) {
    # Subset just those two hosts
    pair_data <- contingency_table %>%
      filter(host %in% c(hosts[i], hosts[j])) %>%
      column_to_rownames("host")
    
    # Chi-squared test
    test_result <- chisq.test(pair_data)
    
    # Store result
    results[[paste(hosts[i], "vs", hosts[j])]] <- test_result$p.value
  }
}

# Adjust p-values using Bonferroni correction
adjusted_pvals <- p.adjust(unlist(results), method = "bonferroni")

# Show results
pairwise_results <- data.frame(
  comparison = names(results),
  raw_p = unlist(results),
  adjusted_p = adjusted_pvals
)

print(pairwise_results)
#some p values are NaN because the mule deer and wild boar only have terrestrial diet items


##########################################################################
### Chi squared test of independence for marine, mixed, terrestrial. WITHOUT NONE
##########################################################################
contingency_table <- grouped_prey_without_none |> 
  # filter(host %in% c("coyote", "bobcat")) |> 
  count(host, diet_category) |> 
  tidyr::pivot_wider(names_from = diet_category, values_from = n, values_fill = 0) |> 
  column_to_rownames("host")

contingency_table

chisq.test(contingency_table)

## pairwise comparisons for chi squared test
# Get unique host species
contingency_table <- contingency_table |> 
  tibble::rownames_to_column(var = "host")
hosts <- unique(contingency_table$host)

# Create empty list to store results
results <- list()

# Loop through all pairwise combinations of host species
for (i in 1:(length(hosts) - 1)) {
  for (j in (i + 1):length(hosts)) {
    # Subset just those two hosts
    pair_data <- contingency_table %>%
      filter(host %in% c(hosts[i], hosts[j])) %>%
      column_to_rownames("host")
    
    # Chi-squared test
    test_result <- chisq.test(pair_data)
    
    # Store result
    results[[paste(hosts[i], "vs", hosts[j])]] <- test_result$p.value
  }
}

# Adjust p-values using Bonferroni correction
adjusted_pvals <- p.adjust(unlist(results), method = "bonferroni")

# Show results
pairwise_results <- data.frame(
  comparison = names(results),
  raw_p = unlist(results),
  adjusted_p = adjusted_pvals
)

print(pairwise_results)
#some p values are NaN because the mule deer and wild boar only have terrestrial diet items

##########################################################################
### Chi squared test of independence for marine/mixed, terrestrial. WITHOUT NONE or MIXED
##########################################################################
contingency_table <- grouped_prey_marine_terr_without_none |> 
  # filter(host %in% c("coyote", "bobcat")) |> 
  count(host, diet_category) |> 
  tidyr::pivot_wider(names_from = diet_category, values_from = n, values_fill = 0) |> 
  column_to_rownames("host")

contingency_table

chisq.test(contingency_table)

## pairwise comparisons for chi squared test
# Get unique host species
contingency_table <- contingency_table |> 
  tibble::rownames_to_column(var = "host")
hosts <- unique(contingency_table$host)

# Create empty list to store results
results <- list()

# Loop through all pairwise combinations of host species
for (i in 1:(length(hosts) - 1)) {
  for (j in (i + 1):length(hosts)) {
    # Subset just those two hosts
    pair_data <- contingency_table %>%
      filter(host %in% c(hosts[i], hosts[j])) %>%
      column_to_rownames("host")
    
    # Chi-squared test
    test_result <- chisq.test(pair_data)
    
    # Store result
    results[[paste(hosts[i], "vs", hosts[j])]] <- test_result$p.value
  }
}

# Adjust p-values using Bonferroni correction
adjusted_pvals <- p.adjust(unlist(results), method = "bonferroni")

# Show results
pairwise_results <- data.frame(
  comparison = names(results),
  raw_p = unlist(results),
  adjusted_p = adjusted_pvals
)

print(pairwise_results)
#some p values are NaN because the mule deer and wild boar only have terrestrial diet items


######################################################################################
# Question 2: what proportion of diet samples include coastal species?
######################################################################################


#view(grouped_prey_marine_mixed_terr)

counts_prey <- grouped_prey_marine_mixed_terr |> 
  select(-c(marine_present, terrestrial_present, diet_category)) |> 
  column_to_rownames(var = "sample_id") |> 
  group_by(host) |> 
  summarise(across(everything(), sum), .groups = "drop") |> 
  rowwise() |> 
  mutate(total = sum(c_across(where(is.numeric)))) |> 
  filter(total != 0) # take out any if total = 0. IN this case it's mule deer because there's no diet items.

proportions_prey <- counts_prey |> 
  mutate(across(where(is.numeric), ~ .x / total)) |> 
  select(-total) |> 
  filter(host != "mule deer") |> # take out mule deer because it doesn't have any prey items.
  column_to_rownames(var = "host")


prey_levels = c("brandts cormorant", 
                "california sea lion",
                "harbor seal",
                "guadelupe fur seal",
                "american shad",
                "small rodent",
                "brush rabbit",
                "terrestrial bird",
                "wild boar",
                "mule deer",
                "cattle",
                "striped skunk",
                "red fox",
                "bobcat",
                "anura",
                "gilberts skink")

proportions_long <- proportions_prey |> 
  rownames_to_column(var = "host") |> 
  pivot_longer(-host, names_to = "prey", values_to = "proportion") |> 
  filter(proportion != 0) |> 
  mutate(prey = gsub("_", " ", prey)) 
# |> 
#   mutate(prey = factor(prey, levels = prey_levels))

colors <- c("#03045e","#0077b6", "#00b4d8", "#90e0ef", "#caf0f8", #marine (5)
            "#6a040f", "#9d0208", "#d00000", "#dc2f02", "#e85d04", "#f48c06", "#faa307", "#ffba08", "#ffc914", "#edd88a", "#f0e7c7") #terrestrial (11)

colors_coyote <- c("#03045e","#0077b6", "#00b4d8", "#90e0ef", "#caf0f8", #marine (5)
            "#0d0a01", "#0d0a01", "#0d0a01", "#0d0a01", "#0d0a01", "#0d0a01", "#0d0a01", "#0d0a01", "#0d0a01", "#0d0a01", "#0d0a01")
  
names(colors) <- prey_levels

names(colors_coyote) <- prey_levels

#make stacked bar chart
ggplot(proportions_long, aes(x = host, y = proportion, fill = prey)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of Prey Species in Each Predator's Diet",
       x = "Predator Species",
       y = "Proportion",
       fill = "Prey Species") +
  theme_minimal() +
 # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # rotate labels
  geom_text(data = counts_prey, 
            aes(x = host, y = 1.05, label = paste0("Count: ", total)),
            inherit.aes = FALSE, vjust = 0) +
  scale_fill_manual(values = colors)
 # coord_cartesian(ylim = c(0, 1.2))


#### plot with just coyote to see marine prey items for Q3
proportions_long_coyote <- proportions_long |> 
  filter(host == "coyote")

counts_prey_coyote <- counts_prey |> 
  filter(host == "coyote")

#make stacked bar chart
ggplot(proportions_long_coyote, aes(x = host, y = proportion, fill = prey)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of Marine Prey Species in Coyote Diet",
       x = "Predator Species",
       y = "Proportion",
       fill = "Prey Species") +
  theme_minimal() +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # rotate labels
  geom_text(data = counts_prey_coyote, 
            aes(x = host, y = 1.05, label = paste0("Count: ", total)),
            inherit.aes = FALSE, vjust = 0) +
  scale_fill_manual(values = colors_coyote) +
  coord_flip()
# coord_cartesian(ylim = c(0, 1.2))





#################################################################################################################
########## Question 3: what are the most frequently foraged taxa, and how does this vary across consumer taxa?

#make bar chart with proportions of marine diet items per target species.
# grouped bar graph with proportions of each diet item
# include count and percentage.
# 
q3_marine_prey <- grouped_prey_marine_mixed_terr |> 
   filter(diet_category %in% c("marine", "mixed"))
# 
# summary_q3_marine_prey <- q3_marine_prey |> 
#   # count(host) |> 
#   group_by(host) |> 
#   count()
#   
#   mutate(prop = n / sum(n)*100) |> # proportion as percentage
#   ungroup() 


# Get only the prey columns (excluding metadata)
prey_cols <- setdiff(colnames(q3_marine_prey),
                     c("sample_id", "host", "marine_present", "terrestrial_present", "diet_category"))

# Subset data to long format
q3_marine_prey_long <- q3_marine_prey |>
  filter(diet_category %in% c("marine", "mixed")) |> 
  select(host, all_of(prey_cols)) |>
  pivot_longer(
    cols = -host,
    names_to = "prey_species",
    values_to = "present"
  ) |>
  filter(present == 1) |> 
  filter(prey_species %in% marine_species) 
  #mutate(prey_species = case_when(
    #prey_species == 'brandts_cormorant' ~ 'cormorant',
    #TRUE ~ prey_species))

# Count prey occurrences per host
q3_prey_counts <- q3_marine_prey_long |>
  count(host, prey_species)

# Convert to proportions per host
q3_prey_props <- q3_prey_counts |>
  group_by(host) |>
  mutate(prop = n / sum(n)) |>
  ungroup() |> 
  mutate(prey_species_clean = gsub("_", " ", janitor::make_clean_names(as.character(prey_species)))) |>  #makes species names without underscores.
  mutate(host = reorder(host, -prop)) |> #orders host
  mutate(prey_species_ordered = fct_reorder2(prey_species_clean, host, prop, .desc = FALSE)) |>  #orders prey_species 
         # label_text = paste0(prey_species_clean, "\nprop = ", round(prop,2), ", n = ", n)) |> 
  mutate(label_text = paste0(prey_species_clean, "\n",round(prop, 2)*100, "%\n",
                        "n = ", n)) |> 
  group_by(host) %>%
  arrange(desc(prop)) %>%           # sort descending by proportion
  mutate(label_to_show = ifelse(row_number() <= 3, label_text, NA)) %>%
  ungroup()

#make label text
  # label_text = paste0(prey_species_clean, ", prop = ", round(prop,2), " n = ", n)) #make label text

# label_text = paste0(prey_species_clean, ", n = ", n, ", prop = ", scales::percent(prop, accuracy = 0.1))) 



# Plot stacked bar chart of proportions
ggplot(q3_prey_props, aes(x = host, y = prop, fill = prey_species_ordered)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Host", y = "Proportion of Prey", fill = "Prey Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#grouped bar chart
ggplot(q3_prey_props, aes(x = host, y = prop, fill = prey_species_ordered)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Host", y = "Proportion of Prey Presence", fill = "Marine Prey Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_flip()

#species name to right of stacked bar chart
ggplot(q3_prey_props, aes(x = host, y = prop, fill = prey_species_ordered)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = prey_species_ordered), 
            position = position_dodge(width = 0.8), 
            hjust = -0.1,   # label just outside the bar
            size = 3) +     # adjust text size as needed
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Host", y = "Proportion of Prey Presence", fill = "Marine Prey Species") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"    # remove legend
  ) +
  coord_flip() +
  ylim(0, max(q3_prey_props$prop) * 1.15)  # add some space for text


#with proportion and n = 
marine_prey_plot <- ggplot(q3_prey_props, aes(x = host, y = prop, fill = prey_species_ordered)) +
  geom_bar(stat = "identity", width = 1, position = position_dodge(width = 1),
           color = "black") +
  geom_text(aes(label = label_text), 
            position = position_dodge(width = 1), 
            hjust = -0.1, 
            size = 3) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "", y = "Proportion of Marine Prey in Diet", fill = "Marine Prey Species") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  coord_flip() +
  ylim(0, max(q3_prey_props$prop) * 1.2) +
  scale_fill_viridis_d(option = "C")  # "A" = purple → green
  #scale_fill_manual(values = brewer.pal(7, "Blues"))
  # scale_fill_manual(values = c(
  #   "#ADD8E6",  # Sky Blue
  #   "#B0E0E6",  # Powder Blue
  #   "#89CFF0",  # Baby Blue
  #   "#87CEEB",  # Sky Blue alt
  #   "#00B9D3",  # Icy Blue
  #   "#6495ED",  # Cornflower Blue
  #   "#4682B4",  # Steel Blue
  #   "#4169E1",  # Royal Blue
  #   "#0077BE",  # Ocean Blue
  #   "#0044FF",  # Rare Blue
  #   "#0000FF",  # Pure Blue
  #   "#191970"   # Midnight Blue
  # ))
  # 

marine_prey_plot



library(viridis)

# Full 9-color Blues palette
full_palette <- brewer.pal(9, "Blues")

#full_palette <- viridis(9, "Blues")

# Generate 7 colors, avoiding the lightest two
blues <- colorRampPalette(full_palette[3:9])(7)
blues
# 

# 
# blues <- viridis(7, option = "D", direction = -1)  # “C” is blue-green, -1 reverses if needed
# blues

full_palette <- viridis(9, option = "D")
# avoid the lightest two colors
blues_subset <- colorRampPalette(full_palette[9:1])(8)
blues_subset

marine_prey_plot_stacked <- ggplot(q3_prey_props, aes(x = host, y = prop, fill = prey_species_ordered)) +
  geom_bar(stat = "identity", width = 1) +  # stacked by default
  geom_text(aes(label = label_to_show),
            position = position_stack(vjust = 0.5),  # centers labels in each stack
            size = 3,
            color = 'white') +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "", y = "Proportion of Marine Prey in Diet", fill = "Marine Prey Species") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    #legend.position = "none"
  ) +
  #coord_flip() +
  #ylim(0, max(q3_prey_props$prop) * 1.2) +
  scale_fill_manual(values = blues_subset) +
  scale_x_discrete(labels = "Coyote")

marine_prey_plot_stacked

ggsave(plot = marine_prey_plot, filename = "figs/fig2.png", dpi = 600, width = 8, height = 6, units = "in")

ggsave(plot = marine_prey_plot_stacked, filename = "figs/fig2_stacked.png", dpi = 600, width = 8, height = 6, units = "in")

#make a pie chart out of the marine prey plot
#Add labels and filter for visibility
q3_prey_props_labeled <- q3_prey_props %>%
  group_by(host) %>%
  mutate(
    label_text = if_else(
      prop >= 0.05,
      paste0(prey_species_ordered, "\n", round(prop * 100), "% (n=", n, ")"),
      ""  # hide small wedges
    )
  )

marine_prey_pie_faceted <- ggplot(q3_prey_props_labeled, 
                                  aes(x = "", y = prop, fill = prey_species_ordered)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y") +
  facet_wrap(~host) +
  geom_text(aes(label = label_text),
            position = position_stack(vjust = 0.5),
            size = 2.5,
            lineheight = 0.9) +  # tighter lines for species + percent + n
  scale_fill_manual(values = brewer.pal(8, "Blues")) +
  labs(
    fill = "Marine Prey Species"
    #title = "Marine Prey Composition Of Coyote Diet"
  ) +
  theme_void() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )


marine_prey_pie_faceted

ggsave(plot = marine_prey_pie_faceted, filename = "figs/marine_pie.png", dpi = 600, width = 5.5, height = 5.5, units = "in")






# get proportions of all for total proportions of marine and terrestrial prey
# Subset data to long format
q3_all_prey_long <- grouped_prey_marine_mixed_terr |>
  select(host, all_of(prey_cols)) |>
  pivot_longer(
    cols = -host,
    names_to = "prey_species",
    values_to = "present"
  ) |>
  filter(present == 1) 

# Count prey occurrences per host
q3_all_prey_counts <- q3_all_prey_long |>
  count(host, prey_species)

# Convert to proportions per host
q3_all_prey_props <- q3_all_prey_counts |>
  group_by(host) |>
  mutate(prop = n / sum(n)) |>
  ungroup() |> 
  mutate(prey_species_clean = gsub("_", " ", janitor::make_clean_names(as.character(prey_species)))) |>  #makes species names without underscores.
  mutate(host = reorder(host, -prop)) |> #orders host
  mutate(prey_species_ordered = fct_reorder2(prey_species_clean, host, prop, .desc = FALSE), #orders prey_species 
         label_text = paste0(prey_species_clean, ", n = ", n)) #make label text

# q3_all_prey_props_coyote_boar <- q3_all_prey_props |>
#   filter(host %in% c("coyote", "wild boar"))
# 
# #convert from individual prey species to marine/terrestrial groupings
# q3_all_prey_props_coyote_boar <- q3_all_prey_props_coyote_boar |>
#   mutate(marine_or_terr = case_when(
#     prey_species %in% marine_species ~ 'marine',
#     prey_species %in% terrestrial_species ~ "terrestrial",
#     prey_species == "bobcat" ~ "terrestrial",
#     TRUE ~ NA
#   )) |>
#   group_by(host, marine_or_terr) |>
#   summarise(sum_prop = sum(prop),
#             n = sum(n)) |>
#   mutate(label = paste0(marine_or_terr, "\nprop = ", round(sum_prop, 2),
#                         "\n n = ", n))

# do this with only coyote, as wild boar doesn't have any marine diet items now
q3_all_prey_props_coyote <- q3_all_prey_props |>
  filter(host == "coyote")

#convert from individual prey species to marine/terrestrial groupings
q3_all_prey_props_coyote <- q3_all_prey_props_coyote |>
  mutate(marine_or_terr = case_when(
    prey_species %in% marine_species ~ 'marine',
    prey_species %in% terrestrial_species ~ "terrestrial",
    prey_species == "bobcat" ~ "terrestrial",
    TRUE ~ NA
  )) |>
  group_by(host, marine_or_terr) |>
  summarise(sum_prop = sum(prop),
            n = sum(n)) |>
  mutate(label = paste0(marine_or_terr, '\n',round(sum_prop, 2)*100, "%\n",
                        "n = ", n))
  # mutate(label = paste0(marine_or_terr, "\nprop = ", round(sum_prop, 2),
  #                       ", n = ", n))


# plot grouped bar chart
all_prey_plot <- ggplot(q3_all_prey_props_coyote, aes(x = host, y = sum_prop, fill = marine_or_terr)) +
  geom_bar(stat = "identity", width = 1, position = position_dodge(width = 1),
           color = "black") +
  geom_text(aes(label = label),
            position = position_dodge(width = 1),
            hjust = -0.1,
            size = 3) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("marine" = "#2980b9", "terrestrial" = "#dc7633")) +  # blue & brown
  labs(x = "", y = "Proportion of All Prey in Diet", fill = "Prey Species") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  coord_flip() +
  ylim(0, max(q3_all_prey_props_coyote$sum_prop) * 1.2)

all_prey_plot

# plot stacked bar chart
all_prey_plot_stacked <- ggplot(q3_all_prey_props_coyote, aes(x = host, y = sum_prop, fill = marine_or_terr)) +
  geom_bar(stat = "identity", width = 1) +  # stacked by default
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),  # centers labels in each segment
            size = 3) +
  scale_y_continuous(labels = scales::percent_format()) +
  #scale_fill_manual(values = c("marine" = "#2980b9", "terrestrial" = "#dc7633")) +  # blue & brown
  scale_fill_manual(values = c("marine" = "#31688E", "terrestrial" = "#E69F00")) +
  labs(x = "", y = "Proportion of All Prey in Diet", fill = "Prey Species") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
  #coord_flip() +
  #ylim(0, max(q3_all_prey_props_coyote_boar$sum_prop) * 1.2)


all_prey_plot_stacked

both_plots <- all_prey_plot / marine_prey_plot

both_plots <- all_prey_plot / marine_prey_plot +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 14, face = "bold"))

both_plots

both_plots_stacked <- all_prey_plot_stacked + marine_prey_plot_stacked +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 14, face = "bold"))

both_plots_stacked

ggsave(plot = both_plots, filename = "figs/fig2.png", dpi = 600, width = 6, height = 6, units = "in")

ggsave(plot = both_plots_stacked, filename = "figs/both_stacked.png", dpi = 600, width = 6, height = 5, units = "in")





# make a stacked bar chart by site











###############
##### Question 4: Does frequency of coastal foraging change between areas with high vs. low human activity?
gav_coords_sites <- gav_coords |> 
  rename(sample_id = ScatID) |> 
  select(sample_id, site, lon, lat)

# de-duplicate gav_coords. This has 244 observations. Grouped_prey only has 234 observations because I took out ones that are not the target species, weird ones, etc.
gav_coords_unique <- gav_coords_sites %>% distinct(sample_id, .keep_all = TRUE)

# add sites and lat, lon to grouped prey dataframe
grouped_prey_marine_mixed_terr_sites <- grouped_prey_marine_mixed_terr |> 
  left_join(y = gav_coords_unique, by = "sample_id")

######find proportion of diet items from just coyotes between areas with high (COP/NCOS) vs. low (Dangermond) human activity

# make dataframe with just coyote and boar observations
coyote_boar_grouped_prey_marine_mixed_terr_sites <- grouped_prey_marine_mixed_terr_sites |> 
  mutate(new_site = case_when( # make new_site column where COP and NCOS are grouped together
    site == "Coal Oil Point" ~ "COP/NCOS",
    site == "North Campus Open Space" ~ "COP/NCOS",
    TRUE ~ site
  )) |> 
  filter(site != "Hollister") |>  # take out hollister samples, because there's only 3 with no marine diet items
  filter(host %in% c("coyote", "wild boar")) # just take coyote samples

#summarize counts
summary_coyote_boar_grouped_prey_marine_mixed_terr_sites <- coyote_boar_grouped_prey_marine_mixed_terr_sites |> 
  count(new_site, diet_category, host) |> 
  group_by(new_site, host) |> 
  mutate(prop = n / sum(n)*100) |> # proportion as percentage
  ungroup() 






# plot
ggplot(summary_coyote_boar_grouped_prey_marine_mixed_terr_sites, aes(x = new_site, y = prop, fill = diet_category)) +
  geom_col(position = "stack") +

  # 
  
  # geom_text(aes(label = paste0(round(prop, 1), "%")),
  #           position = position_stack(vjust = 0.5),  # center text in each segment
  #           size = 3, color = "white") +   
  
  geom_text(
    aes(label = paste0(n, "\n", round(prop, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3, color = "white" ) +   # tweak text size & color
  
  labs(
    x = "Predator Species",
    y = "Percentage of Individual Samples (%)",
    fill = "Diet Category",
    title = "Q4: Does frequency of coastal foraging change between areas with \nhigh (COP/NCOS) vs. low (Dangermond) human activity?"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_flip()  # flip for easier reading (optional)


###
ggplot(summary_coyote_boar_grouped_prey_marine_mixed_terr_sites,
       aes(x = interaction(new_site, host), y = prop, fill = diet_category)) +
  
  geom_col(position = "stack", width = 0.7) +
  
  geom_text(
    aes(label = paste0(n, "\n", round(prop, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3, color = "white"
  ) +
  
  labs(
    x = "Site + Predator Species",
    y = "Percentage of Individual Samples (%)",
    fill = "Diet Category",
    title = "Q4: Does frequency of coastal foraging change between areas with \nhigh (COP/NCOS) vs. low (Dangermond) human activity?"
  ) +
  
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_flip()
###



### facet wrap with predator
facet_plot <- ggplot(summary_coyote_boar_grouped_prey_marine_mixed_terr_sites,
       aes(x = new_site, y = prop, fill = diet_category)) +
  
  geom_col(position = "stack", width = 0.7) +
  
  geom_text(
    aes(label = paste0(n, "\n", round(prop, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3, color = "black"
  ) +
  
  facet_wrap(~host) +
  
  labs(
    x = "Site",
    y = "Percentage of Individual Samples (%)",
    fill = "Diet Category",
    title = "Q4: Does frequency of coastal foraging change between areas with \nhigh (COP/NCOS) vs. low (Dangermond) human activity?",
    size = 5
  ) +
  
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.title.x = element_blank()
  ) +
  coord_flip()

facet_plot

ggsave(plot = facet_plot, filename = "figs/sites.png", dpi = 600, width = 22, height = 10)


###



##### to add numbers of host to plot
# Calculate totals and create labels
totals <- summary_coyote_boar_grouped_prey_marine_mixed_terr_sites |> 
  group_by(new_site) |> 
  summarise(total_n = sum(n))

label_with_totals <- totals |> 
  mutate(site_label = paste0(new_site, "\n(n = ", total_n, ")")) |> 
  select(new_site, site_label)

# Join first to add label column, then create factor
summary_coyote_boar_grouped_prey_marine_mixed_terr_sites_joined <- summary_coyote_boar_grouped_prey_marine_mixed_terr_sites |> 
  left_join(label_with_totals, by = "new_site") 


# Plot with new labels on y axis
ggplot(summary_coyote_boar_grouped_prey_marine_mixed_terr_sites_joined, aes(x = site_label, y = prop, fill = diet_category)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = paste0(n, "\n", round(prop, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3, color = "white"
  ) +
  labs(
    x = "Site",
    y = "Percentage of Individual Samples (%)",
    fill = "Diet Category",
    title = "Q4: Does frequency of coastal foraging change between areas with \nhigh (COP/NCOS) vs. low (Dangermond) human activity?"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_flip() +
  theme(axis.title.y = element_blank())


########### Q5: how far inland is marine foraging found
### this needs to be updated, if I'm going to use it.

## just coyote at dangermond
# coyote_boar_dangermond_grouped_prey_marine_mixed_terr_sites_noNA <- coyote_boar_grouped_prey_marine_mixed_terr_sites |> 
#   filter(site == "Dangermond") |> 
#   na.omit()
# 
# colorpal <- colorFactor(
#   palette = brewer.pal(4, "Dark2"),
#   domain = coyote_boar_dangermond_grouped_prey_marine_mixed_terr_sites_noNA$diet_category
# )
# 
# map1 <- leaflet(coyote_dangermond_grouped_prey_marine_mixed_terr_sites_noNA) |> 
#   addProviderTiles("CartoDB.Positron") |>   
#   addCircleMarkers(
#     lng = ~lon,
#     lat = ~lat,
#     radius = 1,  # Bigger radius for visibility
#     opacity = 0.7,
#     fillOpacity = 0.7,
#     color = ~colorpal(diet_category),  # assign colors by broadtax
#     popup = ~sample_id
#   ) |> 
#   addPolygons(
#     data = preserve_boundary,
#     fillColor = "transparent",   # transparent fill
#     color = "red",                # red boundary line
#     weight = 2,                   # line thickness
#     opacity = 1,
#     label = "Preserve Boundary") |> 
#   addLegend(
#     position = "bottomright",
#     pal = colorpal,
#     values = ~diet_category,
#     title = "Species",
#     opacity = 1
#   )
# 
# map1
# 
# 
# 
# 
# #### only color in marine and mixed
# colorpal <- colorFactor(
#   #palette = brewer.pal(4, "Dark2"),
#   palette = c('darkgreen', 'blue', 'grey', 'grey'),
#   domain = coyote_dangermond_grouped_prey_marine_mixed_terr_sites_noNA$diet_category
# )
# 
# map2 <- leaflet(coyote_dangermond_grouped_prey_marine_mixed_terr_sites_noNA) |> 
#   addProviderTiles("CartoDB.Positron") |>   
#   addCircleMarkers(
#     lng = ~lon,
#     lat = ~lat,
#     radius = 1,  # Bigger radius for visibility
#     opacity = 0.7,
#     fillOpacity = 0.7,
#     color = ~colorpal(diet_category),  # assign colors by broadtax
#     popup = ~sample_id
#   ) |> 
#   addPolygons(
#     data = preserve_boundary,
#     fillColor = "transparent",   # transparent fill
#     color = "red",                # red boundary line
#     weight = 2,                   # line thickness
#     opacity = 1,
#     label = "Preserve Boundary") |> 
#   addLegend(
#     position = "bottomright",
#     pal = colorpal,
#     values = ~diet_category,
#     title = "Species",
#     opacity = 1
#   )
# 
# map2



###Q5 take 2

q5_grouped_sites <- grouped_prey_marine_mixed_terr_sites |> 
  drop_na(lat) |> 
  mutate(site = case_when(
    site == "Coal Oil Point" ~ "COPR/NCOS",
    site == "North Campus Open Space" ~ "COPR/NCOS",
    TRUE ~ site
  )) |> 
  select(sample_id, host, diet_category, site, lat, lon)

q5_dangermond <- q5_grouped_sites |> 
  filter(site == "Dangermond")

q5_COPR_NCOS <- q5_grouped_sites |> 
  filter(site == "COPR/NCOS")

#download us spatial data
usa <- gadm(country="USA", level=2, path = here("data"))

#make it a sf
usa1 <- st_as_sf(usa)

#filter for SB county
SB_county <- usa1 |> 
  filter(NAME_1 == "California") |> 
  filter(NAME_2 == "Santa Barbara")

plot(SB_county$geometry)



#make it a spatial object (sf)
q5_sf <- st_as_sf(q5_grouped_sites, coords = c("lon", "lat"), crs = 4326)

#dangermond sf
q5_dangermond_sf <- st_as_sf(q5_dangermond, coords = c("lon", "lat"), crs = 4326)

#copr/ncos sf
q5_COPR_NCOS_sf <- st_as_sf(q5_COPR_NCOS, coords = c("lon", "lat"), crs = 4326)

#make buffer around points at Dangermond
buffer_dangermond <- st_buffer(q5_dangermond_sf, dist = 200)

#make buffer around points at COPR/NCOS
buffer_copr_ncos <- st_buffer(q5_COPR_NCOS_sf, dist = 200)

#crop SB county around dangermond buffer
crop_SB_dangermond <- st_crop(x = SB_county, y = buffer_dangermond)

#crop SB county around copr/ncos buffer
crop_SB_copr_ncos <- st_crop(x = SB_county, y = buffer_copr_ncos)



#plot

#dangermond samples only
dangermond <- ggplot() +
  geom_sf(data = crop_SB_dangermond) +
  geom_sf(data = q5_dangermond_sf, aes(color = diet_category, shape = host)) +
  theme_minimal()

dangermond


#COPR/NCOS samples only
copr_ncos <- ggplot() +
  geom_sf(data = crop_SB_copr_ncos) +
  geom_sf(data = q5_COPR_NCOS_sf, aes(color = diet_category, shape = host)) +
  theme_minimal()

copr_ncos


ggplot() +
  geom_sf(data = SB_county) +
  geom_sf(data = q5_COPR_NCOS_sf, aes(color = diet_category, shape = host)) +
  theme_minimal()






###########
# proportion of marine prey at each field site

library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(janitor)
library(scales)
library(tidytext)

coyote_grouped_prey_marine_mixed_terr_sites <- coyote_boar_grouped_prey_marine_mixed_terr_sites |> 
filter(host == 'coyote') |> 
  mutate(site = case_when(
    site == "Coal Oil Point" ~ "Coal Oil Point/North Campus Open Space",
    site == "North Campus Open Space" ~ "Coal Oil Point/North Campus Open Space",
    TRUE ~ site
  ))

# ---- 1) Subset and reshape data to long format ----
prey_cols <- setdiff(names(coyote_grouped_prey_marine_mixed_terr_sites), 
                     c("host", "marine_present", "terrestrial_present", "site", "diet_category", "lon", "lat", "new_site", "sample_id"))

# marine_species <- c("species1", "species2")  # replace with your actual marine species

prey_long_sites <- coyote_grouped_prey_marine_mixed_terr_sites %>%
  filter(marine_present == TRUE) %>%
  select(host, site, all_of(prey_cols)) %>%
  pivot_longer(
    cols = -c(host, site),
    names_to = "prey_species",
    values_to = "present"
  ) %>%
  filter(present == 1) %>%
  filter(prey_species %in% marine_species)

# ---- 2) Count prey occurrences per host and site ----
prey_counts_site <- prey_long_sites %>%
  count(site, host, prey_species)


prey_props_site <- prey_counts_site %>%
  group_by(site, host) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    # clean species names
    prey_species_clean = gsub("_", " ", janitor::make_clean_names(as.character(prey_species))),
    label_text = case_when(
      prey_species_clean %in% c("sea butterfly snail 2", "sea butterfly snail 3") ~ "sea butterfly snail",
      prey_species_clean %in% c("green algae 2", "green algae 3") ~ "green algae",
      TRUE ~ prey_species_clean
    ),
    prey_species_ordered = reorder_within(prey_species_clean, prop, host),
     prey_species_ordered = case_when(
        prey_species_ordered == 'american shad___coyote' ~ 'american shad',
        prey_species_ordered == 'green algae___coyote' ~ 'green algae',
        prey_species_ordered == 'sea butterfly snail___coyote' ~ 'sea butterfly snail',
        prey_species_ordered == 'brandts cormorant___coyote' ~ 'brandts cormorant',
        prey_species_ordered == 'california sea lion___coyote' ~ 'california sea lion',
        prey_species_ordered == 'green algae 2___coyote' ~ 'green algae',
        prey_species_ordered == 'guadelupe fur seal___coyote' ~ 'guadelupe fur seal',
        prey_species_ordered == 'harbor seal___coyote' ~ 'harbor seal',
        prey_species_ordered == 'inflated ark clam___coyote' ~ 'inflated ark clam',
        prey_species_ordered == 'sea butterfly snail 2___coyote' ~ 'sea butterfly snail',
        prey_species_ordered == 'sharpnose anchovy___coyote' ~ 'sharpnose anchovy',
        prey_species_ordered == 'green algae 3___wild boar' ~ 'green algae',
        prey_species_ordered == 'sea butterfly snail 3___wild boar' ~ 'sea butterfly snail',
        prey_species_ordered == 'pelagic cormorant___coyote' ~ 'pelagic cormorant',
        TRUE ~ prey_species_ordered
      ))

# # ---- 2) Calculate cumulative props for label placement ----
# prey_props_site <- prey_props_site %>%
#   group_by(site, host) %>%
#   arrange(host, prey_species_clean) %>%
#   mutate(
#     cumul_prop = cumsum(prop),
#     label_y = cumul_prop - (prop / 2)
#   ) %>%
#   ungroup()

# ---- 3) Plot grouped bars with labels ----
marine_prey_site_plot <- ggplot(prey_props_site, aes(x = host, y = prop, fill = prey_species_ordered)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(
    aes(label = label_text),
    position = position_dodge(width = 0.8),
    vjust = .3,
    hjust = -.2,
    size = 3
  ) +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.3))) +
  labs(
    x = "Host",
    y = "Proportion of Marine Prey in Diet",
    fill = "Marine Prey Species"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  coord_flip() +
  facet_wrap(~ site, scales = "free_y") +
  scale_fill_manual(values = c(
    "#ADD8E6", "#B0E0E6", "#89CFF0", "#87CEEB", "#00B9D3",
    "#6495ED", "#4682B4", "#4169E1", "#0077BE", "#0044FF",
    "#0000FF", "#191970", "blue", "darkblue"
  )) 

marine_prey_site_plot

# ---- 4) Save high-quality figure ----
ggsave(
  filename = "figs/marine_prey_site_plot.png",
  plot = marine_prey_site_plot,
  width = 9,
  height = 6,
  units = "in",
  dpi = 300
)


################ results summary -- P1 #########









######### try and make the proportion plot be coyote divided up by site as well, so there's 3 panels

q3_all_prey_long <- coyote_grouped_prey_marine_mixed_terr_sites |>
  select(host, site, all_of(prey_cols)) |>
  pivot_longer(
    cols = -c(host, site),
    names_to = "prey_species",
    values_to = "present"
  ) |>
  mutate(site = case_when(
    site == "Coal Oil Point/North Campus Open Space" ~ "COPR/NCOS",
    TRUE ~ site
  )) |> 
  filter(present == 1) 

# Count prey occurrences per host
q3_all_prey_counts <- q3_all_prey_long |>
  count(site, host, prey_species)

# Convert to proportions per host
q3_all_prey_props <- q3_all_prey_counts |>
  group_by(host, site) |>
  mutate(prop = n / sum(n)) |>
  ungroup() |> 
  mutate(prey_species_clean = gsub("_", " ", janitor::make_clean_names(as.character(prey_species)))) |>  #makes species names without underscores.
  mutate(host = reorder(host, -prop)) |> #orders host
  mutate(prey_species_ordered = fct_reorder2(prey_species_clean, host, prop, .desc = FALSE), #orders prey_species 
         label_text = paste0(prey_species_clean, ", n = ", n)) #make label text

# q3_all_prey_props_coyote_boar <- q3_all_prey_props |>
#   filter(host %in% c("coyote", "wild boar"))
# 
# #convert from individual prey species to marine/terrestrial groupings
# q3_all_prey_props_coyote_boar <- q3_all_prey_props_coyote_boar |>
#   mutate(marine_or_terr = case_when(
#     prey_species %in% marine_species ~ 'marine',
#     prey_species %in% terrestrial_species ~ "terrestrial",
#     prey_species == "bobcat" ~ "terrestrial",
#     TRUE ~ NA
#   )) |>
#   group_by(host, marine_or_terr) |>
#   summarise(sum_prop = sum(prop),
#             n = sum(n)) |>
#   mutate(label = paste0(marine_or_terr, "\nprop = ", round(sum_prop, 2),
#                         "\n n = ", n))

# do this with only coyote, as wild boar doesn't have any marine diet items now
q3_all_prey_props_coyote <- q3_all_prey_props |>
  filter(host == "coyote")

#convert from individual prey species to marine/terrestrial groupings
q3_all_prey_props_coyote <- q3_all_prey_props_coyote |>
  mutate(marine_or_terr = case_when(
    prey_species %in% marine_species ~ 'marine',
    prey_species %in% terrestrial_species ~ "terrestrial",
    prey_species == "bobcat" ~ "terrestrial",
    TRUE ~ NA
  )) |>
  group_by(host, site, marine_or_terr) |>
  summarise(sum_prop = sum(prop),
            n = sum(n)) |>
  mutate(label = paste0(marine_or_terr, '\n',round(sum_prop, 2)*100, "%\n",
                        "n = ", n))


# plot grouped bar chart
all_prey_plot <- ggplot(q3_all_prey_props_coyote, aes(x = site, y = sum_prop, fill = marine_or_terr)) +
  geom_bar(stat = "identity", width = 1, position = position_dodge(width = 1),
           color = "black") +
  geom_text(aes(label = label),
            position = position_dodge(width = 1),
            hjust = -0.1,
            size = 3) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("marine" = "#31688E", "terrestrial" = "#E69F00")) +  # blue & brown
  #scale_fill_manual(values = c("marine" = "#2980b9", "terrestrial" = "#dc7633")) +  # blue & brown
  labs(x = "", y = "Proportion of All Prey in Diet", fill = "Prey Species") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  coord_flip() +
  ylim(0, max(q3_all_prey_props_coyote$sum_prop) * 1.2)

all_prey_plot

# plot stacked bar chart
all_prey_plot_stacked <- ggplot(q3_all_prey_props_coyote, aes(x = site, y = sum_prop, fill = marine_or_terr)) +
  geom_bar(stat = "identity", width = 0.85) +  # stacked by default
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),  # centers labels in each segment
            size = 3) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("marine" = "#31688E", "terrestrial" = "#E69F00")) + 
  scale_x_discrete(labels = c(
    "COPR/NCOS" = "Suburban",
    "Dangermond" = "Remote"
  )) +
  #scale_fill_manual(values = c("marine" = "#2980b9", "terrestrial" = "#dc7633")) +  # blue & brown
  labs(x = "Site", y = "Percentage of Prey in Coyote Diet", fill = "Prey Species") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
#coord_flip() +
#ylim(0, max(q3_all_prey_props_coyote_boar$sum_prop) * 1.2)


all_prey_plot_stacked

both_plots <- all_prey_plot / marine_prey_plot

both_plots <- all_prey_plot / marine_prey_plot +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 14, face = "bold"))

both_plots

marine_prey_plot_stacked <- marine_prey_plot_stacked +
  labs(y = "Percentage of Marine Prey in Coyote Diet")

both_plots_stacked <- all_prey_plot_stacked + marine_prey_plot_stacked +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 14, face = "bold"))

both_plots_stacked

stacked_and_pie <- all_prey_plot_stacked + marine_prey_pie_faceted +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 14, face = "bold"))

stacked_and_pie

#save plots
ggsave(both_plots_stacked, file = "figs/both_plots_stacked.png", units = "in", width = 7, height = 6, dpi = 600)

ggsave(stacked_and_pie, file = "figs/stacked_and_pie.png", units = "in", width = 6, height = 6, dpi = 600)




# write grouped_prey_marine_mixed_terr_sites a csv.

write_csv(grouped_prey_marine_mixed_terr_sites, file = "data/grouped_prey_sites_host.csv")





