
#### Making a Euler diagram


library(tidyverse) 
library(here)
library(janitor)
library(eulerr)
library(RColorBrewer)


gaviota_pres_abs <- read_csv(here("metabarcoding_scripts_GL", "outputs", "gaviota_pres_abs_common_name.csv"))

# used for Q4
gav_coords <- read_csv(here("metabarcoding_scripts_GL", "outputs", "gaviota_with_coordinates.csv")) |> 
  rename(sample_id = ScatID)


# Define marine species to group
#marine_species <- c("brandts_cormorant", "california_sea_lion", "american_shad", "harbor_seal", "guadelupe_fur_seal")

# #take out "sea_butterfly_snail", "green_algae",
# marine_species <- c("american_shad", "brandts_cormorant", "california_sea_lion", 'pelagic_cormorant',  "guadelupe_fur_seal", "harbor_seal", "clam" , "sharpnose_anchovy")

# define terrestrial species
# take out "fly", "flesh_fly", "brown_dog_tick",
# terrestrial_species <- c("deer_mouse", "anura", "bottas_pocket_gopher", "california_vole", "brush_rabbit", "american_robin", "woodrat", "desert_woodrat", "cattle","montane_vole", "wild_boar", "california_sea_lion", "western_harvest_mouse", "cricket", "dung_beetle", "chicken", "cow", "grey_fox",  "sciuridae","golden_mantled_ground_squirrel", "yellow_bellied_marmot","corvidae", "song_sparrow", "wild_turkey", "california_ground_squirrel", "western_skink", "mule_deer","sparrow", "frog", "cactus_mouse", "spotted_towhee", "southern_grasshopper_mouse", "european_earwig", "scarab_beetle","jerusalem_cricket", "california_pocket_mouse","southern_alligator_lizard", "gopher_snake", "mold","fungi", "townsends_vole", "vole","common_carpet_beetle", "hairy_rove_beetle", "california_quail", "acorn_woodpecker", "downy_woodpecker", "big_eared_woodrat", "striped_skunk","pale_kangaroo_mouse", "gilberts_skink", "american_badger", "harvester_ant","california_scrub_jay", "red_fox", "stonefly", "western_rattlesnake", "house_mouse","black_rat","rat") 

# # for species level only
# terrestrial_species <- c("bottas_pocket_gopher", "california_vole", "brush_rabbit", "american_robin", "desert_woodrat", "cattle", "montane_vole", "wild_boar", "california_sea_lion", "western_harvest_mouse", "dung_beetle", "chicken", "grey_fox", "golden_mantled_ground_squirrel", "yellow_bellied_marmot", "song_sparrow", "wild_turkey", "california_ground_squirrel", "western_skink", "mule_deer", "cactus_mouse", "spotted_towhee", "southern_grasshopper_mouse", "european_earwig", "scarab_beetle","jerusalem_cricket", "california_pocket_mouse","southern_alligator_lizard", "gopher_snake", "townsends_vole","common_carpet_beetle", "hairy_rove_beetle", "california_quail", "acorn_woodpecker", "downy_woodpecker", "big_eared_woodrat", "striped_skunk","pale_kangaroo_mouse", "gilberts_skink", "american_badger", "harvester_ant","california_scrub_jay", "red_fox", "stonefly", "western_rattlesnake", "house_mouse","black_rat", "western_deer_mouse") 




#prey_columns <- make_clean_names(c( "coyote", "deer mouse", "frog", "botta's pocket gopher", "brandt's cormorant", "pelagic cormorant", "california vole", "brush rabbit", "american robin", "woodrat", "domestic cattle", "wild boar", "california sea lion", "western harvest mouse", "cricket", "dung beetle", "chicken", "grey fox", "california ground squirrel", "common raven", "song sparrow", "wild turkey", "skilton's skink", "bobcat", "mule deer", "white-crowned sparrow", "spotted towhee", "earwig", "heerman's kangaroo rat", "california alligator lizard", "gopher snake", "carpet beetle", "rove beetle", "california quail", "american shad", "sharpnose anchovy", "acorn woodpecker", "downy woodpecker", "clam", "striped skunk", "american badger", "harvestor ant", "western scrub jay", "harbor seal", "puma", "guadelupe fur seal", "red fox", "stonefly", "western rattlesnake", "house mouse", "black rat" ))

#for 1% filter
prey_columns <- make_clean_names(c( "coyote", "deer mouse", "botta's pocket gopher", "brandt's cormorant", "pelagic cormorant", "california vole", "brush rabbit", "woodrat", "domestic cattle", "wild boar", "california sea lion", "western harvest mouse", "cricket", "dung beetle", "chicken", "grey fox", "california ground squirrel", "common raven", "song sparrow", "wild turkey", "skilton's skink", "bobcat", "mule deer", "white-crowned sparrow", "earwig", "heerman's kangaroo rat", "california alligator lizard", "gopher snake", "carpet beetle", "california quail", "american shad", "sharpnose anchovy", "acorn woodpecker", "downy woodpecker", "clam", "striped skunk", "american badger", "harvestor ant", "western scrub jay", "harbor seal", "puma", "red fox", "stonefly", "western rattlesnake", "house mouse", "black rat" ))

#terrestrial_species <- make_clean_names(c("coyote", "deer mouse", "frog", "botta's pocket gopher", "california vole", "brush rabbit", "american robin", "woodrat", "domestic cattle", "wild boar",  "western harvest mouse", "cricket", "dung beetle", "chicken", "grey fox", "california ground squirrel", "common raven", "song sparrow", "wild turkey", "skilton's skink", "bobcat", "mule deer", "white-crowned sparrow", "spotted towhee", "earwig", "heerman's kangaroo rat", "california alligator lizard", "gopher snake", "carpet beetle", "rove beetle", "california quail", "acorn woodpecker", "downy woodpecker",  "striped skunk", "american badger", "harvestor ant", "western scrub jay", "puma", "red fox", "stonefly", "western rattlesnake", "house mouse", "black rat" ))

#for 1% filter
terrestrial_species <- make_clean_names(c("coyote", "deer mouse", "botta's pocket gopher", "california vole", "brush rabbit", "woodrat", "domestic cattle", "wild boar",  "western harvest mouse", "cricket", "dung beetle", "chicken", "grey fox", "california ground squirrel", "common raven", "song sparrow", "wild turkey", "skilton's skink", "bobcat", "mule deer", "white-crowned sparrow", "earwig", "heerman's kangaroo rat", "california alligator lizard", "gopher snake", "carpet beetle", "california quail", "acorn woodpecker", "downy woodpecker",  "striped skunk", "american badger", "harvestor ant", "western scrub jay", "puma", "red fox", "stonefly", "western rattlesnake", "house mouse", "black rat" ))

#marine_species <- make_clean_names(c("brandt's cormorant", "pelagic cormorant", "california sea lion","american shad", "sharpnose anchovy", "harbor seal", "guadelupe fur seal", "clam"))

#for 1% filter
marine_species <- make_clean_names(c("brandt's cormorant", "pelagic cormorant", "california sea lion","american shad", "sharpnose anchovy", "harbor seal", "clam")) 



# gav_coords |> filter(!common_name %in% marine_species) |> 
# select(common_name) |> 
# unique() |> 
# filter(!common_name %in% c("coyote", "bobcat")) |> 
# clean_names()

#terrestrial_species <- c("small_rodent", "brush_rabbit", "wild_boar", "cattle", "terrestrial_bird", "grey_fox", "mule_deer", "gilberts_skink", "american_badger", "puma", "striped_skunk", "red_fox", "anura")

# create marine, terrestrial, both diet category
# Create new column 'marine' = 1 if ANY marine species present, else 0
# 

gaviota_pres_abs_host <- read_csv(here("metabarcoding_scripts_GL","outputs", "gaviota_pres_abs_host.csv"))




# gaviota_pres_abs_host <- gaviota_pres_abs |>
#   janitor::clean_names() |>
#   # tibble::rownames_to_column(var = "SampleID") |>
#   rowwise() |>
#   mutate(
#     host = case_when(
#       coyote == 1 ~ 'coyote',
#       puma == 1 ~ 'puma',
#       bobcat == 1 & coyote == 1 ~ 'coyote',
#       coyote == 1 & wild_boar == 1 ~ 'coyote',
#       mule_deer == 1 & coyote == 1 ~ 'coyote',
#       
#       bobcat == 1 & coyote == 0 ~ 'bobcat',
#       wild_boar == 1 & coyote == 0 ~ 'wild boar',
#       mule_deer == 1 & coyote == 0 ~ 'mule deer',
#       cattle == 1 & coyote == 0 ~ 'cattle',
#       TRUE ~ NA_character_
#     )) |> 
#   
#   #make values of species columns 0 if they are the host.
#   mutate(coyote = case_when(
#     host %in% "coyote" ~ 0,
#     TRUE ~ coyote
#   )) |> 
#   mutate(bobcat = case_when(
#     host %in% "bobcat" ~ 0,
#     TRUE ~ bobcat
#   )) |> 
#   mutate(mule_deer = case_when(
#     host %in% "mule deer" ~ 0,
#     TRUE ~ mule_deer
#   )) |> 
#   mutate(wild_boar = case_when(
#     host %in% "wild boar" ~ 0,
#     TRUE ~ wild_boar
#   )) |> 
#   mutate(puma = case_when(
#     host %in% "puma" ~ 0,
#     TRUE ~ puma
#   )) |> 
#   filter(host != "cattle") |> 
#   rename(clam = "inflated_ark_clam")

#write_csv(gaviota_pres_abs_host, file = "data/gaviota_pres_abs_host.csv")

# calculate averages prey items per host

#took out "sea butterfly snail" and "green_algae"
# took out cow, frog, mold, fungi, and rat, "fly", "flesh fly",  "brown dog tick",
# prey_columns <- make_clean_names(c("coyote", "deer mouse", "anura", "botta's pocket gopher", "brandts cormorant", "cormorant", "california vole", "brush rabbit", "american robin", "woodrat", "desert woodrat", "cattle","montane vole", "wild boar", "california sea lion", "western harvest mouse", "cricket", "dung beetle","chicken", "grey fox", "sciuridae","golden-mantled ground squirrel", "yellow-bellied marmot","corvidae", "song sparrow", "wild turkey", "california ground squirrel", "western skink","bobcat", "mule deer","sparrow", "cactus mouse", "spotted towhee", "southern grasshopper mouse", "european earwig", "scarab beetle","jerusalem cricket", "california pocket mouse","southern alligator lizard","gopher snake", "townsend's vole", "vole","common carpet beetle", "hairy rove beetle", "california quail","american shad","sharpnose anchovy", "acorn woodpecker", "downy woodpecker","inflated ark clam", "big-eared woodrat", "striped skunk","pale kangaroo mouse", "gilbert's skink", "american badger", "harvester ant","california scrub jay", "harbor seal", "puma", "guadelupe fur seal","red fox", "stonefly", "western rattlesnake", "house mouse","black rat"))

# #for species only  
# prey_columns <- make_clean_names(c("coyote", "botta's pocket gopher", "brandt's cormorant", "pelagic cormorant", "california vole", "brush rabbit", "american robin", "desert woodrat","western deer mouse", "cattle","montane vole", "wild boar", "california sea lion", "western harvest mouse", "dung beetle","chicken", "grey fox", "golden-mantled ground squirrel", "yellow-bellied marmot", "song sparrow", "wild turkey", "california ground squirrel", "western skink","bobcat", "mule deer", "cactus mouse", "spotted towhee", "southern grasshopper mouse", "european earwig", "scarab beetle","jerusalem cricket", "california pocket mouse","southern alligator lizard","gopher snake", "townsend's vole", "common carpet beetle", "hairy rove beetle", "california quail","american shad","sharpnose anchovy", "acorn woodpecker", "downy woodpecker","clam", "big-eared woodrat", "striped skunk","pale kangaroo mouse", "gilbert's skink", "american badger", "harvester ant","california scrub jay", "harbor seal", "puma","guadelupe fur seal","red fox", "stonefly", "western rattlesnake", "house mouse","black rat"))

#make prey count column in new dataframe
gaviota_pres_abs_prey_count <- gaviota_pres_abs_host %>%
  rowwise() %>%
  mutate(prey_count = sum(c_across(all_of(prey_columns)), na.rm = TRUE)) %>%
  ungroup()

####This only outputs plot, not species lists

# Step 1 — collapse to presence/absence by host
host_pa <- gaviota_pres_abs_prey_count %>%
  select(-sample_id, -prey_count) %>%  # drop sample and count columns
  group_by(host) %>%
  summarise(across(where(is.numeric), ~ as.integer(any(. > 0))), .groups = "drop")

host_pa_total_counts <- host_pa |> 
  rowwise() %>%
  mutate(prey_count = sum(c_across(all_of(prey_columns)), na.rm = TRUE)) %>%
  ungroup()

# Step 2 — turn each host into a vector of species eaten
species_lists <- lapply(1:nrow(host_pa), function(i) {
  prey <- names(host_pa)[-1][host_pa[i, -1] == 1]
  prey
})
names(species_lists) <- host_pa$host

species_lists

#total number of prey species
total_species <- length(unique(unlist(species_lists)))
total_species

#find overlaps and uniques of prey diet items

# Shared between all hosts
shared_all <- Reduce(intersect, species_lists)

# Shared between at least 2 (pairwise)
shared_pairs <- combn(names(species_lists), 2, simplify = FALSE, FUN = function(pair) {
  list(
    hosts = paste(pair, collapse = " & "),
    species = intersect(species_lists[[pair[1]]], species_lists[[pair[2]]])
  )
})

shared_pairs_counts <- data.frame(
  hosts = sapply(shared_pairs, function(x) x$hosts),
  n_shared_species = sapply(shared_pairs, function(x) length(x$species))
)

shared_pairs_counts

# Unique to each host
unique_species <- lapply(names(species_lists), function(h) {
  others <- setdiff(names(species_lists), h)
  unique_to_host <- setdiff(species_lists[[h]], unlist(species_lists[others]))
  list(host = h, species = unique_to_host)
})

# Create a data frame of counts of unique species per host
unique_species_counts <- data.frame(
  host = sapply(unique_species, function(x) x$host),
  n_unique_species = sapply(unique_species, function(x) length(x$species))
)

unique_species_counts

# 4 — Combine into one table
diet_summary <- list(
  unique_species_counts = unique_species_counts,
  shared_pairs_counts = shared_pairs_counts,
  shared_all = total_species
)

diet_summary

# Total number of unique prey species
total_species <- length(unique(unlist(species_lists)))

# 1 — Unique species per host
unique_df <- data.frame(
  hosts = sapply(unique_species, function(x) x$host),
  type = "unique",
  count = sapply(unique_species, function(x) length(x$species))
) %>%
  mutate(percent = round(100 * count / total_species, 1))

# 2 — Pairwise shared species
shared_pairs_df <- data.frame(
  hosts = sapply(shared_pairs, function(x) x$hosts),
  type = "shared_pair",
  count = sapply(shared_pairs, function(x) length(x$species))
) %>%
  mutate(percent = round(100 * count / total_species, 1))

# 3 — Shared by all hosts
shared_all_df <- data.frame(
  hosts = "All hosts",
  type = "shared_all",
  count = length(shared_all),
  percent = round(100 * length(shared_all) / total_species, 1)
)

# 4 — Combine into one dataframe
diet_summary_df <- bind_rows(unique_df, shared_pairs_df, shared_all_df)

diet_summary_df


#Step 2 — convert prey→host matrix to eulerr format
make_set_name <- function(row) {
  hosts <- names(row)[row == 1]
  if (length(hosts) == 0) return("")
  paste(hosts, collapse = "&")
}

# Transpose to prey-by-host
prey_by_host <- t(host_pa[,-1])
colnames(prey_by_host) <- host_pa$host

set_names <- apply(prey_by_host, 1, make_set_name)
counts <- as.numeric(table(set_names))
names(counts) <- names(table(set_names))
counts <- counts[names(counts) != ""]

# Step 3 — plot Euler diagram
fit <- euler(counts)
plot(fit,
     fills = list(fill = c("#FF9999", "#99CCFF", "#99FF99"), alpha = 0.7),
     labels = list(font = 2),
     edges = TRUE)



#trial with one function

base_colors <- brewer.pal(length(species_lists), "Set2")
fills <- sapply(base_colors, function(col) adjustcolor(col, alpha.f = 0.5))

compare_host_diets <- function(diet_wide) {
  
  # 1 — collapse to presence/absence per host
  host_pa <- diet_wide %>%
    select(-sample_id, -prey_count) %>%
    filter(host != "puma") %>% 
    group_by(host) %>%
    summarise(across(where(is.numeric), ~ as.integer(any(. > 0))), .groups = "drop")
  
  # 2 — species list per host
  species_lists <- lapply(1:nrow(host_pa), function(i) {
    prey <- names(host_pa)[-1][host_pa[i, -1] == 1]
    prey <- gsub("_", " ", prey)  # Replace underscores with spaces
    prey
  })
  names(species_lists) <- host_pa$host
  
  # 3 — overlaps and uniques
  shared_all <- Reduce(intersect, species_lists)
  
  shared_pairs <- combn(names(species_lists), 2, simplify = FALSE, FUN = function(pair) {
    list(
      hosts = paste(pair, collapse = " & "),
      species = intersect(species_lists[[pair[1]]], species_lists[[pair[2]]])
    )
  })
  
  unique_species <- lapply(names(species_lists), function(h) {
    others <- setdiff(names(species_lists), h)
    list(
      host = h,
      species = setdiff(species_lists[[h]], unlist(species_lists[others]))
    )
  })
  
  # 4 — prepare eulerr counts
  prey_by_host <- t(host_pa[,-1])
  colnames(prey_by_host) <- host_pa$host
  
  make_set_name <- function(row) {
    hosts <- names(row)[row == 1]
    if (length(hosts) == 0) return("")
    paste(hosts, collapse = "&")
  }
  
  set_names <- apply(prey_by_host, 1, make_set_name)
  counts <- as.numeric(table(set_names))
  names(counts) <- names(table(set_names))
  counts <- counts[names(counts) != ""]
  
  # 5 — plot Euler diagram
  fit <- euler(counts)
  print(plot(fit, fills = fills, labels = list(font = 2), edges = TRUE))
  # print(plot(fit,
  #      fills = list(fill = brewer.pal(length(species_lists), "Set2"),
  #                   alpha = 0.7),
  #      labels = list(font = 2),
  #      edges = TRUE,
  #      main = "Coastal Consumer Diet Overlap"))
  # 
  # 6 — print results
  cat("\n=== Species eaten by each host ===\n")
  for (h in names(species_lists)) {
    cat(h, ":", paste(species_lists[[h]], collapse = ", "), "\n")
  }
  
  cat("\n=== Species eaten by ALL hosts ===\n")
  if (length(shared_all) == 0) {
    cat("None\n")
  } else {
    cat(paste(shared_all, collapse = ", "), "\n")
  }
  
  cat("\n=== Pairwise overlaps ===\n")
  for (pair in shared_pairs) {
    if (length(pair$species) == 0) {
      cat(pair$hosts, ": None\n")
    } else {
      cat(pair$hosts, ":", paste(pair$species, collapse = ", "), "\n")
    }
  }
  
  cat("\n=== Unique species per host ===\n")
  for (u in unique_species) {
    if (length(u$species) == 0) {
      cat(u$host, ": None\n")
    } else {
      cat(u$host, ":", paste(u$species, collapse = ", "), "\n")
    }
  }
  
  # 7 — return results invisibly
  invisible(list(
    species_lists = species_lists,
    shared_all = shared_all,
    shared_pairs = shared_pairs,
    unique_species = unique_species
  ))
}

# run the function

compare_host_diets(gaviota_pres_abs_prey_count)


##### add asterix to host label on figure

compare_host_diets_asterix <- function(diet_wide, marine_species) {
  
  # 1 — collapse to presence/absence per host
  host_pa <- diet_wide %>%
    select(-sample_id, -prey_count) %>%
    filter(host != "puma") %>% 
    group_by(host) %>%
    summarise(across(where(is.numeric), ~ as.integer(any(. > 0))), .groups = "drop")
  
  # 2 — species list per host
  species_lists <- lapply(1:nrow(host_pa), function(i) {
    prey <- names(host_pa)[-1][host_pa[i, -1] == 1]
    gsub("_", " ", prey)  # Replace underscores with spaces
  })
  names(species_lists) <- host_pa$host
  
  # 3 — overlaps and uniques
  shared_all <- Reduce(intersect, species_lists)
  
  shared_pairs <- combn(names(species_lists), 2, simplify = FALSE, FUN = function(pair) {
    list(
      hosts = paste(pair, collapse = " & "),
      species = intersect(species_lists[[pair[1]]], species_lists[[pair[2]]])
    )
  })
  
  unique_species <- lapply(names(species_lists), function(h) {
    others <- setdiff(names(species_lists), h)
    list(
      host = h,
      species = setdiff(species_lists[[h]], unlist(species_lists[others]))
    )
  })
  
  # 4 — prepare eulerr counts
  prey_by_host <- t(host_pa[,-1])
  colnames(prey_by_host) <- host_pa$host
  
  make_set_name <- function(row) {
    hosts <- names(row)[row == 1]
    if (length(hosts) == 0) return("")
    paste(hosts, collapse = "&")
  }
  
  set_names <- apply(prey_by_host, 1, make_set_name)
  counts <- as.numeric(table(set_names))
  names(counts) <- names(table(set_names))
  counts <- counts[names(counts) != ""]
  
  # 5 — identify which hosts have any marine prey
  hosts_with_marine <- sapply(species_lists, function(x) any(x %in% marine_species))
  
  # 6 — modify host labels (add * to those that have marine prey)
  host_labels <- names(hosts_with_marine)
  host_labels_starred <- ifelse(hosts_with_marine,
                                paste0(host_labels, "*"),
                                host_labels)
  
  # map old host names to new ones in count names (so euler labels update)
  for (i in seq_along(host_labels)) {
    old <- host_labels[i]
    new <- host_labels_starred[i]
    names(counts) <- gsub(paste0("\\b", old, "\\b"), new, names(counts))
  }
  
  # 7 — plot Euler diagram
  fit <- euler(counts)
  euler_plot <- plot(
    fit,
    fills = list(fill = RColorBrewer::brewer.pal(length(host_labels_starred), "Set2"), alpha = 0.7),
    labels = list(font = 2),
    edges = TRUE
    #main = "Coastal Consumer Diet Overlap"
  )
  print(euler_plot)
  
  # 8 — print results
  cat("\n=== Species eaten by each host ===\n")
  for (h in names(species_lists)) {
    cat(h, ":", paste(species_lists[[h]], collapse = ", "), "\n")
  }
  
  cat("\n=== Species eaten by ALL hosts ===\n")
  if (length(shared_all) == 0) {
    cat("None\n")
  } else {
    cat(paste(shared_all, collapse = ", "), "\n")
  }
  
  cat("\n=== Pairwise overlaps ===\n")
  for (pair in shared_pairs) {
    if (length(pair$species) == 0) {
      cat(pair$hosts, ": None\n")
    } else {
      cat(pair$hosts, ":", paste(pair$species, collapse = ", "), "\n")
    }
  }
  
  cat("\n=== Unique species per host ===\n")
  for (u in unique_species) {
    if (length(u$species) == 0) {
      cat(u$host, ": None\n")
    } else {
      cat(u$host, ":", paste(u$species, collapse = ", "), "\n")
    }
  }
  
  # 9 — return results invisibly
  invisible(list(
    species_lists = species_lists,
    shared_all = shared_all,
    shared_pairs = shared_pairs,
    unique_species = unique_species,
    hosts_with_marine = hosts_with_marine
  ))
  
  ggsave(plot = euler_plot, filename = here("figs", "euler_saved.png"), dpi = 1200, units = "in", width = 5, height = 5)
  
}

compare_host_diets_asterix(gaviota_pres_abs_prey_count, marine_species)



########### put percentage overlap on figure #########



# compare_host_diets_numbers <- function(diet_wide, marine_species, show_percent = FALSE) {
#   library(eulerr)
#   library(RColorBrewer)
#   library(dplyr)
#   library(grid)
#   library(stringr)
#   
#   # 1 — collapse to presence/absence per host
#   host_pa <- diet_wide %>%
#     select(-sample_id, -prey_count) %>%
#     filter(host != "puma") %>%
#     group_by(host) %>%
#     summarise(across(where(is.numeric), ~ as.integer(any(. > 0))), .groups = "drop")
#   
#   # 2 — species list per host
#   species_lists <- lapply(1:nrow(host_pa), function(i) {
#     prey <- names(host_pa)[-1][host_pa[i, -1] == 1]
#     gsub("_", " ", prey)
#   })
#   names(species_lists) <- host_pa$host
#   
#   # 3 — counts for eulerr
#   prey_by_host <- t(host_pa[,-1])
#   colnames(prey_by_host) <- host_pa$host
#   make_set_name <- function(row) {
#     hosts <- names(row)[row == 1]
#     if (length(hosts) == 0) return("")
#     paste(hosts, collapse = "&")
#   }
#   set_names <- apply(prey_by_host, 1, make_set_name)
#   counts <- as.numeric(table(set_names))
#   names(counts) <- names(table(set_names))
#   counts <- counts[names(counts) != ""]
#   
#   # 4 — asterisk marine hosts
#   hosts_with_marine <- sapply(species_lists, function(x) any(x %in% marine_species))
#   host_labels <- names(hosts_with_marine)
#   host_labels_starred <- ifelse(hosts_with_marine,
#                                 paste0(host_labels, "*"),
#                                 host_labels)
#   for (i in seq_along(host_labels)) {
#     old <- host_labels[i]
#     new <- host_labels_starred[i]
#     names(counts) <- gsub(paste0("\\b", old, "\\b"), new, names(counts))
#   }
#   
#   # 5 — fit Euler
#   fit <- euler(counts)
#   
#   # 6 — draw base diagram
#   plot(
#     fit,
#     fills = list(fill = brewer.pal(length(host_labels_starred), "Set2"), alpha = 0.7),
#     labels = list(font = 2),
#     edges = TRUE
#   )
#   
#   # 7 — get ellipse centers robustly
#   get_region_centroids <- function(fit) {
#     shapes <- fit$ellipses
#     centers <- lapply(shapes, function(s) {
#       if (is.null(s)) return(NULL)
#       # old eulerr (matrix)
#       if (is.matrix(s)) return(c(mean(s[, 1]), mean(s[, 2])))
#       # new eulerr (list with $center)
#       if (!is.null(s$center)) return(as.numeric(s$center))
#       NULL
#     })
#     centers
#   }
#   centers <- get_region_centroids(fit)
#   
#   # 8 — make value labels
#   vals <- data.frame(
#     region = names(fit$original.values),
#     value = as.numeric(fit$original.values)
#   )
#   vals <- vals[!is.na(vals$value), ]
#   vals$label <- if (show_percent) {
#     paste0(round(100 * vals$value / sum(vals$value), 1), "%")
#   } else {
#     as.character(vals$value)
#   }
#   
#   # 9 — overlay text labels
#   centers <- centers[match(vals$region, names(centers))]
#   for (i in seq_along(vals$region)) {
#     pos <- centers[[i]]
#     if (!is.null(pos) && length(pos) == 2) {
#       grid.text(
#         label = vals$label[i],
#         x = unit(pos[1], "npc"),
#         y = unit(pos[2], "npc"),
#         gp = gpar(col = "black", fontsize = 12, fontface = "bold")
#       )
#     }
#   }
#   
#   # 10 — print summary
#   cat("\n=== Hosts with marine prey (asterisked) ===\n")
#   print(hosts_with_marine)
#   
#   invisible(list(
#     euler_fit = fit,
#     label_data = vals,
#     species_lists = species_lists,
#     hosts_with_marine = hosts_with_marine
#   ))
# }
# 
# 
# 
# 
# compare_host_diets_numbers(gaviota_pres_abs_prey_count, marine_species)







######### take off species labels

base_colors <- brewer.pal(length(species_lists), "Set2")
fills <- sapply(base_colors, function(col) adjustcolor(col, alpha.f = 0.5))

compare_host_diets_no_labels <- function(diet_wide) {
  
  # 1 — collapse to presence/absence per host
  host_pa <- diet_wide %>%
    select(-sample_id, -prey_count) %>%
    filter(host != "puma") %>% 
    group_by(host) %>%
    summarise(across(where(is.numeric), ~ as.integer(any(. > 0))), .groups = "drop")
  
  # 2 — species list per host
  species_lists <- lapply(1:nrow(host_pa), function(i) {
    prey <- names(host_pa)[-1][host_pa[i, -1] == 1]
    prey <- gsub("_", " ", prey)  # Replace underscores with spaces
    prey
  })
  names(species_lists) <- host_pa$host
  
  # 3 — overlaps and uniques
  shared_all <- Reduce(intersect, species_lists)
  
  shared_pairs <- combn(names(species_lists), 2, simplify = FALSE, FUN = function(pair) {
    list(
      hosts = paste(pair, collapse = " & "),
      species = intersect(species_lists[[pair[1]]], species_lists[[pair[2]]])
    )
  })
  
  unique_species <- lapply(names(species_lists), function(h) {
    others <- setdiff(names(species_lists), h)
    list(
      host = h,
      species = setdiff(species_lists[[h]], unlist(species_lists[others]))
    )
  })
  
  # 4 — prepare eulerr counts
  prey_by_host <- t(host_pa[,-1])
  colnames(prey_by_host) <- host_pa$host
  
  make_set_name <- function(row) {
    hosts <- names(row)[row == 1]
    if (length(hosts) == 0) return("")
    paste(hosts, collapse = "&")
  }
  
  set_names <- apply(prey_by_host, 1, make_set_name)
  counts <- as.numeric(table(set_names))
  names(counts) <- names(table(set_names))
  counts <- counts[names(counts) != ""]
  
  # 5 — plot Euler diagram
  fit <- euler(counts)
  print(plot(fit, fills = fills, labels = FALSE, edges = TRUE))
  # print(plot(fit,
  #      fills = list(fill = brewer.pal(length(species_lists), "Set2"),
  #                   alpha = 0.7),
  #      labels = list(font = 2),
  #      edges = TRUE,
  #      main = "Coastal Consumer Diet Overlap"))
  # 
  # 6 — print results
  cat("\n=== Species eaten by each host ===\n")
  for (h in names(species_lists)) {
    cat(h, ":", paste(species_lists[[h]], collapse = ", "), "\n")
  }
  
  cat("\n=== Species eaten by ALL hosts ===\n")
  if (length(shared_all) == 0) {
    cat("None\n")
  } else {
    cat(paste(shared_all, collapse = ", "), "\n")
  }
  
  cat("\n=== Pairwise overlaps ===\n")
  for (pair in shared_pairs) {
    if (length(pair$species) == 0) {
      cat(pair$hosts, ": None\n")
    } else {
      cat(pair$hosts, ":", paste(pair$species, collapse = ", "), "\n")
    }
  }
  
  cat("\n=== Unique species per host ===\n")
  for (u in unique_species) {
    if (length(u$species) == 0) {
      cat(u$host, ": None\n")
    } else {
      cat(u$host, ":", paste(u$species, collapse = ", "), "\n")
    }
  }
  
  # 7 — return results invisibly
  invisible(list(
    species_lists = species_lists,
    shared_all = shared_all,
    shared_pairs = shared_pairs,
    unique_species = unique_species
  ))
}

# run the function

compare_host_diets_no_labels(gaviota_pres_abs_prey_count)



#trial making it a tidy table format

# Convert presence/absence to long format
# For each prey, create a key with hosts where presence==1, e.g. "Boar&Coyote"
# prey_host_pa <- gaviota_pres_abs_prey_count %>%
#   select(-sample_id, -prey_count) %>%
#   rowwise() %>%
#   mutate(
#     hosts_present = paste(sort(names(cur_data())[which(c_across(-host) == 1)]), collapse = "&")
#   ) %>%
#   ungroup()
#   
#   # Group prey by their unique host combination
# prey_groups <- prey_host_pa %>%
#     group_by(hosts_present) %>%
#     summarise(prey_species_list = list(prey_species), .groups = "drop")








############ make table for all prey species, by host ########


library(tidyverse)
library(here)
library(janitor)
library(gt)
library(flextable)
library(officer)



# === Count and percent frequency per host/prey ===
prey_counts <- gaviota_pres_abs_prey_count %>%
  select(-prey_count, -sample_id) %>%
  pivot_longer(-host, names_to = "prey_species", values_to = "present") %>%
  filter(present == 1) %>%
  # categorize BEFORE replacing underscores
  mutate(prey_category = case_when(
    prey_species %in% marine_species ~ "Marine",
    prey_species %in% terrestrial_species ~ "Terrestrial",
    prey_species == "bobcat" ~ "Terrestrial",
    TRUE ~ "Unknown"
  )) %>%
  # replace underscores with spaces for display
  mutate(prey_species = gsub("_", " ", prey_species)) %>%
  group_by(host, prey_species, prey_category) %>%
  summarise(count = n(), .groups = "drop") %>%
  # add total diet items per host for percent calculation
  group_by(host) %>%
  mutate(total_diet_items = sum(count),
         percent_diet_items = round(100 * count / total_diet_items, 1)) %>%
  ungroup() %>%
  filter(host != "puma")



# # === Compute subtotals by host and prey_category ===
# subtotals <- prey_counts %>%
#   group_by(host, prey_category) %>%
#   reframe(
#     prey_species = paste0(prey_category, " subtotal"),
#     count = sum(count),
#     total_diet_items = first(total_diet_items),
#     percent_diet_items = NA_real_  # leave blank for subtotal
#   ) |> 
#   unique()

# === Compute subtotals by host and prey_category ===
subtotals <- prey_counts %>%
  group_by(host, prey_category) %>%
  summarise(
    count = sum(count),
    total_diet_items = first(total_diet_items),
    percent_diet_items = round(100 * count / first(total_diet_items), 1),
    .groups = "drop"
  ) %>%
  mutate(prey_species = paste0(prey_category, " subtotal")) %>%
  unique()

# === Add total sample counts to host labels ===
host_sample_counts <- gaviota_pres_abs_prey_count %>%
  count(host) %>%
  rename(total_samples = n)

# === Combine detailed prey + subtotals ===
prey_full <- bind_rows(prey_counts, subtotals) %>%
  group_by(host) %>%
  arrange(
    # Regular prey species first (not containing "subtotal")
    grepl("subtotal", prey_species, ignore.case = TRUE),
    desc(percent_diet_items),
    prey_category,
    prey_species,
    .by_group = TRUE
  ) %>%
  ungroup() %>%
  arrange(factor(host, levels = c("coyote", "bobcat", "wild boar"))) %>%
  left_join(host_sample_counts, by = "host") |> 
  mutate(host_label = paste0(str_to_title(host), " (n = ", total_samples, ")")) |> 
  select(-total_samples)


# # === Combine detailed prey + subtotals ===
# prey_full <- bind_rows(prey_counts, subtotals) %>%
#   group_by(host) %>%
#   arrange(desc(percent_diet_items), prey_category, prey_species, .by_group = TRUE) %>%
#   ungroup() %>%
#   arrange(factor(host, levels = c("coyote", "bobcat", "wild boar"))) %>%
#   left_join(host_sample_counts, by = "host") |> 
#   mutate(host_label = paste0(str_to_title(host), " (n = ", total_samples, ")")) |> 
#   select(-total_samples)


# === Publication-ready gt table ===
prey_gt <- prey_full %>%
  select(-host) |> 
 # mutate(host = str_to_title(host) %>%
  gt(groupname_col = "host_label") %>%
  cols_label(
    prey_species = md("*Prey species*"),
    prey_category = md("*Category*"),
    count = md("*Count*"),
    percent_diet_items = md("*% of diet items*")
  ) %>%
  tab_header(
    title = md("**Predator–Prey Relationships at Gaviota**"),
    subtitle = "Prey detected in scats by species, category, count, and frequency"
  ) %>%
  tab_options(
    table.font.names = "Helvetica",
    table.font.size = 12,
    heading.align = "center",
    heading.title.font.weight = "bold",
    heading.background.color = "white",
    data_row.padding = px(6),
    row_group.font.weight = "bold",
    row_group.background.color = "#f9f9f9",
    column_labels.font.weight = "bold",
    table.border.top.width = 0,
    table.border.bottom.width = 0
  ) %>%
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_body(columns = "prey_species")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = grepl("subtotal", prey_species, ignore.case = TRUE)
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "gray80",
      weight = px(1)
    ),
    locations = cells_row_groups()
  )

# === Save table for publication ===
gtsave(prey_gt, filename = here("figs", "gaviota_prey_table_clean.png"), vwidth = 1200, vheight = 900)
gtsave(prey_gt, filename = here("figs", "gaviota_prey_table_clean.pdf"))

# View table
prey_gt


####### now make table with site included#######################

# take out "fly", "flesh fly", "brown dog tick",
# prey_columns <- make_clean_names(c("coyote", "deer mouse", "anura", "botta's pocket gopher", "brandt's cormorant", "cormorant", "california vole", "brush rabbit", "american robin", "woodrat", "desert woodrat", "cattle","montane vole", "wild boar", "california sea lion", "western harvest mouse", "cricket", "dung beetle","chicken", "grey fox",  "sciuridae","golden-mantled ground squirrel", "yellow-bellied marmot", "corvidae", "song sparrow", "wild turkey", "california ground squirrel", "western skink","bobcat", "mule deer","sparrow", "cactus mouse", "spotted towhee", "southern grasshopper mouse", "european earwig", "scarab beetle","jerusalem cricket", "california pocket mouse","southern alligator lizard","gopher snake", "townsend's vole", "vole","common carpet beetle", "hairy rove beetle", "california quail","american shad","sharpnose anchovy", "acorn woodpecker", "downy woodpecker","clam", "big-eared woodrat", "striped skunk","pale kangaroo mouse", "gilbert's skink", "american badger", "harvester ant","california scrub jay", "harbor seal", "puma", "guadelupe fur seal","red fox",  "stonefly", "western rattlesnake", "house mouse","black rat"))

# #for species only  
# prey_columns <- make_clean_names(c("coyote", "botta's pocket gopher", "brandt's cormorant", "pelagic cormorant", "california vole", "brush rabbit", "american robin", "desert woodrat","western deer mouse", "cattle","montane vole", "wild boar", "california sea lion", "western harvest mouse", "dung beetle","chicken", "grey fox", "golden-mantled ground squirrel", "yellow-bellied marmot", "song sparrow", "wild turkey", "california ground squirrel", "western skink","bobcat", "mule deer", "cactus mouse", "spotted towhee", "southern grasshopper mouse", "european earwig", "scarab beetle","jerusalem cricket", "california pocket mouse","southern alligator lizard","gopher snake", "townsend's vole", "common carpet beetle", "hairy rove beetle", "california quail","american shad","sharpnose anchovy", "acorn woodpecker", "downy woodpecker","clam", "big-eared woodrat", "striped skunk","pale kangaroo mouse", "gilbert's skink", "american badger", "harvester ant","california scrub jay", "harbor seal", "puma","guadelupe fur seal","red fox", "stonefly", "western rattlesnake", "house mouse","black rat"))



gav_raw <- read_csv(here("metabarcoding_scripts_GL","outputs", "grouped_prey_sites_host.csv"))

gav_host_site <- gav_raw |> 
  select(-marine_present, -terrestrial_present, -diet_category) |> 
  filter(site != "Hollister") |> 
  mutate(site = case_when(
    site == 'Coal Oil Point' ~ 'Coal Oil Point/North Campus Open Space',
    site == 'North Campus Open Space' ~ 'Coal Oil Point/North Campus Open Space',
    TRUE ~ site
  )) |> 
  filter(host != "puma", 
         host != "mule deer")
  #select(-sea_butterfly_snail, -green_algae)

gaviota_prey_count_site <- gav_host_site %>%
  rowwise() %>%
  mutate(prey_count = sum(c_across(all_of(prey_columns)), na.rm = TRUE)) %>%
  ungroup()


prey_counts_site <- gaviota_prey_count_site %>%
  select(-prey_count, -sample_id, -lat, -lon,) %>%  # <-- keep site!
  pivot_longer(-c(host, site), names_to = "prey_species", values_to = "present") %>%
  filter(present == 1) %>%
  mutate(prey_category = case_when(
    prey_species %in% marine_species ~ "Marine",
    prey_species %in% terrestrial_species ~ "Terrestrial",
    prey_species == "bobcat" ~ "Terrestrial",
    TRUE ~ "Unknown"
  )) %>%
  mutate(prey_species = gsub("_", " ", prey_species)) %>%
  group_by(site, host, prey_species, prey_category) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(site, host) %>%
  mutate(total_diet_items = sum(count),
         percent_diet_items = round(100 * count / total_diet_items, 1)) %>%
  ungroup()
  # mutate(prey_species = case_when(
  #   prey_species == "cormorant" ~ "other cormorant sp.",
  #   prey_species == "brandts cormorant" ~ "Brandt's cormorant",
  #   TRUE ~ prey_species))


# Update subtotals and sample counts, group by site
subtotals_site <- prey_counts_site %>%
  distinct(site, host, prey_species, prey_category, count, total_diet_items) %>%
  group_by(site, host, prey_category) %>%
  summarise(
    count = sum(count, na.rm = TRUE),
    # use total_diet_items directly from existing data
    total_diet_items = first(total_diet_items),
    percent_diet_items = round(100 * count / first(total_diet_items), 1),
    .groups = "drop"
  ) %>%
  mutate(prey_species = paste0(prey_category, " subtotal"))


# === Add total sample counts (by site and host) ===
host_sample_counts_site <- gav_host_site %>%
  distinct(site, host, sample_id) %>%
  count(site, host, name = "total_samples")


# prey_full_site <- bind_rows(prey_counts_site, subtotals_site) %>%
#   group_by(site, host) %>%
#   arrange(
#     # Regular prey species first (not containing "subtotal")
#     grepl("subtotal", prey_species, ignore.case = TRUE),
#     desc(percent_diet_items),
#     prey_category,
#     prey_species,
#     .by_group = TRUE
#   ) %>%
#   ungroup() %>%
#   left_join(host_sample_counts_site, by = c("site", "host")) %>%
#   mutate(
#     site = factor(site, levels = c("Coal Oil Point/North Campus Open Space", "Dangermond")),  # site order
#     host = factor(host, levels = c("coyote", "bobcat", "wild boar")),                        # host order
#     host_label = paste0(str_to_title(host), " (n = ", total_samples, ")"),
#     site_label = str_to_title(site),
#     group_label = paste0(site_label, " — ", host_label)
#   ) |> 
#   arrange(site, host, desc(percent_diet_items))

prey_full_site <- bind_rows(prey_counts_site, subtotals_site) %>%
  left_join(host_sample_counts_site, by = c("site", "host")) %>%
  mutate(
    site = factor(site, levels = c("Coal Oil Point/North Campus Open Space", "Dangermond")),
    host = factor(host, levels = c("coyote", "bobcat", "wild boar")),
    host_label = paste0(str_to_title(host), " (n = ", total_samples, ")"),
    site_label = str_to_title(site),
    group_label = paste0(site_label, " — ", host_label),
    is_subtotal = grepl("subtotal", prey_species, ignore.case = TRUE)
  ) %>%
  arrange(
    site,
    host,
    is_subtotal,               # FALSE first, TRUE (subtotal) last
    desc(percent_diet_items),
    prey_category,
    prey_species
  ) %>%
  select(-is_subtotal)  # optional cleanup


# make gt table
prey_gt_site <- prey_full_site %>%
  # keep Predator as a column AND create group label for top row
  mutate(
    Predator = str_to_title(host),  # column for Predator in table
    group_label = paste0(site_label, " — ", Predator, " (", total_samples, "  Samples, ", total_diet_items, " Diet Items)")
  ) %>%
  select(
    group_label,      # this becomes the row group
    Predator,         # column in table
    `Prey species` = prey_species,
    `Diet category` = prey_category,
    Count = count,
    #`Total Diet Items` = total_diet_items,
    '% of Total Diet Items' = percent_diet_items,
    Site = site_label
  ) %>%
  gt(groupname_col = "group_label") %>%
  cols_label(
    Predator = "Predator"
  ) %>%
  tab_header(
    title = md("**Table 1: List of All Diet Items Found in Scat Samples**"),
    subtitle = "Prey detected in scats by species, diet category, and site"
  ) %>%
  tab_options(
    table.font.names = "Helvetica",
    table.font.size = 12,
    heading.align = "center",
    heading.title.font.weight = "bold",
    heading.background.color = "white",
    data_row.padding = px(6),
    row_group.font.weight = "bold",
    row_group.background.color = "#f9f9f9",
    column_labels.font.weight = "bold",
    table.border.top.width = 0,
    table.border.bottom.width = 0
  ) %>%
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_body(columns = "Prey species")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = grepl("subtotal", `Prey species`, ignore.case = TRUE)
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "gray80",
      weight = px(1)
    ),
    locations = cells_row_groups()
  ) |> 
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = c("Count", "% of Total Diet Items"))
  )


prey_gt_site

# === Save table for publication ===
gtsave(prey_gt_site, filename = here("figs", "gaviota_prey_table_sites.png"), vwidth = 1200, vheight = 900)
gtsave(prey_gt_site, filename = here("figs", "gaviota_prey_table_sites.pdf"))
gtsave(prey_gt_site, filename = here("figs", "gaviota_prey_table_sites.docx"))



########### make table that has scientific name too

#first do same processing to the curated species too.

# gav_pres_abs_curated_sp <- read_csv(here('metabarcoding_scripts_GL','gaviota_pres_abs_curated_species.csv'))
# 
# gaviota_pres_abs_curated_sp_host <- gav_pres_abs_curated_sp |>
#   #janitor::clean_names() |>
#   # tibble::rownames_to_column(var = "SampleID") |>
#   rowwise() |>
#   mutate(
#     host = case_when(
#       `Canis latrans` == 1 ~ 'coyote',
#       `Puma concolor` == 1 ~ 'puma',
#       `Lynx rufus` == 1 & `Canis latrans` == 1 ~ 'coyote',
#       `Canis latrans` == 1 & `Sus scrofa` == 1 ~ 'coyote',
#       `Odocoilius hemionus` == 1 & `Canis latrans` == 1 ~ 'coyote',
#       
#       bobcat == 1 & `Canis latrans` == 0 ~ 'bobcat',
#       `Sus scrofa` == 1 & `Canis latrans` == 0 ~ 'wild boar',
#       `Odocoilius hemionus` == 1 & `Canis latrans` == 0 ~ 'mule deer',
#       cattle == 1 & `Canis latrans` == 0 ~ 'cattle',
#       TRUE ~ NA_character_
#     )) |> 
#   
#   #make values of species columns 0 if they are the host.
#   mutate(`Canis latrans` = case_when(
#     host %in% "coyote" ~ 0,
#     TRUE ~ `Canis latrans`
#   )) |> 
#   mutate(`Lynx rufus` = case_when(
#     host %in% "bobcat" ~ 0,
#     TRUE ~ `Lynx rufus`
#   )) |> 
#   mutate(mule_deer = case_when(
#     host %in% "mule deer" ~ 0,
#     TRUE ~ mule_deer
#   )) |> 
#   mutate(wild_boar = case_when(
#     host %in% "wild boar" ~ 0,
#     TRUE ~ wild_boar
#   )) |> 
#   mutate(puma = case_when(
#     host %in% "puma" ~ 0,
#     TRUE ~ puma
#   )) |> 
#   filter(host != "cattle")
# 
# 
# 

