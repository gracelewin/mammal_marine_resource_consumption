
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------ Diet data reformatting for multivariate analysis -----------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------------------------- 1. Set up ------------------------------- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(here)
library(vegan) # package for multivariate stuff

# replace with correct folders
gav_coords <- read_csv(here("metabarcoding_scripts_GL", "outputs", "gaviota_with_coordinates.csv"))

# 244 total samples

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ----------------- 2. finding samples with only host DNA -----------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 162 samples with only the hosts detected (no species other than hosts)

# samples with only host detection
only_hosts <- gav_coords |>
  select(ScatID, host) |> 
  # group by sample
  group_by(ScatID) |> 
  # create a new column called only_host_detection
  # if the sample included any prey items, put "no" (as in, not an "only host" sample)
  # otherwise, put "yes" (as in, yes this is a sample with only host DNA)
  mutate(only_host_detection = case_when(
    any(host == "no") ~ "no",
    TRUE ~ "yes"
  )) |> 
  # filter to only include samples that only had host DNA
  filter(only_host_detection == "yes")

# D.07.24.23.B2: bobcat and wild boar but guess was bobcat?

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------------- 3. creating a metadata data frame -------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This contains all the information for each sample:ScatID, (new) host, and site

# creating a "metadata" data frame
sample_metadata <- gav_coords |> 
  select(ScatID, host, site) |> 
  # group by sample ID
  group_by(ScatID) |> 
  # create a column called new_host
  # within a sample, if any predator shows up, then fill in the predator as the host
  # if no predators show up, then fill in "no host assigned"
  mutate(new_host = case_when(
    any(host == "Coyote") ~ "Coyote",
    any(host == "Wild boar") ~ "Wild boar",
    any(host == "Bobcat") ~ "Bobcat",
    any(host == "Mule deer") ~ "Mule deer",
    any(host == "Puma") ~ "Puma",
    any(host == "no") ~ "no host assigned"
  )) |> 
  # making sure new_host is a factor and releveling the factors
  mutate(new_host = as_factor(new_host)) |> 
  mutate(new_host = fct_relevel(new_host, 
                                "Bobcat", "Coyote", "Mule deer", "Puma", 
                                "Wild boar", "no host assigned")) |> 
  # selecting unique combinations of ScatID, new_host, and site
  select(ScatID, new_host, site) |> 
  unique() |> 
  # add a new column called type
  # if ScatID is in the only_hosts data frame, then fill in "only host ID" because that sample only includes host DNA
  # if not, then fill in "host and prey" because that sample includes host and prey DNA
  mutate(type = case_when(
    ScatID %in% only_hosts$ScatID ~ "only host ID",
    TRUE ~ "host and prey"
    )) |> 
#filtering out weird samples - super far out in the NMDS
  filter(!(ScatID %in% c("D.07.11.23.F1", # wild turkey only
                       "C.08.17.23.C1", # grey fox only
                       "D.06.28.23.C8", # american badger only
                       #"D.07.11.23.C9", # puma only
                       "D.07.13.23.F2", # striped skunk only
                       "D.00.00.22.H5", #cow only
                       'D.00.00.22.H9',#cow only
                       "D.07.24.23.H1",#cow only
                       "D.04.14.22.H2",#cow only
                       "D.04.14.22.H1"#cow only
                       ))) 


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ----------------- 4. creating a wide format data frame ------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# The typical format for community analysis is to have samples as rows, and
# species as columns. This data frame has samples as rows, species as columns,
# and the values are 1/0 for species presence/absence.

# samples_wide_basic just has ScatID and counts of species
samples_wide_basic <- gav_coords |> 
  filter(!(ScatID %in% c("D.07.11.23.F1", # wild turkey only
                         "C.08.17.23.C1", # grey fox only
                         "D.06.28.23.C8", # american badger only
                         #"D.07.11.23.C9", # puma only
                         "D.07.13.23.F2", # striped skunk only
                         "D.00.00.22.H5", #cow only
                         'D.00.00.22.H9',#cow only
                         "D.07.24.23.H1",#cow only
                         "D.04.14.22.H2",#cow only
                         "D.04.14.22.H1"))) |>  # striped skunk only
 
  # select columns of interest
  select(ScatID, curated_species) |> #changed from curated_species to common_name
  # only select unique combinations (sometimes species are detected twice in a scat sample?)
  unique() |> 
  # create a column for occurrence (presence or absence)
  mutate(occurrence = 1) |> 
  # make the data frame wider
  pivot_wider(names_from = curated_species, #changed from curated_species to common_name
              values_from = occurrence) |> 
  # replace all missing values with 0 (no detection of that species in that scat sample)
  mutate(across(where(is.numeric), ~replace_na(., 0))) |> 
  filter(ScatID %in% sample_metadata$ScatID) |> 
  column_to_rownames("ScatID") 

prey_count <- gav_coords |> 
  filter(!(ScatID %in% c("D.07.11.23.F1", # wild turkey only
                         "C.08.17.23.C1", # grey fox only
                         "D.06.28.23.C8", # american badger only
                       #  "D.07.11.23.C9", # puma only
                         "D.07.13.23.F2"))) |>  # striped skunk only
  # select columns of interest
  select(ScatID, curated_species, common_name, order, family, genus, species) |> #changed from curated_species to common_name
  # only select unique combinations (sometimes species are detected twice in a scat sample?)
  unique() |> 
  # create a column for occurrence (presence or absence)
 #  mutate(occurrence = 1) |> 
  # double check host column in gav_coords
  # if the host column is ok in gav_coords, then you don't need to do this left_join (so just take it out)
  left_join(sample_metadata,
            by = "ScatID") |> 
  mutate(new_host = case_when(
    ScatID == "D.00.00.22.H2" ~ "Bobcat",
    TRUE ~ new_host
  ))

# |> 
#   group_by(new_host) |> 
#   summarise(n_common_name = n_distinct(common_name)),
#             n_species = sum())

# prey_count <- gav_coords |> 
#   filter(!(ScatID %in% c("D.07.11.23.F1",  # wild turkey only
#                          "C.08.17.23.C1",  # grey fox only
#                          "D.06.28.23.C8",  # american badger only
#                          # "D.07.11.23.C9",  # puma only
#                          "D.07.13.23.F2"))) |>  # striped skunk only
#   select(ScatID, common_name, order, family, genus, species) |> 
#   distinct(ScatID, common_name, .keep_all = TRUE) |>  # keep one of each prey per scat
#   left_join(sample_metadata, by = "ScatID") |> 
#   group_by(new_host) |> 
#   summarise(n_common_name = n_distinct(common_name), .groups = "drop")


coyote_common_name <- prey_count |> 
  filter(new_host == 'Coyote') |> 
  select(common_name) |> 
  filter(common_name != "coyote") |> 
  drop_na() |> 
  #nrow()
  unique() |> 
  nrow()
coyote_common_name

coyote_order <- prey_count |> 
  filter(new_host == 'Coyote') |> 
  filter(common_name != "coyote") |>
  select(order) |> 
  drop_na() |> 
  unique() |> 
  nrow()
coyote_order

coyote_fam <- prey_count |> 
  filter(new_host == 'Coyote') |> 
  filter(common_name != "coyote") |>
  select(family) |> 
  drop_na() |> 
  unique() |> 
  nrow()
coyote_fam

coyote_genus <- prey_count |> 
  filter(new_host == 'Coyote') |> 
  filter(common_name != "coyote") |>
  select(genus) |> 
  drop_na() |> 
  unique() |> 
  nrow()
coyote_genus

coyote_species <- prey_count |> 
  filter(new_host == 'Coyote') |> 
  filter(common_name != "coyote") |>
  select(species) |> 
  drop_na() |> 
  unique() |> 
  nrow()
coyote_species

#bobcat
bobcat_common_name <- prey_count |> 
  filter(new_host == 'Bobcat') |> 
  select(common_name) |> 
  filter(common_name != "bobcat") |> 
  drop_na() |> 
  #nrow()
  unique() |> 
  nrow()
bobcat_common_name

bobcat_order <- prey_count |> 
  filter(new_host == 'Bobcat') |> 
  filter(common_name != "bobcat") |> 
  select(order) |> 
  drop_na() |> 
  #nrow()
  unique() |> 
  nrow()
bobcat_order

bobcat_fam <- prey_count |> 
  filter(new_host == 'Bobcat') |> 
  filter(common_name != "bobcat") |> 
  select(family) |> 
  drop_na() |> 
  #nrow()
  unique() |> 
  nrow()
bobcat_fam

bobcat_genus <- prey_count |> 
  filter(new_host == 'Bobcat') |> 
  filter(common_name != "bobcat") |> 
  select(genus) |> 
  drop_na() |> 
  #nrow()
  unique() |> 
  nrow()
bobcat_genus

bobcat_species <- prey_count |> 
  filter(new_host == 'Bobcat') |> 
  filter(common_name != "bobcat") |> 
  select(species) |> 
  drop_na() |> 
  #nrow()
  unique() |> 
  nrow()
bobcat_species

#wild boar
boar_common_name <- prey_count |> 
  filter(new_host == 'Wild boar') |> 
  filter(common_name != "wild boar") |> 
  select(common_name) |> 
  drop_na() |> 
  #nrow()
  unique() |> 
  nrow()
boar_common_name

boar_order <- prey_count |> 
  filter(new_host == 'Wild boar') |> 
  filter(common_name != "wild boar") |> 
  select(order) |> 
  drop_na() |> 
  #nrow()
  unique() |> 
  nrow()
boar_order

boar_fam <- prey_count |> 
  filter(new_host == 'Wild boar') |> 
  filter(common_name != "wild boar") |> 
  select(family) |>
  drop_na() |> 
  #nrow()
  unique() |> 
  nrow()
boar_fam

boar_genus <- prey_count |> 
  filter(new_host == 'Wild boar') |> 
  filter(common_name != "wild boar") |> 
  select(genus) |> 
  drop_na() |> 
  #nrow()
  unique() |> 
  nrow()
boar_genus

boar_species <- prey_count |> 
  filter(new_host == 'Wild boar') |> 
  filter(common_name != "wild boar") |> 
  select(species) |> 
  drop_na() |> 
  #nrow()
  unique() |> 
  nrow()
boar_species










prey_count_genus <- gav_coords |> 
  filter(!(ScatID %in% c("D.07.11.23.F1", # wild turkey only
                         "C.08.17.23.C1", # grey fox only
                         "D.06.28.23.C8", # american badger only
                        # "D.07.11.23.C9", # puma only
                         "D.07.13.23.F2"))) |>  # striped skunk only
  # select columns of interest
  select(ScatID, genus) |> #changed from curated_species to common_name
  # only select unique combinations (sometimes species are detected twice in a scat sample?)
  unique() |> 
  # create a column for occurrence (presence or absence)
  #  mutate(occurrence = 1) |> 
  # double check host column in gav_coords
  # if the host column is ok in gav_coords, then you don't need to do this left_join (so just take it out)
  left_join(sample_metadata,
            by = "ScatID") |> 
  group_by(new_host) |> 
  count(genus) 

bobcat_genus <- prey_count_genus |> 
  filter(new_host == 'Bobcat') |> 
  nrow()

coyote_genus <- prey_count_genus |> 
  filter(new_host == 'Coyote') |> 
  nrow()

pig_genus <- prey_count_genus |> 
  filter(new_host == 'Wild boar') |> 
  nrow()

prey_count_family <- gav_coords |> 
  filter(!(ScatID %in% c("D.07.11.23.F1", # wild turkey only
                         "C.08.17.23.C1", # grey fox only
                         "D.06.28.23.C8", # american badger only
                         #"D.07.11.23.C9", # puma only
                         "D.07.13.23.F2"))) |>  # striped skunk only
  # select columns of interest
  select(ScatID, family) |> #changed from curated_species to common_name
  # only select unique combinations (sometimes species are detected twice in a scat sample?)
  unique() |> 
  # create a column for occurrence (presence or absence)
  #  mutate(occurrence = 1) |> 
  # double check host column in gav_coords
  # if the host column is ok in gav_coords, then you don't need to do this left_join (so just take it out)
  left_join(sample_metadata,
            by = "ScatID") |> 
  group_by(new_host) |> 
  count(family) 

prey_count_family |> 
  filter(new_host == 'Coyote') |> 
  nrow()

#Dangermond species counts (prey and predator) by common name
dangermond_prey_count_common_name <- gav_coords |> 
  filter(site == "Dangermond") |> 
  # select columns of interest
  select(ScatID, common_name) |> #change for diff taxa
  # only select unique combinations (sometimes species are detected twice in a scat sample?)
  unique() |> 
  # create a column for occurrence (presence or absence)
  #  mutate(occurrence = 1) |> 
  # double check host column in gav_coords
  # if the host column is ok in gav_coords, then you don't need to do this left_join (so just take it out)
  left_join(sample_metadata,
            by = "ScatID") |> 
  group_by(new_host) |> 
  count(common_name) #change for diff taxa

#Dangermond species counts (prey and predator) by broadtax
dangermond_prey_count_broadtax <- gav_coords |> 
  filter(site == "Dangermond") |> 
  # select columns of interest
  select(ScatID, broadtax) |> #change for diff taxa
  # only select unique combinations (sometimes species are detected twice in a scat sample?)
  unique() |> 
  # create a column for occurrence (presence or absence)
  #  mutate(occurrence = 1) |> 
  # double check host column in gav_coords
  # if the host column is ok in gav_coords, then you don't need to do this left_join (so just take it out)
  left_join(sample_metadata,
            by = "ScatID") |> 
  group_by(new_host) |> 
  count(broadtax) #change for diff taxa



cop_ncos_prey_count <- gav_coords |> 
  filter(site %in% c("Coal Oil Point", "North Campus Open Space"))|>
  # select columns of interest
  select(ScatID, curated_species) |> #changed from curated_species to common_name
  # only select unique combinations (sometimes species are detected twice in a scat sample?)
  unique() |> 
  # create a column for occurrence (presence or absence)
  #  mutate(occurrence = 1) |> 
  # double check host column in gav_coords
  # if the host column is ok in gav_coords, then you don't need to do this left_join (so just take it out)
  left_join(sample_metadata,
            by = "ScatID") |> 
  group_by(new_host) |> 
  count(curated_species) 


#Dangermond coyote plot by common name
dangermond_coyote_prey_count_common_name <- dangermond_prey_count_common_name |> 
  filter(new_host == "Coyote") |> 
  filter(common_name != "coyote") |> #change for diff taxa
  ggplot(aes(y = reorder(common_name, n), #change for diff taxa
             x = n)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Count of prey items",
       y = "Prey Species",
       title = "Coyote Prey Items at Dangermond")
dangermond_coyote_prey_count_common_name

#make prey levels for colors for plot
prey_levels = c("Small rodent",
                "Brandt's cormorant",
                "California sea lion",
                "Brush rabbit",
                "Terrestrial bird",
                "Cattle",
                "Wild boar",
                "Bobcat",
                "Striped skunk",
                "Red fox",
                "Mule deer",
                "Harbor seal",
                "Guadelupe fur seal",
                "Gilbert's skink",
                "Anura",
                "American shad")

# make colors. this is in order of dangermond_coyote_prey_count_broadtax species
colors = c("#0d0a01", "#03045e", "#0077b6", "#0d0a01", "#0d0a01", "#0d0a01", "#0d0a01", "#0d0a01", "#0d0a01", "#0d0a01", "#0d0a01", "#00b4d8", "#90e0ef", "#0d0a01", "#0d0a01", "#caf0f8")

# assign names to colors
names(colors) <- prey_levels

#Dangermond coyote plot by broadtax
dangermond_coyote_prey_count_broadtax <- dangermond_prey_count_broadtax |> 
  filter(new_host == "Coyote") |> 
  filter(broadtax != "Coyote") |> #change for diff taxa
  ggplot(aes(y = reorder(broadtax, n), #change for diff taxa
             x = n,
             fill = broadtax)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Count of prey items",
       y = "Prey Species",
       title = "Coyote Prey Items at Dangermond (Broad Taxonomy Categories)") +
  scale_fill_manual(values = colors) +
  theme(legend.position = "none")
dangermond_coyote_prey_count_broadtax

#Dangermond bobcat plot by common name
dangermond_bobcat_prey_count_common_name <- dangermond_prey_count_common_name |> 
  filter(new_host == "Bobcat") |> 
  filter(common_name != "bobcat") |> #change for diff taxa
  ggplot(aes(y = reorder(common_name, n), #change for diff taxa
             x = n)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Count of prey items",
       y = "Prey Species",
       title = "Bobcat Prey Items at Dangermond")
dangermond_bobcat_prey_count_common_name

#Dangermond bobcat plot by broadtax
dangermond_bobcat_prey_count_broadtax <- dangermond_prey_count_broadtax |> 
  filter(new_host == "Bobcat") |> 
  filter(broadtax != "Bobcat") |> #change for diff taxa
  ggplot(aes(y = reorder(broadtax, n), #change for diff taxa
             x = n)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Count of prey items",
       y = "Prey Species",
       title = "Bobcat Prey Items at Dangermond (Broad Taxonomy Categories)")
dangermond_bobcat_prey_count_broadtax

#samples_wide has ScatID, LabID, site, lat, lon, and counts of species in wide format
samples_wide_curated_species <- gav_coords |> 
  # select columns of interest
  select(ScatID, curated_species) |> #changed from curated_species to common_name, removed host and LabID variable from select
  # only select unique combinations (sometimes species are detected twice in a scat sample?)
  unique() |> 
  # create a column for occurrence (presence or absence)
  mutate(occurrence = 1) |> 
  # make the data frame wider
  pivot_wider(names_from = curated_species, #changed from curated_species to common_name
              values_from = occurrence) |> 
  # replace all missing values with 0 (no detection of that species in that scat sample)
  mutate(across(where(is.numeric), ~replace_na(., 0))) |> 
  filter(ScatID %in% sample_metadata$ScatID) |> 
  column_to_rownames("ScatID")


#samples_wide has ScatID, LabID, site, lat, lon, and counts of species in wide format
samples_wide_common_name <- gav_coords |> 
  # select columns of interest
  select(ScatID, common_name) |> #changed from curated_species to common_name, removed host and LabID variable from select
  # only select unique combinations (sometimes species are detected twice in a scat sample?)
  unique() |> 
  # create a column for occurrence (presence or absence)
  mutate(occurrence = 1) |> 
  # make the data frame wider
  pivot_wider(names_from = common_name, #changed from curated_species to common_name
              values_from = occurrence) |> 
  # replace all missing values with 0 (no detection of that species in that scat sample)
  mutate(across(where(is.numeric), ~replace_na(., 0))) |> 
  filter(ScatID %in% sample_metadata$ScatID) |> 
  column_to_rownames("ScatID")

#samples_wide has ScatID, LabID, site, lat, lon, and counts of species in wide format
samples_wide_broadtax <- gav_coords |> 
  # select columns of interest
  select(ScatID, broadtax) |> #changed from curated_species to common_name, removed host and LabID variable from select
  # only select unique combinations (sometimes species are detected twice in a scat sample?)
  unique() |> 
  # create a column for occurrence (presence or absence)
  mutate(occurrence = 1) |> 
  # make the data frame wider
  pivot_wider(names_from = broadtax, #changed from curated_species to common_name
              values_from = occurrence) |> 
  # replace all missing values with 0 (no detection of that species in that scat sample)
  mutate(across(where(is.numeric), ~replace_na(., 0))) |> 
  filter(ScatID %in% sample_metadata$ScatID) |> 
  column_to_rownames("ScatID")

whattt <- unique(gav_coords$host)






# finding duplicates
# these are scat samples with a prey species detected more than once
# for example: D.06.14.23.B1 has two rows for Peromyscus
dupes <- gav_coords |> 
  select(ScatID, curated_species) |>
  group_by_all() |>
  filter(n() > 1) |>
  ungroup()
# from here: https://www.spsanderson.com/steveondata/posts/2023-07-18/index.html

# how many prey items does each scat sample have in it?
row_totals <- samples_wide_broadtax |> 
  mutate(total = rowSums(across(where(is.numeric)))) |> 
  select(total)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -------------------- 5. basic multivariate analysis ---------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# making a distance matrix based on Jaccard dissimilarity (presence absence)
diet_jaccard <- vegdist(samples_wide_broadtax,
                        method = "jaccard")

# NMDS
diet_nmds <- metaMDS(samples_wide_broadtax,
                     distance = "jaccard")

# taking the NMDS coordinates and joining with metadata data frame
diet_scores <- scores(diet_nmds, display = "sites") |> 
  as_tibble(rownames = "ScatID") |> 
  left_join(sample_metadata, by = "ScatID")

# plot
ggplot(data = diet_scores,
       aes(x = NMDS1,
           y = NMDS2,
           color = new_host,
           shape = site)) +
  geom_point(size = 3,
             alpha = 0.6) +
  theme_minimal() +
  labs(color = "Host",
       shape = "Site")

#####
# trial with gpt code
# 1. Run envfit using the original wide-format presence/absence matrix
# (same used in metaMDS)
diet_fit <- envfit(diet_nmds, samples_wide_broadtax, permutations = 999)

# 2. Extract vectors for significant prey items
vectors <- as.data.frame(diet_fit$vectors$arrows * sqrt(diet_fit$vectors$r))  # scale by fit
vectors$Prey <- rownames(vectors)
vectors$pval <- diet_fit$vectors$pvals
vectors <- vectors |> 
  filter(pval <= 0.05)  # only keep significant vectors (optional)

# 3. Plot with arrows
ggplot() +
  geom_point(data = diet_scores,
             aes(x = NMDS1, y = NMDS2, color = new_host, shape = site),
             size = 3, alpha = 0.6) +
  geom_segment(data = vectors,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "black") +
  geom_text(data = vectors,
            aes(x = NMDS1, y = NMDS2, label = Prey),
            color = "black",
            size = 3,
            vjust = 1.2) +
  theme_minimal() +
  labs(color = "Host", shape = "Site")


#####




# plot nmds with arrows
plot(diet_nmds)
fit <- envfit(diet_nmds, env = samples_wide_broadtax, permutations = 999)
plot(fit, p.max = 0.05, col = "blue")



# does diet differ by host? -- permutational anova, diet composition
adonis2(samples_wide_broadtax ~ new_host,
        data = sample_metadata)

# calculating dispersions -- from center to edge of observations
dispersions <- betadisper(d = diet_jaccard, # uses the distance object
                          group = sample_metadata$new_host)

# checking that dispersions are equal (they are not)
permutest(dispersions)


### export samples_wide_broadtax as csv to do analyses on.
gaviota_pres_abs_broadtax <- samples_wide_broadtax |> 
  tibble::rownames_to_column(var = "SampleID")

gaviota_pres_abs_common_name <- samples_wide_common_name |> 
  tibble::rownames_to_column(var = "SampleID")

gaviota_pres_abs_curated_species <- samples_wide_curated_species |> 
  tibble::rownames_to_column(var = "SampleID")

# make gaviota_pres_abs_broadtax a .csv to use in 4_proportions for Q1  
write_csv(gaviota_pres_abs_broadtax, file = here("metabarcoding_scripts_GL", "outputs", "gaviota_pres_abs_broadtax.csv"))

# make samples_wide_common_name a .csv to use in 4_proportions for Q1  
write_csv(gaviota_pres_abs_common_name, file = here("metabarcoding_scripts_GL", "outputs", "gaviota_pres_abs_common_name.csv"))

# make samples_wide_curated_species a .csv to use in 4_proportions for Q1  
write_csv(gaviota_pres_abs_curated_species, file = here("metabarcoding_scripts_GL", "outputs", "gaviota_pres_abs_curated_species.csv"))

# make prey_count a .csv to use in 4_proportions for Q2
write_csv(prey_count, file = here("metabarcoding_scripts_GL", "outputs", "prey_count.csv"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -------------------- 5. Data Viz ---------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#diet_counts <- samples_wide |> 
  #group_by()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -------------------- 7. Prey combinations ---------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 



# all unique prey combinations
combo_counts <- samples_wide_broadtax |>
  dplyr::count(across(everything()))

# Store prey column names
prey_cols <- colnames(samples_wide_broadtax)

# Create summary of unique prey combinations
prey_summary <- samples_wide_broadtax |>
  rowwise() |>
  mutate(prey_combo = paste(prey_cols[which(c_across(all_of(prey_cols)) == 1)], collapse = ", ")) |>
  ungroup() |>
  count(prey_combo, name = "n_samples") |>
  arrange(desc(n_samples))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Using prey_counts (and site specific ones) to answer Q2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

prey_counts <- gav_coords %>%
  group_by(host, common_name) %>%
  summarise(count = n(), .groups = "drop")


# Add proportions for each predator
# prey_proportions <- prey_count %>%
#   group_by(new_host, common_name) %>%
#   summarise(n_prey = n_distinct(common_name))
#   mutate(proportion = n() / sum(n)) %>%
#   ungroup()
# 
# prey_proportions





########## trying to make nmds plot with arrows in ggplot for question 3




# Fit environmental variables (e.g., prey taxa presence/absence)
fit <- envfit(diet_nmds, samples_wide_broadtax, permutations = 999)

#extract envfit vectors
vectors <- as.data.frame(fit$vectors$arrows * sqrt(fit$vectors$r))  # scale by r for arrow length
vectors$Variable <- rownames(vectors)

# to include only significant vectors:
vectors$pval <- fit$vectors$pvals
vectors <- subset(vectors, pval <= 0.05)

# plot with ggplot
ggplot(data = diet_scores,
       aes(x = NMDS1,
           y = NMDS2,
           color = new_host,
           shape = site)) +
  geom_point(size = 3,
             alpha = 0.6) +
  theme_minimal() +
  geom_segment(data = vectors,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2, color = Variable),
               arrow = arrow(length = unit(0.3, "cm")),
               inherit.aes = FALSE)  +
  geom_text(data = vectors,
            aes(x = NMDS1, y = NMDS2, label = Variable),
            inherit.aes = FALSE,
            hjust = 0.5, vjust = -0.5, size = 3)  +
  theme_minimal() +
  coord_equal() +
  labs(color = "Host", 
       shape = "Site")

#nmds plot and vectors of just coyotes

coyote_diet_scores <- diet_scores |> 
  filter(new_host == "Coyote")

# plot with ggplot
ggplot(data = coyote_diet_scores,
       aes(x = NMDS1,
           y = NMDS2,
           color = site)) +
  geom_point(size = 3,
             alpha = 0.6) +
  theme_minimal() +
  geom_segment(data = vectors,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2, color = Variable),
               arrow = arrow(length = unit(0.3, "cm")),
               inherit.aes = FALSE)  +
  geom_text(data = vectors,
            aes(x = NMDS1, y = NMDS2, label = Variable),
            inherit.aes = FALSE,
            hjust = 0.5, vjust = -0.5, size = 3)  +
  theme_minimal() +
  coord_equal() +
  labs(color = "Host", 
       shape = "Site")




