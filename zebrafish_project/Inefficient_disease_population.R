library(tidyverse)
library(magrittr)

# upload fish data
zdata_formatted <- read.csv("final_formatted_data_v2.csv", na.strings=c("","NA"))

########## Pivot #######
# Pivots text longer (each fish has own row  )
zdata_tidy <- zdata_formatted %>%
  pivot_longer(
    cols = starts_with("Fish_Entry"),
    names_to = "fish",
    names_prefix = "Fish_Entry",
    values_to = "histo_inv_fish",
    values_drop_na = TRUE
  )

######### Disease ##########
disease_columns_mutate <- as.data.frame(zdata_tidy) %>%
  
  # sex column
  filter(str_detect(histo_inv_fish, "[Mm]ale")) %>%
  group_by(CaseId) %>% mutate(fish = row_number()) %>%
  mutate(sex = str_extract(histo_inv_fish, "Female|Male")) %>%
  mutate(fish_id = paste(CaseId, "_", fish, sep = "")) %>%
  
  # pseudoloma neurophilia
  mutate(Pseudoloma_Neurophilia = str_extract(histo_inv_fish, "[Pp]seudoloma|[Mm]icrosporidia|[Mm]icrosporidium|[Mm]icrosporidian|[Mm]icrosporidosis|[Mm]icrosporidiosis|[Xx]enoma|[Pp]seudolomiasis|[Nn]europseudolomiasis|[Mm]icrosporidsiosi|P.n.")) %>%
  mutate(Pseudoloma_Neurophilia = if_else(Pseudoloma_Neurophilia == "[a-z]", 1, 1, 0)) %>%
  
  # pleistophora hyphessobryconis
  mutate(Pleistophora_Hyphessobryconis = str_extract(histo_inv_fish, "[Pp]leistophora| [Hh]yphessobryconis")) %>%
  mutate(Pleistophora_Hyphessobryconis =   if_else(Pleistophora_Hyphessobryconis == "[a-z]", 1, 1, 0)) %>%
  
  # mycobacterium
  mutate(Mycobacterium = str_extract(histo_inv_fish, "[Mm]ycobacterium|[Hh]aemophilum|[Mm]arinum|[Cc]helonae|[Mm]ycobacterial|[Mm]acobacteriosis")) %>%
  mutate(Mycobacterium =   if_else(Mycobacterium == "[a-z]", 1, 1, 0)) %>%
  
  # bacterial infection
  mutate(Bacterial_infection = str_extract(histo_inv_fish, "[Bb]acterial infection")) %>%
  mutate(Bacterial_infection = if_else(Bacterial_infection == "[a-z]", 1, 1, 0)) %>%
  
  # pseudocapillaria tomentosa
  mutate(Pseudocapillaria_tomentosa = str_extract(histo_inv_fish, "[Pp]seudocapillaria|[Tt]omentosa")) %>%
  mutate(Pseudocapillaria_tomentosa = if_else(Pseudocapillaria_tomentosa == "[a-z]", 1, 1, 0)) %>%
  
  # helminth
  mutate(Helminth = str_extract(histo_inv_fish, "[Hh]elminth|[Hh]elminths")) %>%
  mutate(Helminth = if_else(Helminth == "[a-z]", 1, 1, 0)) %>%
  
  # myxidium streisingeri
  mutate(Myxidium_streisingeri = str_extract(histo_inv_fish, "[Mm]yxidium|[Ss]treisingeri")) %>%
  mutate(Myxidium_streisingeri = if_else(Myxidium_streisingeri == "[a-z]", 1, 1, 0)) %>%
  
  # fungal infection
  mutate(Fungal_infection = str_extract(histo_inv_fish, "[Ff]ungal infection")) %>%
  mutate(Fungal_infection = if_else(Fungal_infection == "[a-z]", 1, 1, 0)) %>%
  
  # hepatic megalocytosis
  mutate(Hepatic_meglocytosis = str_extract(histo_inv_fish, "[Hh]epatic megalocytosis|HM")) %>%
  mutate(Hepatic_meglocytosis = if_else(Hepatic_meglocytosis == "[a-z]", 1, 1, 0)) %>%
  
  # oophoritis
  mutate(Oophoritis = str_extract(histo_inv_fish, "[Oo]phoritis")) %>%
  mutate(Oophoritis = if_else(Oophoritis == "[a-z]", 1, 1, 0)) %>%
  
  # nephrocalcinosis
  mutate(Nephrocalcinosis = str_extract(histo_inv_fish, "[Nn]ephrocalcinosis|[Nn]eprhocalcinosis")) %>%
  mutate(Nephrocalcinosis = if_else(Nephrocalcinosis == "[a-z]", 1, 1, 0)) %>%
  
  # gill lesions
  mutate(Gill_lesions = str_extract(histo_inv_fish, "[Gg]ill lesions")) %>%
  mutate(Gill_lesions = if_else(Gill_lesions == "[a-z]", 1, 1, 0)) %>%
  
  # edwardsiella ictaluri
  mutate(Edwardsiella_ictaluri = str_extract(histo_inv_fish, "[Ee]dwardsiella|[Ii]ctaluri")) %>%
  mutate(Edwardsiella_ictaluri = if_else(Edwardsiella_ictaluri == "[a-z]", 1, 1, 0)) %>%
  
  # seminoma
  mutate(Seminoma = str_extract(histo_inv_fish, "[Ss]eminoma")) %>%
  mutate(Seminoma = if_else(Seminoma == "[a-z]", 1, 1, 0)) %>%
  
  # UA
  mutate(UA = str_extract(histo_inv_fish, "[Uu]ltimobranchial [Aa]denoma|[Uu]ltimobranchial [Aa]denocarcinoma")) %>%
  mutate(UA = if_else(UA == "[a-z]", 1, 1, 0)) %>%
  
  # chordoma
  mutate(chordoma = str_extract(histo_inv_fish, "[Cc]hordoma")) %>%
  mutate(chordoma = if_else(chordoma == "[a-z]", 1, 1, 0)) %>%
  
  # Spinal deformity
  mutate(Spinal_deformity = str_extract(histo_inv_fish, "[Ss]pinal deformity")) %>%
  mutate(Spinal_deformity = if_else(Spinal_deformity == "[a-z]", 1, 1, 0)) %>%
  
  # Piscinoodnium pillulare
  mutate(Piscinoodnium_pillulare = str_extract(histo_inv_fish, "[Pp]iscinoodnium pillulare|[Pp]illulare")) %>%
  mutate(Piscinoodnium_pillulare = if_else(Piscinoodnium_pillulare == "[a-z]", 1, 1, 0)) %>%
  
  # Aerocystitis
  mutate(Aerocystitis = str_extract(histo_inv_fish, "[Aa]erocystitis")) %>%
  mutate(Aerocystitis = if_else(Aerocystitis == "[a-z]", 1, 1, 0)) %>%
  
  # Coelomitis
  mutate(Coelomitis = str_extract(histo_inv_fish, "[Cc]oelomitis")) %>%
  mutate(Coelomitis = if_else(Coelomitis == "[a-z]", 1, 1, 0)) %>%
  
  # Splenomegaly
  mutate(Splenomegaly = str_extract(histo_inv_fish, "[Ss]plenomegaly|[Ee]nlarged spleen|[Mm]ylodysplastic|[Mm]ylodysplasia")) %>%
  mutate(Splenomegaly = if_else(Splenomegaly == "[a-z]", 1, 1, 0)) %>%
  
  # Bacilli
  mutate(Bacilli = str_extract(histo_inv_fish, "[Bb]acilli")) %>%
  mutate(Bacilli = if_else(Bacilli == "[a-z]", 1, 1, 0))

str(disease_columns_mutate)

write.csv(disease_columns_mutate, "disease_columns_mutate.csv")

######### Plots ##########
library(corrplot)
# extract sex and disease data 
corrdat <- disease_columns_mutate[,c(77, 79:98)]
str(corrdat)

# test if any columns have all rows equal to 0 
lapply(corrdat[-1], function(x) all(x == 0))
# true for pleistophora, edwardsiella, and piscinoodnium

corrmatrix <- cor(corrdat[,2:21])

par(cex=0.5)
corrplot(corrmatrix, method="color")
