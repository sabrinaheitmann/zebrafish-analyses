library(tidyverse)
library(magrittr)

# upload fish data
zdata_formatted <- read.csv("all_final_formatted_data.csv", na.strings=c("","NA"))

########## Pivot #######
# Pivots text longer (each fish has own row)

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
  
  #filter(str_detect(histo_inv_fish, "[Mm]ale")) %>%
  group_by(CaseId) %>% mutate(fish = row_number()) %>%
  mutate(sex = str_extract(histo_inv_fish, "Female|Male")) %>%
  mutate(fish_id = paste(CaseId, "_", fish, sep = "")) %>%
  
  #Neural pseudolomiasis
  # pseudoloma neurophilia
  mutate(New_PseudolomaNeurophilia = str_extract(histo_inv_fish, "[Pp]seudoloma|[Mm]icrosporidia|[Mm]icrosporidium|P. [Nn]europhilia|[Mm]icrosporidian|[Mm]icrosporidosis|[Mm]icrosporidiosis|[Xx]enoma|[Pp]seudolomiasis|[Nn]europseudolomiasis|[Mm]icrosporidsiosi|P.n.|PN|P.N.")) %>%
  mutate(New_PseudolomaNeurophilia = if_else(New_PseudolomaNeurophilia == "[a-z]", 1, 1, 0)) %>%
  
  # pleistophora hyphessobryconis
  mutate(New_PleistophoraHyphessobryconis = str_extract(histo_inv_fish, "[Pp]leistophora|[Hh]yphessobryconis")) %>%
  mutate(New_PleistophoraHyphessobryconis =   if_else(New_PleistophoraHyphessobryconis == "[a-z]", 1, 1, 0)) %>%
  
  # mycobacterium
  mutate(New_MycobacteriumSpp = str_extract(histo_inv_fish, "[Mm]ycobacterium|[Hh]aemophilum|[Mm]arinum|[Cc]helonae|[Mm]ycobacterial|[Mm]ycobacteriosis")) %>%
  mutate(New_MycobacteriumSpp =   if_else(New_MycobacteriumSpp == "[a-z]", 1, 1, 0)) %>%
  
  # bacterial infection
  mutate(New_BacterialInfectionNonAcidFast = str_extract(histo_inv_fish, "[Bb]acterial infection")) %>%
  mutate(New_BacterialInfectionNonAcidFast = if_else(New_BacterialInfectionNonAcidFast == "[a-z]", 1, 1, 0)) %>%
  
  # pseudocapillaria tomentosa
  mutate(New_PseudocapillariaTomentosa = str_extract(histo_inv_fish, "[Pp]seudocapillaria|[Tt]omentosa|[Nn]ematode|[Ww]orm|[Cc]apiliarid")) %>%
  mutate(New_PseudocapillariaTomentosa = if_else(New_PseudocapillariaTomentosa == "[a-z]", 1, 1, 0)) %>%
  
  # helminth
  mutate(New_OtherHelminths = str_extract(histo_inv_fish, "[Hh]elminth|[Hh]elminths")) %>%
  mutate(New_OtherHelminths = if_else(New_OtherHelminths == "[a-z]", 1, 1, 0)) %>%
  
  # myxidium streisingeri
  mutate(New_MyxidiumStreisingeri = str_extract(histo_inv_fish, "[Mm]yxidium|[Ss]treisingeri|[Mm]yxozoan|[Mm]yxozoa|[Mm]uxidium|[Mm]u[y]xidium")) %>%
  mutate(New_MyxidiumStreisingeri = if_else(New_MyxidiumStreisingeri == "[a-z]", 1, 1, 0)) %>%
  
  # fungal infection
  mutate(New_FungalInfection = str_extract(histo_inv_fish, "[Ff]ungal infection|[Hh]yphae|[Bb]udding|[Yy]east")) %>%
  mutate(New_FungalInfection = if_else(New_FungalInfection == "[a-z]", 1, 1, 0)) %>%
  
  # hepatic megalocytosis
  mutate(New_HepaticMegalocytosis = str_extract(histo_inv_fish, "[Hh]epatic megalocytosis|HM")) %>%
  mutate(New_HepaticMegalocytosis = if_else(New_HepaticMegalocytosis == "[a-z]", 1, 1, 0)) %>%
  
  # oophoritis
  mutate(New_EggAssociatedInflammationOophoritis = str_extract(histo_inv_fish, "[Oo]phoritis|[Ee]gg associated inflammation|EAIF|eaif|Eaif|[Ee]gg associated fibroplasia")) %>%
  mutate(New_EggAssociatedInflammationOophoritis = if_else(New_EggAssociatedInflammationOophoritis == "[a-z]", 1, 1, 0)) %>%
  
  # nephrocalcinosis
  mutate(New_Nephrocalcinosis = str_extract(histo_inv_fish, "[Nn]ephrocalcinosis|[Nn]eprhocalcinosis")) %>%
  mutate(New_Nephrocalcinosis = if_else(New_Nephrocalcinosis == "[a-z]", 1, 1, 0)) %>%
  
  # gill lesions
  mutate(New_GillLesions = str_extract(histo_inv_fish, "[Gg]ill lesions|[Ee]pithelial hyperplasia")) %>%
  mutate(New_GillLesions = if_else(New_GillLesions == "[a-z]", 1, 1, 0)) %>%
  
  # edwardsiella ictaluri
  mutate(New_EdwardsiellaIctaluri = str_extract(histo_inv_fish, "[Ee]dwardsiella|[Ii]ctaluri")) %>%
  mutate(New_EdwardsiellaIctaluri = if_else(New_EdwardsiellaIctaluri == "[a-z]", 1, 1, 0)) %>%
  
  # seminoma
  mutate(New_Seminoma = str_extract(histo_inv_fish, "[Ss]eminoma")) %>%
  mutate(New_Seminoma = if_else(New_Seminoma == "[a-z]", 1, 1, 0)) %>%
  
  # UA
  mutate(New_UltimobranchialAdenomaOrAdenocarcinoma = str_extract(histo_inv_fish, "[Uu]ltimobranchial [Aa]denoma|[Uu]ltimobranchial [Aa]denocarcinoma")) %>%
  mutate(New_UltimobranchialAdenomaOrAdenocarcinoma = if_else(New_UltimobranchialAdenomaOrAdenocarcinoma == "[a-z]", 1, 1, 0)) %>%
  
  # chordoma
  mutate(New_Chordoma = str_extract(histo_inv_fish, "[Cc]hordoma")) %>%
  mutate(New_Chordoma = if_else(New_Chordoma == "[a-z]", 1, 1, 0)) %>%
  
  # Spinal deformity
  mutate(New_SpinalDeformityInHistologicSections = str_extract(histo_inv_fish, "[Ss]pinal deformity|[Kk]yphosis|[Ll]irdisus|[Ss]coliosis")) %>%
  mutate(New_SpinalDeformityInHistologicSections = if_else(New_SpinalDeformityInHistologicSections == "[a-z]", 1, 1, 0)) %>%
  
  # Piscinoodnium pillulare
  mutate(New_PiscinoodiniumPillulare = str_extract(histo_inv_fish, "[Pp]iscinoodnium pillulare|[Pp]illulare")) %>%
  mutate(New_PiscinoodiniumPillulare = if_else(New_PiscinoodiniumPillulare == "[a-z]", 1, 1, 0)) %>%
  
  # Aerocystitis
  mutate(New_AerocystitisEtiologyUnknown = str_extract(histo_inv_fish, "[Aa]erocystitis|[Ss]wim bladder inflammation")) %>%
  mutate(New_AerocystitisEtiologyUnknown = if_else(New_AerocystitisEtiologyUnknown == "[a-z]", 1, 1, 0)) %>%
  
  # Coelomitis
  mutate(New_CoelomitisEtiologyUnknown = str_extract(histo_inv_fish, "[Cc]oelomitis")) %>%
  mutate(New_CoelomitisEtiologyUnknown = if_else(New_CoelomitisEtiologyUnknown == "[a-z]", 1, 1, 0)) %>%
  
  # Splenomegaly
  mutate(New_Splenomegaly = str_extract(histo_inv_fish, "[Ss]plenomegaly|[Ee]nlarged spleen|[Mm]ylodysplastic|[Mm]ylodysplasia|[Mm]yelodysplastic")) %>%
  mutate(New_Splenomegaly = if_else(New_Splenomegaly == "[a-z]", 1, 1, 0)) %>%
  
  # Bacilli
  mutate(New_Bacilli = str_extract(histo_inv_fish, "[Bb]acilli")) %>%
  mutate(New_Bacilli = if_else(New_Bacilli == "[a-z]", 1, 1, 0)) %>%
  
  # Swim Bladder
  mutate(New_SwimBladder = str_extract(histo_inv_fish, "[Ss]wim bladder")) %>%
  mutate(New_SwimBladder = if_else(New_SwimBladder == "[a-z]", 1, 1, 0))

# New columns created:
# sex = Male, Female, or NA
# histo_inv_fish = histopathology data for an individual fish
# fish = fish number within a case
# fish_id = CaseId + fish
# New_"disease" = counts of each disease have been extracted into new column 

write.csv(disease_columns_mutate, "all_final_formatted_data_with_extracted_diseases.csv")