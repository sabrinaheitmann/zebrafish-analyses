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
  mutate(New_PseudolomaNeurophilia = str_extract(histo_inv_fish, "[Pp]seudoloma|[Mm]icrosporidia|[Mm]icrosporidium|[Mm]icrosporidian|[Mm]icrosporidosis|[Mm]icrosporidiosis|[Xx]enoma|[Pp]seudolomiasis|[Nn]europseudolomiasis|[Mm]icrosporidsiosi|P.n.")) %>%
  mutate(New_PseudolomaNeurophilia = if_else(New_PseudolomaNeurophilia == "[a-z]", 1, 1, 0)) %>%
  
  # pleistophora hyphessobryconis
  mutate(New_PleistophoraHyphessobryconis = str_extract(histo_inv_fish, "[Pp]leistophora| [Hh]yphessobryconis")) %>%
  mutate(New_PleistophoraHyphessobryconis =   if_else(New_PleistophoraHyphessobryconis == "[a-z]", 1, 1, 0)) %>%
  
  # mycobacterium
  mutate(New_MycobacteriumSpp = str_extract(histo_inv_fish, "[Mm]ycobacterium|[Hh]aemophilum|[Mm]arinum|[Cc]helonae|[Mm]ycobacterial|[Mm]acobacteriosis")) %>%
  mutate(New_MycobacteriumSpp =   if_else(New_MycobacteriumSpp == "[a-z]", 1, 1, 0)) %>%
  
  # bacterial infection
  mutate(New_BacterialInfectionNonAcidFast = str_extract(histo_inv_fish, "[Bb]acterial infection")) %>%
  mutate(New_BacterialInfectionNonAcidFast = if_else(New_BacterialInfectionNonAcidFast == "[a-z]", 1, 1, 0)) %>%
  
  # pseudocapillaria tomentosa
  mutate(New_PseudocapillariaTomentosa = str_extract(histo_inv_fish, "[Pp]seudocapillaria|[Tt]omentosa")) %>%
  mutate(New_PseudocapillariaTomentosa = if_else(New_PseudocapillariaTomentosa == "[a-z]", 1, 1, 0)) %>%
  
  # helminth
  mutate(New_OtherHelminths = str_extract(histo_inv_fish, "[Hh]elminth|[Hh]elminths")) %>%
  mutate(New_OtherHelminths = if_else(New_OtherHelminths == "[a-z]", 1, 1, 0)) %>%
  
  # myxidium streisingeri
  mutate(New_MyxidiumStreisingeri = str_extract(histo_inv_fish, "[Mm]yxidium|[Ss]treisingeri")) %>%
  mutate(New_MyxidiumStreisingeri = if_else(New_MyxidiumStreisingeri == "[a-z]", 1, 1, 0)) %>%
  
  # fungal infection
  mutate(New_FungalInfection = str_extract(histo_inv_fish, "[Ff]ungal infection")) %>%
  mutate(New_FungalInfection = if_else(New_FungalInfection == "[a-z]", 1, 1, 0)) %>%
  
  # hepatic megalocytosis
  mutate(New_HepaticMegalocytosis = str_extract(histo_inv_fish, "[Hh]epatic megalocytosis|HM")) %>%
  mutate(New_HepaticMegalocytosis = if_else(New_HepaticMegalocytosis == "[a-z]", 1, 1, 0)) %>%
  
  # oophoritis
  mutate(New_EggAssociatedInflammationOophoritis = str_extract(histo_inv_fish, "[Oo]phoritis")) %>%
  mutate(New_EggAssociatedInflammationOophoritis = if_else(New_EggAssociatedInflammationOophoritis == "[a-z]", 1, 1, 0)) %>%
  
  # nephrocalcinosis
  mutate(New_Nephrocalcinosis = str_extract(histo_inv_fish, "[Nn]ephrocalcinosis|[Nn]eprhocalcinosis")) %>%
  mutate(New_Nephrocalcinosis = if_else(New_Nephrocalcinosis == "[a-z]", 1, 1, 0)) %>%
  
  # gill lesions
  mutate(New_GillLesions = str_extract(histo_inv_fish, "[Gg]ill lesions")) %>%
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
  mutate(New_chordoma = str_extract(histo_inv_fish, "[Cc]hordoma")) %>%
  mutate(New_chordoma = if_else(New_chordoma == "[a-z]", 1, 1, 0)) %>%
  
  # Spinal deformity
  mutate(New_SpinalDeformityInHistologicSections = str_extract(histo_inv_fish, "[Ss]pinal deformity")) %>%
  mutate(New_SpinalDeformityInHistologicSections = if_else(New_SpinalDeformityInHistologicSections == "[a-z]", 1, 1, 0)) %>%
  
  # Piscinoodnium pillulare
  mutate(New_PiscinoodiniumPillulare = str_extract(histo_inv_fish, "[Pp]iscinoodnium pillulare|[Pp]illulare")) %>%
  mutate(New_PiscinoodiniumPillulare = if_else(New_PiscinoodiniumPillulare == "[a-z]", 1, 1, 0)) %>%
  
  # Aerocystitis
  mutate(New_AerocystitisEtiologyUnknown = str_extract(histo_inv_fish, "[Aa]erocystitis")) %>%
  mutate(New_AerocystitisEtiologyUnknown = if_else(New_AerocystitisEtiologyUnknown == "[a-z]", 1, 1, 0)) %>%
  
  # Coelomitis
  mutate(New_CoelomitisEtiologyUnknown = str_extract(histo_inv_fish, "[Cc]oelomitis")) %>%
  mutate(New_CoelomitisEtiologyUnknown = if_else(New_CoelomitisEtiologyUnknown == "[a-z]", 1, 1, 0)) %>%
  
  # Splenomegaly
  mutate(New_Splenomegaly = str_extract(histo_inv_fish, "[Ss]plenomegaly|[Ee]nlarged spleen|[Mm]ylodysplastic|[Mm]ylodysplasia")) %>%
  mutate(New_Splenomegaly = if_else(New_Splenomegaly == "[a-z]", 1, 1, 0)) %>%
  
  # Bacilli
  mutate(New_Bacilli = str_extract(histo_inv_fish, "[Bb]acilli")) %>%
  mutate(New_Bacilli = if_else(New_Bacilli == "[a-z]", 1, 1, 0))
  
  

str(disease_columns_mutate)

write.csv(disease_columns_mutate, "disease_columns_mutate.csv")

####### Perform Paired T-test or Equivalent ########
# recombine new data into cases to compare with old data
t_test_new <- disease_columns_mutate %>%
  group_by(CaseId) %>%
  select(starts_with("New_")) %>%
  select(-New_Splenomegaly, -New_Bacilli) %>%
  summarise(across(everything(), sum)) %>% 
  as.data.frame(disease_columns_mutate)

# only keep the first row of each case from the old data
t_test_old <- disease_columns_mutate %>% 
  select(1, 44:56, 59:61, 63:66) %>%
  group_by(CaseId) %>%
  slice(1) %>%
  as.data.frame(disease_columns_mutate)

str(t_test_old)
str(t_test_new)

# combine
t_test <- merge(t_test_old, t_test_new, by = "CaseId")
str(t_test)

# plot in ggplot
library(reshape2)
library(ggplot2)

new.dat <- melt(t_test)

# create new column with grouping variable
# new.dat$condition <- new.dat %>%
# mutate(GroupCond = match(df$CompleteName[1], df$CompleteName.1[1], nomatch = 0))

ggplot(data = new.dat, aes(x = variable, y = value)) + geom_boxplot(outlier.size= 0.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5)) +
  ylab("Fish with Conditions") + 
  xlab("Disease") + 
  scale_x_discrete(limits = c("PseudolomaNeurophilia", 
  "New_PseudolomaNeurophilia", 
  "PleistophoraHyphessobryconis", 
  "New_PleistophoraHyphessobryconis",
  "MycobacteriumSpp",
  "New_MycobacteriumSpp",
  "BacterialInfectionNonAcidFast",
  "New_BacterialInfectionNonAcidFast",
  "PseudocapillariaTomentosa",
  "New_PseudocapillariaTomentosa",
  "OtherHelminths",
  "New_OtherHelminths",
  "MyxidiumStreisingeri",
  "New_MyxidiumStreisingeri",
  "FungalInfection",
  "New_FungalInfection",
  "HepaticMegalocytosis",
  "New_HepaticMegalocytosis",
  "EggAssociatedInflammationOophoritis",
  "New_EggAssociatedInflammationOophoritis",
  "Nephrocalcinosis",
  "New_Nephrocalcinosis",
  "GillLesions",
  "New_GillLesions",
  "EdwardsiellaIctaluri",
  "New_EdwardsiellaIctaluri",
  "Seminoma",
  "New_Seminoma",
  "UltimobranchialAdenomaOrAdenocarcinoma",
  "New_UltimobranchialAdenomaOrAdenocarcinoma",
  "Chordoma",
  "New_Chordoma",
  "SpinalDeformityInHistologicSections",
  "New_SpinalDeformityInHistologicSections",
  "PiscinoodiniumPillulare",
  "New_PiscinoodiniumPillulare",
  "AerocystitisEtiologyUnknown",
  "New_AerocystitisEtiologyUnknown",
  "CoelomitisEtiologyUnknown",
  "New_CoelomitisEtiologyUnknown"))

######### Correlation Plots ##########
library(corrplot)
library(RColorBrewer)
library(ggcorrplot)

# extract sex and disease data 
corrdat <- disease_columns_mutate[,c(77, 79:98)]
str(corrdat)

# test if any columns have all rows equal to 0 
lapply(corrdat[-1], function(x) all(x == 0))
# true for pleistophora, edwardsiella, and piscinoodnium

corrmatrix <- cor(corrdat[,2:21])

# extract only interesting data
corrdat.red <- disease_columns_mutate %>%
  select(New_AerocystitisEtiologyUnknown, 
         New_CoelomitisEtiologyUnknown, 
         New_UltimobranchialAdenomaOrAdenocarcinoma, 
         New_BacterialInfectionNonAcidFast,
         New_MycobacteriumSpp, 
         New_MyxidiumStreisingeri, 
         New_Bacilli,
         New_Splenomegaly,
         New_PseudocapillariaTomentosa,
         sex)

corrmatrix.red <- cor(corrdat.red)

par(cex=0.5)
corrplot(corrmatrix, method="color")
corrplot(corrmatrix.red, method="color")

corrplot(corrmatrix, type="upper",
         col=brewer.pal(n=8, name="RdYlBu"))

ggcorrplot(corrmatrix.red, tl.cex = 5, 
           legend.title = "Correlation Coefficient",
           hc.order = TRUE) + 
  theme(legend.text = element_text(size = 6),
        legend.title = element_text(size = 8)) 

########### chi square test ########
# turn data into table 
corrdat.red <- disease_columns_mutate %>%
  select(New_AerocystitisEtiologyUnknown, 
         New_CoelomitisEtiologyUnknown, 
         New_UltimobranchialAdenomaOrAdenocarcinoma, 
         New_BacterialInfectionNonAcidFast,
         New_MycobacteriumSpp, 
         New_MyxidiumStreisingeri, 
         New_Bacilli,
         New_Splenomegaly,
         New_PseudocapillariaTomentosa,
         sex)

require(data.table)
tab <- setDT(corrdat.red)
tab
ftable(corrdat.red, row.vars = corrdat.red$sex) 
# chisq <- chisq.test(data)
?chisq.test
