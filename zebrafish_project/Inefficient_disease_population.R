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

#test <- disease_columns_mutate %>%
#  select(starts_with("New_"), CaseId, histo_inv_fish) %>%
#  group_by(CaseId) %>%
#  summarise(starts_with("New_"))
  
str(disease_columns_mutate)
write.csv(disease_columns_mutate, "output/disease_columns_mutate.csv")

for_Jacob <- disease_columns_mutate %>%
  group_by(CaseId) %>%
  mutate_at(vars(starts_with("New_")), sum)

for_Jacob <- for_Jacob[!duplicated(for_Jacob$CaseId),]

for_Jacob %<>%
  select(-c(sex, fish, histo_inv_fish, fish_id))

#Every case is own row with number of fish per case that have the disease
write.csv(for_Jacob, "output/for_jacob.csv")

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

#### Sabrina's figure ####
library(stringr)
library(forcats)
library(wesanderson)

disease_counts <- new.dat %>%
  group_by(variable) %>%
  summarise(fish_count = sum(value)) %>%
  mutate(who_processed = case_when(str_detect(variable, "New_") ~ "ZTeam", TRUE ~ "Katy")) %>%
  mutate_all(~gsub("New_", "", .)) %>%
  rename(disease = variable) %>%
  mutate(disease = gsub('([[:upper:]])', ' \\1', disease)) %>%
  mutate(fish_count = as.numeric(fish_count)) %>%
  filter(fish_count != 0) %>% # This could be better coded. Filter out disease if both old and new disease = 0
  filter(!str_detect(disease, 'Bladder')) # Only until Risa fixes code

disease_counts$disease <- trimws(disease_counts$disease) 

write_csv(disease_counts, "disease_counts_case_basis.csv")
disease_counts
unique(disease_counts[c("disease")]) # there are 17 which means that old and new disease names match

# Adjust error

#Inputs: New_disease = Diagnosis from the extract function, disease = diagnosis from Katy, col_name = disease_name
adjust_Katy <- function(New_disease, disease, col_name){
  disease <- disease_columns_mutate %>%
    as.data.frame() %>%
    select(New_disease, disease, histo_inv_fish) %>%
    tally(New_disease == 1 & disease == 0) %>%
    summarise(n = sum(n))
  #rename(!!sym(col_name) := n)
  
  disease %>%
    add_column(disease = col_name)
}

# Maude's suggested loop
#list1 = c(HepaticMegalocytosis, MycobacteriumSpp
#list2 = Hepatic Megalo
#list1[1]
#list1[i]
#For I in 1:length(list1)
#{{adjust_Katy(diseasblablabla$New_list1[I])}
#then paste New_$New_list1[I]))}
#{{adjust_Katy(diseasblablabla$New_list1[I])}

d1 <- adjust_Katy(disease_columns_mutate$New_HepaticMegalocytosis, disease_columns_mutate$HepaticMegalocytosis, "Hepatic Megalocytosis")
d2 <- adjust_Katy(disease_columns_mutate$New_MycobacteriumSpp, disease_columns_mutate$MycobacteriumSpp, "Mycobacterium Spp")
d3 <- adjust_Katy(disease_columns_mutate$New_PseudolomaNeurophilia, disease_columns_mutate$PseudolomaNeurophilia, "Pseudoloma Neurophilia")
d4 <- adjust_Katy(disease_columns_mutate$New_MyxidiumStreisingeri, disease_columns_mutate$MyxidiumStreisingeri, "Myxidium Streisingeri")
d5 <- adjust_Katy(disease_columns_mutate$New_Nephrocalcinosis, disease_columns_mutate$Nephrocalcinosis, "Nephrocalcinosis")
d6 <- adjust_Katy(disease_columns_mutate$New_GillLesions, disease_columns_mutate$GillLesions, "Gill Lesions")
d7 <- adjust_Katy(disease_columns_mutate$New_CoelomitisEtiologyUnknown, disease_columns_mutate$CoelomitisEtiologyUnknown, "Coelomitis Etiology Unknown")
d8 <- adjust_Katy(disease_columns_mutate$New_SpinalDeformityInHistologicSections, disease_columns_mutate$SpinalDeformityInHistologicSections, "Spinal Deformity In Histologic Sections")
d9 <- adjust_Katy(disease_columns_mutate$New_UltimobranchialAdenomaOrAdenocarcinoma, disease_columns_mutate$UltimobranchialAdenomaOrAdenocarcinoma, "Ultimobranchial Adenoma Or Adenocarcinoma")
d10 <- adjust_Katy(disease_columns_mutate$New_Seminoma, disease_columns_mutate$Seminoma, "Seminoma")
d11 <- adjust_Katy(disease_columns_mutate$New_AerocystitisEtiologyUnknown, disease_columns_mutate$AerocystitisEtiologyUnknown, "Aerocystitis Etiology Unknown")
d12 <- adjust_Katy(disease_columns_mutate$New_PseudocapillariaTomentosa, disease_columns_mutate$PseudocapillariaTomentosa, "Pseudocapillaria Tomentosa")
d13 <- adjust_Katy(disease_columns_mutate$New_BacterialInfectionNonAcidFast, disease_columns_mutate$BacterialInfectionNonAcidFast, "Bacterial Infection Non Acid Fast")
d14 <- adjust_Katy(disease_columns_mutate$New_Chordoma, disease_columns_mutate$Chordoma, "Chordoma")
d15 <- adjust_Katy(disease_columns_mutate$New_FungalInfection, disease_columns_mutate$FungalInfection, "Fungal Infection")
d16 <- adjust_Katy(disease_columns_mutate$New_OtherHelminths, disease_columns_mutate$OtherHelminths, "OtherHelminths")
d17 <- adjust_Katy(disease_columns_mutate$New_EggAssociatedInflammationOophoritis, disease_columns_mutate$EggAssociatedInflammationOophoritis, "Egg Associated Inflammation Oophoritis")

add_to_katy <- rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16, d17)

#### Error percentages 
error_percents <- disease_counts %>%
  pivot_wider(names_from = who_processed, values_from = fish_count) %>%
  mutate(error_percent = (ZTeam - Katy)/Katy)

error_percents <- left_join(error_percents, add_to_katy, by = "disease")

error_percents %<>%
  mutate(adjusted_Katy = Katy + n) %>%
  mutate(adjusted_error_percent = (ZTeam - adjusted_Katy)/adjusted_Katy) %>%
  mutate(adjusted_error_percent_dif = error_percent - adjusted_error_percent) %>%
  filter(!disease == "Other Helminths")

write.csv(error_percents, "output/error_percents.csv")

error_for_fig <- error_percents %>% 
  select(error_percent, adjusted_error_percent, disease) %>%
  mutate(real_error = adjusted_error_percent * 100) %>%
  mutate_if(is.numeric, round, digits = 2)

# Add adjusted counts to disease_counts df
add_to_katy %<>%
  mutate(who_processed = "Katy")

disease_counts <- left_join(disease_counts, add_to_katy, by = c("disease", "who_processed"))

disease_counts %<>%
  filter(fish_count > 5) %>%
  mutate(adjusted_fish_count = if_else(!is.na(n), fish_count + n, fish_count))

# Reorder for barchart
disease_counts %<>%
  ungroup() %>%
  arrange(who_processed, adjusted_fish_count) %>%
  mutate(disease = fct_inorder(disease))
  
# Barchart
max_val <- max(disease_counts$adjusted_fish_count)
disease_counts %>%
  ggplot(aes(x = disease, y = adjusted_fish_count, fill = who_processed)) +
  geom_bar(stat="identity",position="dodge", alpha=.6, width=.9) +
  #scale_y_log10()
  coord_flip() +
  theme_light() +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  ylab("Number of fish") +
  xlab("") +
  labs(fill="Diagnosis") +
  scale_fill_manual(values=wes_palette(n=2, name="GrandBudapest1"), labels = c("ZIRC", "Extract function")) +
  scale_y_continuous(breaks = seq(0, max_val, by = 250), limits=c(0, max_val)) +
  geom_text(data=error_for_fig,aes(x=disease,y=0,label=paste(real_error, "%", sep = ""),position = "dodge"),
            hline = 1,
            inherit.aes=FALSE, size = 10)

ggsave("output/Katy_Zteam_fishcounts_perdisease.png", height = 10, width = 17)

#### Adjusting error ####

# The start of a loop but don't know if I have time to figure it out
#DiseaseOutput <- data.frame(matrix(vector(),16,1, dimnames=list(c(), c("Disease"))))
#DiseaseOutput$Disease <- unique(disease_counts[,1]) 
#DiseaseOutput$Adjustment <- c(NA)

# This function tally's up all of the diagnoses Katy missed and that the extract function recognizes.
# This is only for cases tha Katy

#for (index in 44:117){
  
  #130:117
#  disease_column = disease_columns_mutate[,index]
  
#  130:152
#  row_disease = row_entry$Disease.disease
#  VAR1 == New_HepaticMegalocytosis  #New_HepaticMegalocytosis
#  VAR2 == disease_column      #HepaticMegalocytosis
#  VAR3 == colnames(disease_column)   #"Hepatic Megalocytosis"
#  adjust_Katy(disease_columns_mutate$VAR1, VAR2, VAR3)

#  row_entry$Adjustment = output
#}

d1

#Every case is own row with number of fish per case that have the disease
disease_columns_mutate %>%
  group_by(CaseId) %>%
  su

# adjust_Katy code but for only 1 disease
test <- disease_columns_mutate %>%
  as.data.frame() %>%
  select(New_HepaticMegalocytosis, HepaticMegalocytosis, histo_inv_fish, CaseId) %>%
  group_by(CaseId)
#  summarise(New_HepaticMegalocytosis)
#  tally(New_HepaticMegalocytosis == 1 & HepaticMegalocytosis == 0) %>%
#  summarise(n = sum(n))
#rename(!!sym(col_name) := n)

#### Risa's figure ####

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
corrdat <- disease_columns_mutate[,c(77, 79:101)]
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
         New_SwimBladder)

str(as.data.frame(corrdat.red))
corrmatrix.red <- cor(corrdat.red[, 2:11])

par(cex=0.5)
corrplot(corrmatrix, method="color")
corrplot(corrmatrix.red, method="color", type = "upper")

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
