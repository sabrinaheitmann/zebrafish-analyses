############# read in data #########
library(dplyr)
library(tidyverse)
library(magrittr)
library(reshape2)
library(patchwork)

disease.fin <- read.csv("all_final_formatted_data_with_extracted_diseases.csv")
disease.fac <- read.csv("all_final_formatted_data_with_extracted_diseases_withFacility.csv")
############# function by sex #########
# create function
disease.sex <- disease.fin[,c(79, 81:103)]
str(disease.sex)

chi_disease <- function(disease_name){
disease_assoc <- as_tibble(disease.sex) %>%
  select(sex, disease_name)

disease_assoc %<>%
  drop_na(sex)

disease_table <- with(disease_assoc, table(sex, disease_name))

print(disease_table)

chi_test <- chisq.test(disease_table, correct=TRUE)

chi_output <- c(chi_test$statistic, chi_test$parameter, chi_test$p.value)

print(chi_output)
}

# chi square by disease
chi_disease(disease_assoc$New_PseudolomaNeurophilia)
chi_disease(disease_assoc$New_PleistophoraHyphessobryconis)
chi_disease(disease_assoc$New_MycobacteriumSpp)
chi_disease(disease_assoc$New_BacterialInfectionNonAcidFast)
chi_disease(disease_assoc$New_PseudocapillariaTomentosa)
chi_disease(disease_assoc$New_OtherHelminths)
chi_disease(disease_assoc$New_MyxidiumStreisingeri)
chi_disease(disease_assoc$New_FungalInfection)
chi_disease(disease_assoc$New_HepaticMegalocytosis)
chi_disease(disease_assoc$New_EggAssociatedInflammationOophoritis)
chi_disease(disease_assoc$New_Nephrocalcinosis)
chi_disease(disease_assoc$New_GillLesions)
chi_disease(disease_assoc$New_EdwardsiellaIctaluri)
chi_disease(disease_assoc$New_Seminoma)
chi_disease(disease_assoc$New_UltimobranchialAdenomaOrAdenocarcinoma)
chi_disease(disease_assoc$New_chordoma)
chi_disease(disease_assoc$New_SpinalDeformityInHistologicSections)
chi_disease(disease_assoc$New_PiscinoodiniumPillulare)
chi_disease(disease_assoc$New_AerocystitisEtiologyUnknown)
chi_disease(disease_assoc$New_CoelomitisEtiologyUnknown)
chi_disease(disease_assoc$New_Splenomegaly)
chi_disease(disease_assoc$New_Bacilli)
chi_disease(disease_assoc$New_SwimBladder)

######### function by facility ##########
# find which facilities submitted the most fish
top_row <- disease.fac[!duplicated(disease.fac$CaseId), ]
top_row_new <- disease.fac[,c(24,80:103)]
str(top_row_new)

fac_reduced <- top_row %>%
  group_by(Facility) %>% 
  drop_na(NumberOfFish) %>%
  dplyr::summarize(Total_fish = sum(NumberOfFish)) %>%
  arrange(desc(Total_fish))

# manipulate data for four diseases: PN, HM, UA, and Splenomegaly
# Pseudoloma neurophilia
disease.fac.new <- disease.fac[,c(24,80:103)]

disease_assoc_fac <- as_tibble(disease.fac.new) %>%
  select(Facility, New_PseudolomaNeurophilia, NumberOfFish) %>% 
  drop_na(NumberOfFish) %>%
  drop_na(Facility) %>%
  select(Facility, New_PseudolomaNeurophilia)

disease_table_fac <- with(disease_assoc_fac, table(Facility, New_PseudolomaNeurophilia))
write.csv(disease_table_fac, "PN_table_fac_old.csv")


# UA
disease_assoc_fac_UA <- as_tibble(disease.fac.new) %>%
  select(Facility, New_UltimobranchialAdenomaOrAdenocarcinoma, NumberOfFish) %>% 
  drop_na(NumberOfFish) %>%
  drop_na(Facility) %>%
  select(Facility, New_UltimobranchialAdenomaOrAdenocarcinoma)

disease_table_fac_UA <- with(disease_assoc_fac_UA, table(Facility, New_UltimobranchialAdenomaOrAdenocarcinoma))
write.csv(disease_table_fac_UA, "UA_table_fac_old.csv")

# Splenomegaly
disease_assoc_fac_spl <- as_tibble(disease.fac.new) %>%
  select(Facility, New_Splenomegaly, NumberOfFish) %>% 
  drop_na(NumberOfFish) %>%
  drop_na(Facility) %>%
  select(Facility, New_Splenomegaly)

disease_table_fac_spl <- with(disease_assoc_fac_spl, table(Facility, New_Splenomegaly))
write.csv(disease_table_fac_spl, "Spl_table_fac_old.csv")

# HM
disease_assoc_fac_hm <- as_tibble(disease.fac.new) %>%
  select(Facility, New_HepaticMegalocytosis, NumberOfFish) %>% 
  drop_na(NumberOfFish) %>%
  drop_na(Facility) %>%
  select(Facility, New_HepaticMegalocytosis)

disease_table_fac_hm <- with(disease_assoc_fac_hm, table(Facility, New_HepaticMegalocytosis))
write.csv(disease_table_fac_hm, "hm_table_fac_old.csv")

############# facility plots ##########
# create data frames for each plot
# these are very messy, my bad! 
PN_per <- data.frame (top_facility  = c("Frederic", "Zhu", "Mayorga", "Austen", "Swaim"),
                      second_column = as.numeric(c("66.7", "66.7", "53.3", "50", "43.8")))

Spl_per <- data.frame(facility = c("K Summit Plc, DVM", "Whitaker", "Belting", "ZIRC"),
                      percent = as.numeric(c("2.3", "3.6", "12.5", "0.3")))
HM_per <- data.frame(facility = c("Ross", "Alisantosa", "Prober lab", "Halluin", "ZIRC"),
                     percent = as.numeric(c("50", "33.3", "25", "21.1", "18.4")))
UA_df <- read.csv("UA_table_fac_new.csv")

# create plots
(PN_per <- ggplot(PN_per) + geom_bar(aes(x = top_facility, 
 y = second_column), stat = "identity", fill = "seagreen4") + theme_light()) + 
  coord_flip() + ylab("Percentage of zebrafish with disease") + 
  xlab("Facilities")
(UA_per <- ggplot(UA_df) + geom_bar(aes(x = X, 
  y = percent), stat = "identity", fill = "skyblue4") + theme_light()) + 
  coord_flip() + ylab("Percentage of zebrafish with disease") + 
  xlab("Facilities")
(Spl_per <- ggplot(Spl_per) + geom_bar(aes(x = facility, y = percent), 
  stat = "identity", fill = "thistle4") + theme_light()) + 
  coord_flip() + ylab("Percentage of zebrafish with disease") + 
  xlab("Facilities")
(HM_per <- ggplot(HM_per) + geom_bar(aes(x = facility, y = percent), 
  stat = "identity", fill = "lightcyan4") + theme_light()) + 
  coord_flip() + ylab("Percentage of zebrafish with disease") + 
  xlab("Facilities")

# save plots
ggsave("HM_per.png", HM_per)
ggsave("Spl_per.png", Spl_per)
ggsave("UA_per.png", UA_per)
ggsave("PN_per.png", PN_per)

###########  sex plots #########
# summarize data
plot_fishsex <- disease.sex %>%
  select(sex, New_PseudolomaNeurophilia, New_MyxidiumStreisingeri,
         New_HepaticMegalocytosis, New_EggAssociatedInflammationOophoritis,
         New_Nephrocalcinosis, New_Seminoma, 
         New_UltimobranchialAdenomaOrAdenocarcinoma, 
         New_SpinalDeformityInHistologicSections, 
         New_CoelomitisEtiologyUnknown, 
         New_Splenomegaly) %>%
  group_by(sex) %>%
  summarize_all(.funs = c(Sum ="sum")) %>%
  na.omit(plot_fishsex)

# change from wide to long format
newplot_sex <- melt(plot_fishsex)

# rename variable titles
newplot_red <- newplot_sex %>% 
  mutate(variable = as.character(variable)) %>%
  mutate(variable = replace(variable, variable == "New_PseudolomaNeurophilia_Sum", "Pseudoloma Nerophilia")) %>%
  mutate(variable = replace(variable, variable == "New_MyxidiumStreisingeri_Sum", "Myxidium Streisingeri")) %>%
  mutate(variable = replace(variable, variable == "New_HepaticMegalocytosis_Sum", "Hepaic Megalocytosis")) %>%
  mutate(variable = replace(variable, variable == "New_EggAssociatedInflammationOophoritis_Sum", "Egg Associated Inflammation Oophoritis")) %>%
  mutate(variable = replace(variable, variable == "New_Nephrocalcinosis_Sum", "Nephrocalcinosis")) %>%
  mutate(variable = replace(variable, variable == "New_Seminoma_Sum", "Seminoma")) %>%
  mutate(variable = replace(variable, variable == "New_UltimobranchialAdenomaOrAdenocarcinoma_Sum", "Ultimobranchial Adenoma or Adenocarcinoma")) %>%
  mutate(variable = replace(variable, variable == "New_SpinalDeformityInHistologicSections_Sum", "Spinal Deformity in Histologic Sections")) %>%
  mutate(variable = replace(variable, variable == "New_CoelomitisEtiologyUnknown_Sum", "Coelomitis Etiology Unknown")) %>%
  mutate(variable = replace(variable, variable == "New_Splenomegaly_Sum", "Splenomegaly"))

# create plot
(fish_sex_plot <- ggplot(newplot_red, aes(y = value, x = sex)) + geom_bar(stat = "identity") + 
  theme_light() + 
  theme(axis.text.x = element_text(size=7, angle=45, hjust = 1, vjust = 1.1), 
  strip.text = element_text(size = 7))+ 
  facet_wrap(~ variable, scales = "free") + 
  ylab("Number of fish with condition") + 
  xlab("Fish sex"))

# save plot
ggsave("chi_sex_plot.png", fish_sex_plot)
  

                                                                  