library(tidyr)
library(dplyr)

############################################
#            SET VARIABLES                 #
############################################
EXPORT_DIR <- "../Analysis Output/Final"


############################################
#        IMPORT AND PROCESS DATA           #
############################################

fish_data <- read.csv("../Preprocessed Statistics Files/nbm_and_vp_stats_merged.csv")
specimen_info <- read.csv("../Preprocessed Statistics Files/specimen info_ASD genetics.csv")

# change the first column name of specimen info, and perform a left join.
colnames(specimen_info)[1] = "sample_id"
merged_data <- merge(x = fish_data, y = specimen_info, by = "sample_id", all.x = T)

# remove samples without any corresponding binding data
merged_data<-merged_data[!is.na(merged_data$oxtr_density_in_nbm) | !is.na(merged_data$oxtr_density_in_vp),]

# change Disorder / Sex / Race to factors
merged_data$Disorder <- as.factor(merged_data$Disorder)
merged_data$Race <- as.factor(merged_data$Race)
merged_data$Sex <- as.factor(merged_data$Sex)

# generate split datasets from the merged data.
merged_data.asd <- merged_data %>% dplyr::filter(Disorder == "ASD - Autism")
merged_data.nt <- merged_data %>% dplyr::filter(Disorder != "ASD - Autism")

