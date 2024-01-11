# Reliability for the questionnaires, based on current data
# Author: Magda Matetovici

#Packages:
#install.packages("psych")
#install.packages("dplyr")
#install.packages("mice")
#install.packages("ufs")

# Load packages
library(psych)
library(dplyr)
library(mice)


# Put the data together
data_attachment_recoded<- read.csv(file = "data_attachment_original_recoded.csv")
data_sdq_recoded <- read.csv(file = "data_sdq_original_recoded.csv")

#select recoded variables and add the data together
data_groups <- cbind(data_attachment_recoded[, 49:97], data_sdq_recoded[, 23:42])

#Make everthing is orderded factors
# Make sure they are all ordered variables
# Convert all columns to factors for both datasets
data_groups <- data_groups %>% 
  mutate_all(factor)


# Convert all factors to ordered factors for both datasets
data_groups <- data_groups %>% 
  mutate_all(ordered)

set.seed(12345)
imp_groups <- mice(data_groups, m = 1, method = "polr")
imp_groups <- complete(imp_groups, 1)

imp_groups <- imp_groups %>%
  mutate(parents = case_when(
    dyad %in% c("mother_daughter", "mother_son") ~ "mother",
    dyad %in% c("father_daughter", "father_son") ~ "father",
  ))
imp_groups$parents


imp_groups <- imp_groups %>%
  mutate(children = case_when(
    dyad %in% c("mother_daughter", "father_daughter") ~ "daughter",
    dyad %in% c("father_son", "mother_son") ~ "son",
  ))
imp_groups$children

imp_groups <- imp_groups %>%
  mutate(similar = case_when(
    dyad %in% c("mother_daughter", "father_son") ~ "same",
    dyad %in% c("father_daughter", "mother_son") ~ "different",
  ))
imp_groups$similar



secure_attachment = imp_groups[,c("C_ari6_recoded", "C_ari11_recoded", "C_ari17_recoded", "C_ari30_recoded", "C_ari37_recoded", "C_ari42_recoded", "C_ari46_recoded", "C_ari47_recoded", "C_ari50_recoded", "C_ari54_recoded","C_ari59_recoded", "C_ari61_recoded", "C_ari64_recoded")]

avoidant_attachment = imp_groups[, c("C_ari3_recoded", "C_ari14_recoded", "C_ari23_recoded", "C_ari25_recoded", "C_ari26_recoded","C_ari31_recoded", "C_ari33_recoded", "C_ari34_recoded", "C_ari36_recoded", "C_ari57_recoded", "C_ari66_recoded" )]

ambivalent_attachment = imp_groups[, c("C_ari12_recoded", 'C_ari15_recoded', "C_ari16_recoded", "C_ari19_recoded", "C_ari38_recoded", "C_ari39_recoded", "C_ari51_recoded", "C_ari53_recoded", "C_ari56_recoded", "C_ari58_recoded", "C_ari63_recoded")]

disorganised_attachment = imp_groups[, c("C_ari4_recoded", "C_ari8_recoded", "C_ari13_recoded", "C_ari18_recoded", "C_ari22_recoded", "C_ari28_recoded", "C_ari29_recoded", "C_ari35_recoded", "C_ari40_recoded", "C_ari43_recoded", "C_ari49_recoded", "C_ari62_recoded", "C_ari65_recoded")]

internalising = imp_groups[, c("C_SDQ8.2_3_recoded", "C_SDQ8.2_8_recoded", "C_SDQ8.2_13_recoded", "C_SDQ8.2_16_recoded", "C_SDQ8.2_24_recoded", "C_SDQ8.2_6_recoded", "C_SDQ8.2_11_recoded", "C_SDQ8.2_14_recoded", "C_SDQ8.2_19_recoded", "C_SDQ8.2_23_recoded")] 

externalising = imp_groups[, c("C_SDQ8.2_5_recoded", "C_SDQ8.2_7_recoded", "C_SDQ8.2_12_recoded", "C_SDQ8.2_18_recoded", "C_SDQ8.2_22_recoded", "C_SDQ8.2_2_recoded", "C_SDQ8.2_10_recoded", "C_SDQ8.2_15_recoded", "C_SDQ8.2_21_recoded", "C_SDQ8.2_25_recoded")]

# Cronbach's alphas

# ARI-CP
library(ufs)

scaleStructure(secure_attachment)
scaleStructure(avoidant_attachment)
scaleStructure(ambivalent_attachment)
scaleStructure(disorganised_attachment)
scaleStructure(internalising)
scaleStructure(externalising)
