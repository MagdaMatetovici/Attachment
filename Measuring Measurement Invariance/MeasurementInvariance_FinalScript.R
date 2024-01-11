#Measurement Invariance for Attachment and Problem Behavior Questionnaires
# Author: Magda Matetovici

#Packages
#install.packages("lavaan")
#install.packages("dplyr")
#install.packages("mice")
#install.packaes("semTools")

#Load libraries
# library(lavaan)
library(dplyr)
library(mice)
library(semTools)


# Load datafiles
data_attachment_recoded<- read.csv(file = "data_attachment_original_recoded.csv")
data_sdq_recoded <- read.csv(file = "data_sdq_original_recoded.csv")

# Make sure they are all ordered variables
# Convert all columns to factors for both datasets
data_attachment_recoded <- data_attachment_recoded %>% 
  mutate_all(factor)

data_sdq_recoded <- data_sdq_recoded %>% 
  mutate_all(factor)

# Convert all factors to ordered factors for both datasets
data_attachment_recoded <- data_attachment_recoded %>% 
  mutate_all(ordered)

data_sdq_recoded <- data_sdq_recoded %>% 
  mutate_all(ordered)


#Perform multiple imputation for attachment data 
set.seed(12345)
imp_attachment <- mice(data_attachment_recoded[, 49:97], m = 1, method = "polr")
imp_attachment <- complete(imp_attachment, 1)

imp_attachment <- imp_attachment %>%
  mutate(parents = case_when(
    dyad %in% c("mother_daughter", "mother_son") ~ "mother",
    dyad %in% c("father_daughter", "father_son") ~ "father",
  ))
imp_attachment$parents


imp_attachment <- imp_attachment %>%
  mutate(children = case_when(
    dyad %in% c("mother_daughter", "father_daughter") ~ "daughter",
    dyad %in% c("father_son", "mother_son") ~ "son",
  ))
imp_attachment$children

imp_attachment <- imp_attachment %>%
  mutate(similar = case_when(
    dyad %in% c("mother_daughter", "father_son") ~ "same",
    dyad %in% c("father_daughter", "mother_son") ~ "different",
  ))
imp_attachment$similar
imp_attachment$dyad

#-------------------------------------------------------------------------------
# SEM model
#-------------------------------------------------------------------------------
#model with 4 factors
model_attachment <- '
        secure_attachment =~ C_ari6_recoded + C_ari11_recoded + C_ari17_recoded + C_ari30_recoded + C_ari37_recoded + 
                              C_ari42_recoded + C_ari46_recoded + C_ari47_recoded + C_ari50_recoded + C_ari54_recoded + 
                                C_ari59_recoded + C_ari61_recoded + C_ari64_recoded
                    
        avoidant_attachment =~ C_ari3_recoded + C_ari14_recoded + C_ari23_recoded + C_ari25_recoded + C_ari26_recoded + 
                                C_ari31_recoded + C_ari33_recoded + C_ari34_recoded + C_ari36_recoded + C_ari57_recoded + 
                                  C_ari66_recoded
                            
        ambivalent_attachment =~ C_ari12_recoded + C_ari15_recoded + C_ari16_recoded + C_ari19_recoded + C_ari38_recoded + 
                                  C_ari39_recoded + C_ari51_recoded + C_ari53_recoded + C_ari56_recoded + C_ari58_recoded + 
                                    C_ari63_recoded
                                
        disorganised_attachment =~ C_ari4_recoded + C_ari8_recoded + C_ari13_recoded + C_ari18_recoded + C_ari22_recoded + 
                                    C_ari28_recoded + C_ari29_recoded + C_ari35_recoded + C_ari40_recoded + C_ari43_recoded + 
                                      C_ari49_recoded + C_ari62_recoded + C_ari65_recoded
'

#-------------------------------------------------------------------------------
# Configural Invariance
# Table A1 manuscript Attachment Model
#-------------------------------------------------------------------------------

fit_configural_parent_attachment <- cfa(model = model_attachment, 
                                               data = imp_attachment,
                                               group = "parents",
                                               ordered = TRUE,
                                               estimator = 'WLSMV',
                                               parameterization = "delta",
                                               std.lv = TRUE)
fitMeasures(fit_configural_parent_attachment)
save(fit_configural_parent_attachment, file = "fit_configural_parent_attachment")
load("fit_configural_parent_attachment")

permuteMeasEq_fit_configural_parent_attachment <- permuteMeasEq(nPermute = 1000, con = fit_configural_parent_attachment,
                            modelType = "mgcfa", uncon = NULL, param = NULL,
                            parallelType = "multicore")
permuteMeasEq_fit_configural_parent_attachment
save(permuteMeasEq_fit_configural_parent_attachment, file = "permuteMeasEq_fit_configural_parent_attachment")
load("permuteMeasEq_fit_configural_parent_attachment")

fit_configural_children_attachment <- cfa(model = model_attachment , 
                                   data = imp_attachment,
                                   group = "children",
                                   ordered = TRUE,
                                   estimator = 'WLSMV',
                                   parameterization = "delta",
                                   std.lv = TRUE)
fitMeasures(fit_configural_children_attachment)
save(fit_configural_children_attachment, file = "fit_configural_children_attachment")
load("fit_configural_children_attachment")

permuteMeasEq_fit_configural_children_attachment <- permuteMeasEq(nPermute = 1000, con = fit_configural_children_attachment,
                              modelType = "mgcfa", uncon = NULL, param = NULL,
                              parallelType = "multicore")
permuteMeasEq_fit_configural_children_attachment
save(permuteMeasEq_fit_configural_children_attachment, file = "permuteMeasEq_fit_configural_children_attachment")
load("permuteMeasEq_fit_configural_children_attachment")

fit_configural_similar_attachment <- cfa(model = model_attachment , 
                                   data = imp_attachment,
                                   group = "similar",
                                   ordered = TRUE,
                                   estimator = 'WLSMV',
                                   parameterization = "delta",
                                   std.lv = TRUE)
fitMeasures(fit_configural_similar_attachment)
save(fit_configural_similar_attachment, file = "fit_configural_similar_attachment")
load("fit_configural_similar_attachment")

permuteMeasEq_fit_configural_similar_attachment <- permuteMeasEq(nPermute = 1000, con = fit_configural_similar_attachment,
                              modelType = "mgcfa", uncon = NULL, param = NULL,
                              parallelType = "multicore")
permuteMeasEq_fit_configural_similar_attachment
save(permuteMeasEq_fit_configural_similar_attachment, file = "permuteMeasEq_fit_configural_similar_attachment")
load("permuteMeasEq_fit_configural_similar_attachment")

#-------------------------------------------------------------------------------
# Configural Invariance
# Table A1 manuscript Problem Behavior Model
#-------------------------------------------------------------------------------

#Perform multiple imputation for attachment data 
imp_problems <- mice(data_sdq_recoded[, 22:42], m = 1, method = "polr")
imp_problems <- complete(imp_problems, 1)
imp_problems$parents <- imp_attachment$parents
imp_problems$children <- imp_attachment$children
imp_problems$similar <- imp_attachment$similar

model_problems <- '
                  internalising =~ C_SDQ8.2_3_recoded +  C_SDQ8.2_8_recoded + C_SDQ8.2_13_recoded + 
                                    C_SDQ8.2_16_recoded + C_SDQ8.2_24_recoded + C_SDQ8.2_6_recoded + 
                                    C_SDQ8.2_11_recoded + C_SDQ8.2_14_recoded + C_SDQ8.2_19_recoded + 
                                    C_SDQ8.2_23_recoded
                  externalising =~ C_SDQ8.2_5_recoded + C_SDQ8.2_7_recoded + C_SDQ8.2_12_recoded + C_SDQ8.2_18_recoded + 
                                    C_SDQ8.2_22_recoded + C_SDQ8.2_2_recoded + C_SDQ8.2_10_recoded + C_SDQ8.2_15_recoded + 
                                    C_SDQ8.2_21_recoded + C_SDQ8.2_25_recoded
'

#-------------------------------------------------------------------------------
#Parents
fit_configural_parents_problems <- cfa(model = model_problems, 
                                       data = imp_problems,
                                       group = "parents",
                                       ordered = TRUE,
                                       estimator = 'WLSMV',
                                       parameterization = "delta",
                                       std.lv = TRUE)

fitMeasures(fit_configural_parents_problems)
save(fit_configural_parents_problems, file = "fit_configural_parents_problems")
load("fit_configural_parents_problems")

permuteMeasEq_fit_configural_parents_problems <- permuteMeasEq(nPermute = 1000, con = fit_configural_parents_problems,
                              modelType = "mgcfa", uncon = NULL, param = NULL,
                              parallelType = "multicore")
permuteMeasEq_fit_configural_parents_problems
save(permuteMeasEq_fit_configural_parents_problems, file = "permuteMeasEq_fit_configural_parents_problems")
load("permuteMeasEq_fit_configural_parents_problems")

#-------------------------------------------------------------------------------
# Children
fit_configural_children_problems <- cfa(model = model_problems, 
                                        data = imp_problems,
                                        group = "children",
                                        ordered = TRUE,
                                        estimator = 'WLSMV',
                                        parameterization = "delta",
                                        std.lv = TRUE)

fitMeasures(fit_configural_children_problems)
save(fit_configural_children_problems, file = "fit_configural_children_problems")
load("fit_configural_children_problems")

permuteMeasEq_fit_configural_children_problems <- permuteMeasEq(nPermute = 1000, con = fit_configural_children_problems,
                              modelType = "mgcfa", uncon = NULL, param = NULL,
                              parallelType = "multicore")

permuteMeasEq_fit_configural_children_problems
save(permuteMeasEq_fit_configural_children_problems, file = "permuteMeasEq_fit_configural_children_problems")
load("permuteMeasEq_fit_configural_children_problems")

#-------------------------------------------------------------------------------
# similar
fit_configural_similar_problems <- cfa(model = model_problems, 
                                       data = imp_problems,
                                       group = "similar",
                                       ordered = TRUE,
                                       estimator = 'WLSMV',
                                       parameterization = "delta",
                                       std.lv = TRUE)

fitMeasures(fit_configural_similar_problems)
save(fit_configural_similar_problems, file = "fit_configural_similar_problems")
load("fit_configural_similar_problems")

permuteMeasEq_fit_configural_similar_problems <- permuteMeasEq(nPermute = 1000, con = fit_configural_similar_problems,
                              modelType = "mgcfa", uncon = NULL, param = NULL,
                              parallelType = "multicore")

permuteMeasEq_fit_configural_similar_problems
save(permuteMeasEq_fit_configural_similar_problems, file = "permuteMeasEq_fit_configural_similar_problems")
load("permuteMeasEq_fit_configural_similar_problems")

# End of table A1 data
################################################################################

#-------------------------------------------------------------------------------
# Threshold, weak and strong Invariance
# Attachment
# Table A2 manuscript 
#-------------------------------------------------------------------------------

####
#### Threshold invariance
####

#-------------------------------------------------------------------------------
# parents
tMod <- measEq.syntax(configural.model = fit_configural_parent_attachment,
                      data = imp_attachment,
                      ordered = TRUE,
                      parameterization = "delta",
                      ID.fac = "std.lv",
                      ID.cat = "Wu.Estabrook.2016",
                      group = "parents",
                      group.equal = "thresholds")

tMod %>% as.character()%>% cat ()

tOut_full_attachment_parents <- cfa(model = as.character(tMod), 
            data = imp_attachment,
            group = "parents",
            ordered = TRUE,
            estimator = 'WLSMV')
fitMeasures(tOut_full_attachment_parents)

save(tOut_full_attachment_parents, file = "tOut_full_attachment_parents")

anova_threshold_parents <- anova(fit_configural_parent_attachment,tOut_full_attachment_parents)

save(anova_threshold_parents, file = "anova_threshold_parents")

#-------------------------------------------------------------------------------
# children 
tMod_full_children <- measEq.syntax(configural.model = fit_configural_children_attachment,
                             data = imp_attachment,
                             ordered = TRUE,
                             parameterization = "delta",
                             ID.fac = "std.lv",
                             ID.cat = "Wu.Estabrook.2016",
                             group = "children",
                             group.equal = "thresholds")

tMod_full_children  %>% as.character()%>% cat ()

tOut_full_children <- cfa(model = as.character(tMod_full_children ), 
                            data = imp_attachment,
                            group = "children",
                            ordered = TRUE,
                            estimator = 'WLSMV')
fitMeasures(tOut_full_children)

anova_threshold_children <- anova(fit_configural_children_attachment, tOut_full_children)
save(anova_threshold_children, file = "anova_threshold_children")

#-------------------------------------------------------------------------------
# similar
tMod_full_same <- measEq.syntax(configural.model = fit_configural_similar_attachment,
                             data = imp_attachment,
                             ordered = TRUE,
                             parameterization = "delta",
                             ID.fac = "std.lv",
                             ID.cat = "Wu.Estabrook.2016",
                             group = "similar",
                             group.equal = "thresholds")

tMod_full_same %>% as.character()%>% cat ()

tOut_full_same <- cfa(model = as.character(tMod_full_same ), 
                           data = imp_attachment,
                           group = "similar",
                           ordered = TRUE,
                           estimator = 'WLSMV')
fitMeasures(tOut_full_same)

anova_threshold_similar <- anova(fit_configural_similar_attachment, tOut_full_same)
save(anova_threshold_similar , file = "anova_threshold_similar")

####
#### Weak invariance
####

#-------------------------------------------------------------------------------
# parents
tlMod <- measEq.syntax(configural.model = fit_configural_parent_attachment,
                      data = imp_attachment,
                      ordered = TRUE,
                      parameterization = "delta",
                      ID.fac = "std.lv",
                      ID.cat = "Wu.Estabrook.2016",
                      group = "parents",
                      group.equal = c("thresholds", "loadings"))

tlOut_weak_full_parents <- cfa(model = as.character(tlMod), 
            data = imp_attachment,
            group = "parents",
            ordered = TRUE,
            estimator = 'WLSMV')

fitMeasures(tlOut_weak_full_parents)
save(tlOut_weak_full_parents, file = "tlOut_weak_full_parents")

anova_weak_parents <- anova(tOut_full_attachment_parents, tlOut_weak_full_parents)
save(anova_weak_parents , file = "anova_weak_parents")

#-------------------------------------------------------------------------------
# children
tlMod_full_children <- measEq.syntax(configural.model = fit_configural_children_attachment,
                              data = imp_attachment,
                              ordered = TRUE,
                              parameterization = "delta",
                              ID.fac = "std.lv",
                              ID.cat = "Wu.Estabrook.2016",
                              group = "children",
                              group.equal = c("thresholds", "loadings"))

tlOut_full_children <- cfa(model = as.character(tlMod_full_children ), 
                             data = imp_attachment,
                             group = "children",
                             ordered = TRUE,
                             estimator = 'WLSMV')
fitMeasures(tlOut_full_children)
save(tlOut_full_children, file = "tlOut_full_children")

anova_weak_children <- anova(tOut_full_children, tlOut_full_children)
save(anova_weak_children, file = "anova_weak_children")

#-------------------------------------------------------------------------------
# similar
tlMod_full_same <- measEq.syntax(configural.model = fit_configural_similar_attachment,
                              data = imp_attachment,
                              ordered = TRUE,
                              parameterization = "delta",
                              ID.fac = "std.lv",
                              ID.cat = "Wu.Estabrook.2016",
                              group = "similar",
                              group.equal = c("thresholds", "loadings"))

tlOut_full_similar <- cfa(model = as.character(tlMod_full_same), 
                            data = imp_attachment,
                            group = "similar",
                            ordered = TRUE,
                            estimator = 'WLSMV')
fitMeasures(tlOut_full_similar)
save(tlOut_full_similar, file = "tlOut_full_similar")

anova_weak_similar <- anova(tOut_full_same, tlOut_full_similar)
save(anova_weak_similar, file = "anova_weak_similar")


####
#### Strong invariance
####

#-------------------------------------------------------------------------------
# parents
tliMod_full <- measEq.syntax(configural.model = fit_configural_parent_attachment,
                               data = imp_attachment,
                               ordered = TRUE,
                               parameterization = "delta",
                               ID.fac = "std.lv",
                               ID.cat = "Wu.Estabrook.2016",
                               group = "parents",
                               group.equal = c("thresholds", "loadings", "intercepts"))

tliOut_full_parents <- cfa(model = as.character(tliMod_full), 
                             data = imp_attachment,
                             group = "parents",
                             ordered = TRUE,
                             estimator = 'WLSMV')
fitMeasures(tliOut_full_parents)
save(tliOut_full_parents, file = "tliOut_full_parents_attachment")

anova_strong_parents <- anova(tlOut_weak_full_parents, tliOut_full_parents)
save(anova_strong_parents, file = "anova_strong_parents")

#-------------------------------------------------------------------------------
# children
tliMod_full_children <- measEq.syntax(configural.model = fit_configural_children_attachment,
                               data = imp_attachment,
                               ordered = TRUE,
                               parameterization = "delta",
                               ID.fac = "std.lv",
                               ID.cat = "Wu.Estabrook.2016",
                               group = "children",
                               group.equal = c("thresholds", "loadings", "intercepts"))

tliOut_full_children <- cfa(model = as.character(tliMod_full_children), 
                              data = imp_attachment,
                              group = "children",
                              ordered = TRUE,
                              estimator = 'WLSMV')
fitMeasures(tliOut_full_children)
save(tliOut_full_children, file = "tliOut_full_children")

anova_strong_children <- anova(tlOut_full_children, tliOut_full_children)
save(anova_strong_children, file = "anova_strong_children")

#-------------------------------------------------------------------------------
# similar
tliMod_full_same <- measEq.syntax(configural.model = fit_configural_similar_attachment,
                               data = imp_attachment,
                               ordered = TRUE,
                               parameterization = "delta",
                               ID.fac = "std.lv",
                               ID.cat = "Wu.Estabrook.2016",
                               group = "similar",
                               group.equal = c("thresholds", "loadings", "intercepts"))

tliOut_full_similar <- cfa(model = as.character(tliMod_full_same), 
                             data = imp_attachment,
                             group = "similar",
                             ordered = TRUE,
                             estimator = 'WLSMV')
fitMeasures(tliOut_full_similar )
save(tliOut_full_similar, file = "tliOut_full_similar")

anova_strong_similar <- anova(tlOut_full_similar, tliOut_full_similar)
save(anova_strong_similar, file = "anova_strong_similar")

# End of table A2
################################################################################

#-------------------------------------------------------------------------------
# Threshold, weak and strong Invariance
# Problems
# Table A3 manuscript 
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# parents
# Weak invariance
tlMod_problems <- measEq.syntax(configural.model = fit_configural_parents_problems,
                       data = imp_problems,
                       ordered = TRUE,
                       parameterization = "delta",
                       ID.fac = "std.lv",
                       ID.cat = "Wu.Estabrook.2016",
                       group = "parents",
                       group.equal = c("thresholds", "loadings"))

tlOut_problems_parents <- cfa(model = as.character(tlMod_problems), 
             data = imp_problems,
             group = "parents",
             ordered = TRUE,
             estimator = 'WLSMV')

fitMeasures(tlOut_problems_parents)

# Strong invariance
tliMod_problems <- measEq.syntax(configural.model = fit_configural_parents_problems,
                                 data = imp_problems,
                                 ordered = TRUE,
                                 parameterization = "delta",
                                 ID.fac = "std.lv",
                                 ID.cat = "Wu.Estabrook.2016",
                                 group = "parents",
                                 group.equal = c("thresholds", "loadings", "intercepts"))

tliOut_problems_parents <- cfa(model = as.character(tliMod_problems), 
                       data = imp_problems,
                       group = "parents",
                       ordered = TRUE,
                       estimator = 'WLSMV')

fitMeasures(tliOut_problems_parents)

anova_problems_parents <- anova(fit_configural_parents_problems, tlOut_problems_parents, tliOut_problems_parents)
save(anova_problems_parents, file = "anova_problems_parents")

#-------------------------------------------------------------------------------
# children
# Weak invariance
tlMod_problems <- measEq.syntax(configural.model = fit_configural_children_problems,
                                data = imp_problems,
                                ordered = TRUE,
                                parameterization = "delta",
                                ID.fac = "std.lv",
                                ID.cat = "Wu.Estabrook.2016",
                                group = "children",
                                group.equal = c("thresholds", "loadings"))

tlOut_problems_children <- cfa(model = as.character(tlMod_problems), 
                              data = imp_problems,
                              group = "children",
                              ordered = TRUE,
                              estimator = 'WLSMV')

fitMeasures(tlOut_problems_children)

# Strong invariance
tliMod_problems <- measEq.syntax(configural.model = fit_configural_children_problems,
                                 data = imp_problems,
                                 ordered = TRUE,
                                 parameterization = "delta",
                                 ID.fac = "std.lv",
                                 ID.cat = "Wu.Estabrook.2016",
                                 group = "children",
                                 group.equal = c("thresholds", "loadings", "intercepts"))

tliOut_problems_children <- cfa(model = as.character(tliMod_problems), 
                               data = imp_problems,
                               group = "children",
                               ordered = TRUE,
                               estimator = 'WLSMV')

fitMeasures(tliOut_problems_children)

anova_problems_children <- anova(fit_configural_children_problems, tlOut_problems_children, tliOut_problems_children)
save(anova_problems_children, file = "anova_problems_children")

#-------------------------------------------------------------------------------
# similar
# Weak invariance
tlMod_problems <- measEq.syntax(configural.model = fit_configural_similar_problems,
                                data = imp_problems,
                                ordered = TRUE,
                                parameterization = "delta",
                                ID.fac = "std.lv",
                                ID.cat = "Wu.Estabrook.2016",
                                group = "similar",
                                group.equal = c("thresholds", "loadings"))

tlOut_problems_similar <- cfa(model = as.character(tlMod_problems), 
                               data = imp_problems,
                               group = "similar",
                               ordered = TRUE,
                               estimator = 'WLSMV')

fitMeasures(tlOut_problems_similar)

# Strong invariance
tliMod_problems <- measEq.syntax(configural.model = fit_configural_similar_problems,
                                 data = imp_problems,
                                 ordered = TRUE,
                                 parameterization = "delta",
                                 ID.fac = "std.lv",
                                 ID.cat = "Wu.Estabrook.2016",
                                 group = "similar",
                                 group.equal = c("thresholds", "loadings", "intercepts"))

tliOut_problems_similar <- cfa(model = as.character(tliMod_problems), 
                                data = imp_problems,
                                group = "similar",
                                ordered = TRUE,
                                estimator = 'WLSMV')

fitMeasures(tliOut_problems_similar)

anova_problems_similar <- anova(fit_configural_similar_problems, tlOut_problems_similar, tliOut_problems_similar)
save(anova_problems_similar, file = "anova_problems_similar")
