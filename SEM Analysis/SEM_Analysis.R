#Main Analysis Attachment and Problem behavior
#Author: Magda Matetovici

#Packages
#install.packages("lavaan")
#install.packages("semTools")
#install.packages("mice")
#install.package("tidyverse")
#install.package("ggplot2")
#install.package("jtools)

#Load packages
library(lavaan)
library(semTools)
library(mice)
library(tidyverse)
library(ggplot2)
library(jtools)

# Source models definitions
source("Model_Definitions_Parent_Child_Dyads.R")

# Put the data from the two csv files together
data_attachment_recoded<- read.csv(file = "data_attachment_original_recoded.csv")
data_sdq_recoded <- read.csv(file = "data_sdq_original_recoded.csv")

data_groups <- cbind(data_attachment_recoded[, 49:97], data_sdq_recoded[, 23:42])

#Make everything orderded factors

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



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Internalising Problems
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Models Internalising Parent Gender
#-------------------------------------------------------------------------------

# Moderation of Parent Gender Internalising Secure Model

fit_model_internalising_secure <- sem(model = model_internalising_secure, data = imp_groups,
                                        group= "parents",estimator = "WLSMV", group.equal = c("thresholds", "loadings"))

summary(fit_model_internalising_secure)
fitMeasures(fit_model_internalising_secure)

#correlations between latent variables 
fit_model_internalising_secure_all <- sem(model = model_internalising_secure, data = imp_groups,
                                     estimator = "WLSMV")

standardized_estimates <- standardizedSolution(fit_model_internalising_secure_all)


fit_model_internalising_secure_equal <- sem(model = model_internalising_secure_equal, data = imp_groups,
                                              group= "parents",estimator = "WLSMV", group.equal = c("thresholds", "loadings"))

summary(fit_model_internalising_secure_equal)
fitMeasures(fit_model_internalising_secure_equal)

anova(fit_model_internalising_secure, fit_model_internalising_secure_equal)

# Moderation of Parent Gender Internalising Avoidant Model

fit_model_internalising_avoidant <- sem(model = model_internalising_avoidant, data = imp_groups,
                                      group= "parents",estimator = "WLSMV", group.equal = c("thresholds", "loadings"))

summary(fit_model_internalising_avoidant)
fitMeasures(fit_model_internalising_avoidant)

#correlations between latent variables 
fit_model_internalising_avoidant_all <- sem(model = model_internalising_avoidant, data = imp_groups,
                                          estimator = "WLSMV")

standardized_estimates <- standardizedSolution(fit_model_internalising_avoidant_all)

fit_model_internalising_avoidant_equal <- sem(model = model_internalising_avoidant_equal, data = imp_groups,
                                            group= "parents",estimator = "WLSMV", group.equal = c("thresholds", "loadings"))

summary(fit_model_internalising_avoidant_equal)
fitMeasures(fit_model_internalising_avoidant_equal)

anova(fit_model_internalising_avoidant, fit_model_internalising_avoidant_equal)

# Moderation of Parent Gender Internalising Ambivalent Model

fit_model_internalising_ambivalent <- sem(model = model_internalising_ambivalent, data = imp_groups,
                                        group= "parents",estimator = "WLSMV", group.equal = c("thresholds", "loadings"))

summary(fit_model_internalising_ambivalent)
fitMeasures(fit_model_internalising_ambivalent)

#correlations between latent variables 
fit_model_internalising_ambivalent_all <- sem(model = model_internalising_ambivalent, data = imp_groups,
                                            estimator = "WLSMV")

standardized_estimates <- standardizedSolution(fit_model_internalising_ambivalent_all)


fit_model_internalising_ambivalent_equal <- sem(model = model_internalising_ambivalent_equal, data = imp_groups,
                                              group= "parents",estimator = "WLSMV", group.equal = c("thresholds", "loadings"))

summary(fit_model_internalising_ambivalent_equal)
fitMeasures(fit_model_internalising_ambivalent_equal)

anova(fit_model_internalising_ambivalent, fit_model_internalising_ambivalent_equal)

# Moderation of Parent Gender Internalising Disorganised Model

fit_model_internalising_disorganised <- sem(model = model_internalising_disorganised , data = imp_groups,
                                          group= "parents",estimator = "WLSMV", group.equal = c("thresholds", "loadings"))

summary(fit_model_internalising_disorganised )
fitMeasures(fit_model_internalising_disorganised)

#correlations
fit_model_internalising_disorganised_all <- sem(model = model_internalising_disorganised, data = imp_groups,
                                              estimator = "WLSMV")

standardized_estimates <- standardizedSolution(fit_model_internalising_disorganised_all)

fit_model_internalising_disorganised_equal <- sem(model = model_internalising_disorganised_equal, data = imp_groups,
                                                group= "parents",estimator = "WLSMV", group.equal = c("thresholds", "loadings"))

summary(fit_model_internalising_disorganised_equal)
fitMeasures(fit_model_internalising_disorganised_equal)

anova(fit_model_internalising_disorganised, fit_model_internalising_disorganised_equal)

#-------------------------------------------------------------------------------
# Models Internalising Same vs Different gender
#-------------------------------------------------------------------------------
# Moderation of Same Gender Internalising Secure Model

fit_model_internalising_secure <- sem(model = model_internalising_secure, data = imp_groups,
                                      group= "similar",estimator = "WLSMV", group.equal = c("thresholds", "loadings", "intercepts"))

summary(fit_model_internalising_secure)
fitMeasures(fit_model_internalising_secure)

fit_model_internalising_secure_equal <- sem(model = model_internalising_secure_equal, data = imp_groups,
                                            group= "similar",estimator = "WLSMV", group.equal = c("thresholds", "loadings", "intercepts"))

summary(fit_model_internalising_secure_equal)
fitMeasures(fit_model_internalising_secure_equal)

anova(fit_model_internalising_secure, fit_model_internalising_secure_equal)

# Moderation of Same Gender Internalising Avoidant Model

fit_model_internalising_avoidant <- sem(model = model_internalising_avoidant, data = imp_groups,
                                        group= "similar",estimator = "WLSMV", group.equal = c("thresholds", "loadings"))

summary(fit_model_internalising_avoidant)
output <- capture.output(summary(fit_model_internalising_avoidant, fit.measures=TRUE))
selected_lines <- output[grep("same|different|Variances|intercept|thresh|Estimates|internalising|avodnt_ttchmnt", output, ignore.case = TRUE)]
cat(selected_lines, sep="\n")

fitMeasures(fit_model_internalising_avoidant)

fit_model_internalising_avoidant_equal <- sem(model = model_internalising_avoidant_equal, data = imp_groups,
                                              group= "similar",estimator = "WLSMV", group.equal = c("thresholds", "loadings"))

summary(fit_model_internalising_avoidant_equal)
fitMeasures(fit_model_internalising_avoidant_equal)

anova(fit_model_internalising_avoidant, fit_model_internalising_avoidant_equal)

#Plot the slopes in the two groups

## Generate factor scores:
tmp <- predict(fit_model_internalising_avoidant)
## Stack factor scores into a "tidy" dataset:
pData <- data.frame(do.call(rbind, tmp),
                    group = rep(names(tmp), sapply(tmp, nrow))
)
## Create a simple slopes plot:
ssPlot_internalizing_avoidant_attachment <- ggplot(pData, aes(avoidant_attachment, internalising, color = group)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm") +
  theme_apa(legend.pos = "bottomright",) + 
  theme(panel.border = element_blank(),
        axis.line = element_line(color = "black")) +
  labs(y = "Internalizing Problems", x = "Avoidant Attachment") + 
  scale_color_manual(
    name = "Group",
    values = c("different" = "red", "same" = "blue"),
    labels = c("Different gender", "Same gender")
  )
ssPlot_internalizing_avoidant_attachment
ggsave("ssPlot_internalizing_avoidant_attachment.png", width = 6, height = 4, dpi = 300)

# Moderation of same Gender Internalising Ambivalent Model

fit_model_internalising_ambivalent <- sem(model = model_internalising_ambivalent, data = imp_groups,
                                          group= "similar",estimator = "WLSMV", group.equal = c("thresholds", "loadings"))

summary(fit_model_internalising_ambivalent)
fitMeasures(fit_model_internalising_ambivalent)

fit_model_internalising_ambivalent_equal <- sem(model = model_internalising_ambivalent_equal, data = imp_groups,
                                                group= "similar",estimator = "WLSMV", group.equal = c("thresholds", "loadings"))

summary(fit_model_internalising_ambivalent_equal)
fitMeasures(fit_model_internalising_ambivalent_equal)

anova(fit_model_internalising_ambivalent, fit_model_internalising_ambivalent_equal)

# Moderation of Same Gender Internalising Disorganised Model

fit_model_internalising_disorganised <- sem(model = model_internalising_disorganised , data = imp_groups,
                                            group= "similar",estimator = "WLSMV", group.equal = c("thresholds", "loadings", "intercepts"))

summary(fit_model_internalising_disorganised )
fitMeasures(fit_model_internalising_disorganised)

output <- capture.output(summary(fit_model_internalising_disorganised, fit.measures=TRUE))
selected_lines <- output[grep("same|different|Variances|intercept|thresh|Estimates|internalising|dsrgnsd_ttchmn", output, ignore.case = TRUE)]
cat(selected_lines, sep="\n")

fit_model_internalising_disorganised_equal <- sem(model = model_internalising_disorganised_equal, data = imp_groups,
                                                  group= "similar",estimator = "WLSMV", group.equal = c("thresholds", "loadings", "intercepts"))

summary(fit_model_internalising_disorganised_equal)
fitMeasures(fit_model_internalising_disorganised_equal)

anova(fit_model_internalising_disorganised, fit_model_internalising_disorganised_equal)

#Graph the slopes
#Plot the slopes in the two groups

## Generate factor scores:
tmp <- predict(fit_model_internalising_disorganised)
## Stack factor scores into a "tidy" dataset:
pData <- data.frame(do.call(rbind, tmp),
                    group = rep(names(tmp), sapply(tmp, nrow))
)
## Create a simple slopes plot:
ssPlot_internalizing_disorganized_attachment <- ggplot(pData, aes(disorganised_attachment, internalising, color = group)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm") +
  theme_apa(legend.pos = "bottomright",) + 
  theme(panel.border = element_blank(),
        axis.line = element_line(color = "black")) +
  labs(y = "Internalizing Problems", x = "Disorganized Attachment") + 
  scale_color_manual(
    name = "Group",
    values = c("different" = "red", "same" = "blue"),
    labels = c("Different gender", "Same gender")
  )
ssPlot_internalizing_disorganized_attachment
ggsave("ssPlot_internalizing_disorganized_attachment.png", width = 6, height = 4, dpi = 300)

#-------------------------------------------------------------------------------
#Models Internalising Children gender
#-------------------------------------------------------------------------------
# Moderation of Child Gender Internalising Secure Model

fit_model_internalising_secure <- sem(model = model_internalising_secure, data = imp_groups,
                                      group= "children",estimator = "WLSMV", group.equal = c("thresholds", "loadings", "intercepts"))

summary(fit_model_internalising_secure)
fitMeasures(fit_model_internalising_secure)

fit_model_internalising_secure_equal <- sem(model = model_internalising_secure_equal, data = imp_groups,
                                            group= "children",estimator = "WLSMV", group.equal = c("thresholds", "loadings", "intercepts"))

summary(fit_model_internalising_secure_equal)
fitMeasures(fit_model_internalising_secure_equal)

anova(fit_model_internalising_secure, fit_model_internalising_secure_equal)

# Moderation of Child Gender Internalising Avoidant Model

fit_model_internalising_avoidant <- sem(model = model_internalising_avoidant, data = imp_groups,
                                        group= "children",estimator = "WLSMV", group.equal = c("thresholds", "loadings", "intercepts"))

summary(fit_model_internalising_avoidant)
fitMeasures(fit_model_internalising_avoidant)

fit_model_internalising_avoidant_equal <- sem(model = model_internalising_avoidant_equal, data = imp_groups,
                                              group= "children",estimator = "WLSMV", group.equal = c("thresholds", "loadings", "intercepts"))

summary(fit_model_internalising_avoidant_equal)
fitMeasures(fit_model_internalising_avoidant_equal)

anova(fit_model_internalising_avoidant, fit_model_internalising_avoidant_equal)

# Moderation of Child Gender Internalising Ambivalent Model

fit_model_internalising_ambivalent <- sem(model = model_internalising_ambivalent, data = imp_groups,
                                          group= "children",estimator = "WLSMV", group.equal = c("thresholds", "loadings"))

summary(fit_model_internalising_ambivalent)
fitMeasures(fit_model_internalising_ambivalent)

fit_model_internalising_ambivalent_equal <- sem(model = model_internalising_ambivalent_equal, data = imp_groups,
                                                group= "children",estimator = "WLSMV", group.equal = c("thresholds", "loadings"))

summary(fit_model_internalising_ambivalent_equal)
fitMeasures(fit_model_internalising_ambivalent_equal)

anova(fit_model_internalising_ambivalent, fit_model_internalising_ambivalent_equal)

# Moderation of Child Gender Internalising Disorganised Model

fit_model_internalising_disorganised <- sem(model = model_internalising_disorganised , data = imp_groups,
                                            group= "children",estimator = "WLSMV",parameterization = "delta", group.equal = c("thresholds", "loadings", "intercepts"))

summary(fit_model_internalising_disorganised)
fitMeasures(fit_model_internalising_disorganised)

fit_model_internalising_disorganised_equal <- sem(model = model_internalising_disorganised_equal, data = imp_groups,
                                                  group= "children",estimator = "WLSMV", group.equal = c("thresholds", "loadings", "intercepts"))

summary(fit_model_internalising_disorganised_equal)
fitMeasures(fit_model_internalising_disorganised_equal)

anova(fit_model_internalising_disorganised, fit_model_internalising_disorganised_equal)


fit_model_internalising_disorganised_children <- sem(model = model_internalising_disorganised_children , data = imp_groups,
                                            group= "children",estimator = "WLSMV",parameterization = "delta", group.equal = c("thresholds", "loadings", "intercepts"))

fit_model_internalising_disorganised_equal_children <- sem(model = model_internalising_disorganised_equal_children, data = imp_groups,
                                                  group= "children",estimator = "WLSMV", group.equal = c("thresholds", "loadings", "intercepts"))
anova(fit_model_internalising_disorganised_children, fit_model_internalising_disorganised_equal_children)
summary(fit_model_internalising_disorganised_children)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Externalising Models
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Moderation of Parent Gender Externalising Secure Model

fit_model_externalising_secure <- sem(model = model_externalising_secure, data = imp_groups,
                                      group= "parents",estimator = "WLSMV", group.equal = c("thresholds", "loadings"))

summary(fit_model_externalising_secure)
fitMeasures(fit_model_externalising_secure)

#correlations

fit_model_externalising_secure_all <- sem(model = model_externalising_secure, data = imp_groups,
                                                estimator = "WLSMV")

standardized_estimates <- standardizedSolution(fit_model_externalising_secure_all)

fit_model_externalising_secure_equal <- sem(model = model_externalising_secure_equal, data = imp_groups,
                                            group= "parents",estimator = "WLSMV", group.equal = c("thresholds", "loadings"))

summary(fit_model_externalising_secure_equal)
fitMeasures(fit_model_externalising_secure_equal)

anova(fit_model_externalising_secure, fit_model_externalising_secure_equal)

# Moderation of Parent Gender Externalising Avoidant Model

fit_model_externalising_avoidant <- sem(model = model_externalising_avoidant, data = imp_groups,
                                        group= "parents",estimator = "WLSMV", group.equal = c("thresholds", "loadings"))

summary(fit_model_externalising_avoidant)
fitMeasures(fit_model_externalising_avoidant)

#corelations
fit_model_externalising_avoidant_all <- sem(model = model_externalising_avoidant, data = imp_groups,
                                          estimator = "WLSMV")

standardized_estimates <- standardizedSolution(fit_model_externalising_avoidant_all)

fit_model_externalising_avoidant_equal <- sem(model = model_externalising_avoidant_equal, data = imp_groups,
                                              group= "parents",estimator = "WLSMV", group.equal = c("thresholds", "loadings"))

summary(fit_model_externalising_avoidant_equal)
fitMeasures(fit_model_externalising_avoidant_equal)

anova(fit_model_externalising_avoidant, fit_model_externalising_avoidant_equal)

# Moderation of Parent Gender Externalising Ambivalent Model

fit_model_externalising_ambivalent <- sem(model = model_externalising_ambivalent, data = imp_groups,
                                          group= "parents",estimator = "WLSMV", group.equal = c("thresholds", "loadings"))

summary(fit_model_externalising_ambivalent)
fitMeasures(fit_model_externalising_ambivalent)

#corelations
fit_model_externalising_ambivalent_all <- sem(model = model_externalising_ambivalent, data = imp_groups,
                                            estimator = "WLSMV")

standardized_estimates <- standardizedSolution(fit_model_externalising_ambivalent_all)

fit_model_externalising_ambivalent_equal <- sem(model = model_externalising_ambivalent_equal, data = imp_groups,
                                                group= "parents",estimator = "WLSMV", group.equal = c("thresholds", "loadings"))

summary(fit_model_externalising_ambivalent_equal)
fitMeasures(fit_model_externalising_ambivalent_equal)

anova(fit_model_externalising_ambivalent, fit_model_externalising_ambivalent_equal)

# Moderation of Parent Gender Externalising Disorganised Model

fit_model_externalising_disorganised <- sem(model = model_externalising_disorganised , data = imp_groups,
                                            group= "parents",estimator = "WLSMV", group.equal = c("thresholds", "loadings"))

summary(fit_model_externalising_disorganised )
fitMeasures(fit_model_externalising_disorganised)

#corrlations between latent var 
fit_model_externalising_disorganised_all <- sem(model = model_externalising_disorganised, data = imp_groups,
                                              estimator = "WLSMV")
params <- parameterEstimates(fit_model_externalising_disorganised_all)

standardized_estimates <- standardizedSolution(fit_model_externalising_disorganised_all)


fit_model_externalising_disorganised_equal <- sem(model = model_externalising_disorganised_equal, data = imp_groups,
                                                  group= "parents",estimator = "WLSMV", group.equal = c("thresholds", "loadings"))

summary(fit_model_externalising_disorganised_equal)
fitMeasures(fit_model_externalising_disorganised_equal)

anova(fit_model_externalising_disorganised, fit_model_externalising_disorganised_equal)

#-------------------------------------------------------------------------------
# Model Externalising Same vs Different gender
#-------------------------------------------------------------------------------
# Moderation of Same Gender Externalising Secure Model

fit_model_externalising_secure <- sem(model = model_externalising_secure, data = imp_groups,
                                      group= "similar",estimator = "WLSMV", group.equal = c("thresholds", "loadings", "intercepts"))

summary(fit_model_externalising_secure)
fitMeasures(fit_model_externalising_secure)

fit_model_externalising_secure_equal <- sem(model = model_externalising_secure_equal, data = imp_groups,
                                            group= "similar",estimator = "WLSMV", group.equal = c("thresholds", "loadings", "intercepts"))

summary(fit_model_externalising_secure_equal)
fitMeasures(fit_model_externalising_secure_equal)

anova(fit_model_externalising_secure, fit_model_externalising_secure_equal)

# Moderation of Same Gender Externalising Avoidant Model

fit_model_externalising_avoidant <- sem(model = model_externalising_avoidant, data = imp_groups,
                                        group= "similar",estimator = "WLSMV", group.equal = c("thresholds", "loadings", "intercepts"))

summary(fit_model_externalising_avoidant)
fitMeasures(fit_model_externalising_avoidant)

fit_model_externalising_avoidant_equal <- sem(model = model_externalising_avoidant_equal, data = imp_groups,
                                              group= "similar",estimator = "WLSMV", group.equal = c("thresholds", "loadings", "intercepts"))

summary(fit_model_externalising_avoidant_equal)
fitMeasures(fit_model_externalising_avoidant_equal)

anova(fit_model_externalising_avoidant, fit_model_externalising_avoidant_equal)

# Moderation of same Gender Externalising Ambivalent Model

fit_model_externalising_ambivalent <- sem(model = model_externalising_ambivalent, data = imp_groups,
                                          group= "similar",estimator = "WLSMV", group.equal = c("thresholds", "loadings", "intercepts"))

summary(fit_model_externalising_ambivalent)
fitMeasures(fit_model_externalising_ambivalent)

fit_model_externalising_ambivalent_equal <- sem(model = model_externalising_ambivalent_equal, data = imp_groups,
                                                group= "similar",estimator = "WLSMV", group.equal = c("thresholds", "loadings", "intercepts"))

summary(fit_model_externalising_ambivalent_equal)
fitMeasures(fit_model_externalising_ambivalent_equal)

anova(fit_model_externalising_ambivalent, fit_model_externalising_ambivalent_equal)

# Moderation of Same Gender Externalising Disorganised Model

fit_model_externalising_disorganised <- sem(model = model_externalising_disorganised , data = imp_groups,
                                            group= "similar",estimator = "WLSMV", group.equal = c("thresholds", "loadings", "intercepts"))

summary(fit_model_externalising_disorganised )
fitMeasures(fit_model_externalising_disorganised)

fit_model_externalising_disorganised_equal <- sem(model = model_externalising_disorganised_equal, data = imp_groups,
                                                  group= "similar",estimator = "WLSMV", group.equal = c("thresholds", "loadings", "intercepts"))

summary(fit_model_externalising_disorganised_equal)
fitMeasures(fit_model_externalising_disorganised_equal)

anova(fit_model_externalising_disorganised, fit_model_externalising_disorganised_equal)

#-------------------------------------------------------------------------------
#Models Externalising Children gender
#-------------------------------------------------------------------------------
# Moderation of Child Gender Externalising Secure Model

fit_model_externalising_secure <- sem(model = model_externalising_secure, data = imp_groups,
                                      group= "children",estimator = "WLSMV", group.equal = c("thresholds", "loadings", "intercepts"))

summary(fit_model_externalising_secure)
fitMeasures(fit_model_externalising_secure)



fit_model_externalising_secure_equal <- sem(model = model_externalising_secure_equal, data = imp_groups,
                                            group= "children",estimator = "WLSMV", group.equal = c("thresholds", "loadings", "intercepts"))

summary(fit_model_externalising_secure_equal)
fitMeasures(fit_model_externalising_secure_equal)

output <- capture.output(summary(fit_model_externalising_secure, fit.measures=TRUE))
selected_lines <- output[grep("son|daughter|Variances|intercept|thresh|Estimates|externalising|secure_ttchmnt", output, ignore.case = TRUE)]
cat(selected_lines, sep="\n")

anova(fit_model_externalising_secure, fit_model_externalising_secure_equal)

#Plots

#Plot the slopes in the two groups

## Generate factor scores:
tmp <- predict(fit_model_externalising_secure)
## Stack factor scores into a "tidy" dataset:
pData <- data.frame(do.call(rbind, tmp),
                    group = rep(names(tmp), sapply(tmp, nrow))
)
## Create a simple slopes plot:
ssPlot_externalizing_secure_attachment <- ggplot(pData, aes(secure_attachment, externalising, color = group)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm") +
  theme_apa(legend.pos = "topright",) + 
  theme(panel.border = element_blank(),
        axis.line = element_line(color = "black")) +
  labs(y = "Externalizing Problems", x = "Secure Attachment") + 
  scale_color_manual(
    name = "Group",
    values = c("daughter" = "darkgreen", "son" = "yellow"),
    labels = c("Daughter", "Son")
  )
ssPlot_externalizing_secure_attachment
ggsave("ssPlot_externalizing_secure_attachment.png", width = 6, height = 4, dpi = 300)

# Moderation of Child Gender Externalising Avoidant Model

fit_model_externalising_avoidant <- sem(model = model_externalising_avoidant, data = imp_groups,
                                        group= "children",estimator = "WLSMV", group.equal = c("thresholds", "loadings", "intercepts"))

summary(fit_model_externalising_avoidant)
fitMeasures(fit_model_externalising_avoidant)

output <- capture.output(summary(fit_model_externalising_avoidant, fit.measures=TRUE))
selected_lines <- output[grep("son|daughter|Variances|intercept|thresh|Estimates|externalising|avodnt_ttchmnt", output, ignore.case = TRUE)]
cat(selected_lines, sep="\n")

fit_model_externalising_avoidant_equal <- sem(model = model_externalising_avoidant_equal, data = imp_groups,
                                              group= "children",estimator = "WLSMV", group.equal = c("thresholds", "loadings", "intercepts"))

summary(fit_model_externalising_avoidant_equal)
fitMeasures(fit_model_externalising_avoidant_equal)

anova(fit_model_externalising_avoidant, fit_model_externalising_avoidant_equal)

#Plot the slopes 


## Generate factor scores:
tmp <- predict(fit_model_externalising_avoidant)
## Stack factor scores into a "tidy" dataset:
pData <- data.frame(do.call(rbind, tmp),
                    group = rep(names(tmp), sapply(tmp, nrow))
)
## Create a simple slopes plot:
ssPlot_externalizing_avoidant_attachment <- ggplot(pData, aes(avoidant_attachment, externalising, color = group)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm") +
  theme_apa(legend.pos = "bottomright",) + 
  theme(panel.border = element_blank(),
        axis.line = element_line(color = "black")) +
  labs(y = "Externalizing Problems", x = "Avoidant Attachment") + 
  scale_color_manual(
    name = "Group",
    values = c("daughter" = "darkgreen", "son" = "yellow"),
    labels = c("Daughter", "Son")
  )
ssPlot_externalizing_avoidant_attachment
ggsave("ssPlot_externalizing_avoidant_attachment.png", width = 6, height = 4, dpi = 300)


# Moderation of Child Gender Externalising Ambivalent Model

fit_model_externalising_ambivalent <- sem(model = model_externalising_ambivalent, data = imp_groups,
                                          group= "children",estimator = "WLSMV", group.equal = c("thresholds", "loadings", "intercepts"))

summary(fit_model_externalising_ambivalent)
fitMeasures(fit_model_externalising_ambivalent)

fit_model_externalising_ambivalent_equal <- sem(model = model_externalising_ambivalent_equal, data = imp_groups,
                                                group= "children",estimator = "WLSMV", group.equal = c("thresholds", "loadings", "intercepts"))

summary(fit_model_externalising_ambivalent_equal)
fitMeasures(fit_model_externalising_ambivalent_equal)

anova(fit_model_externalising_ambivalent, fit_model_externalising_ambivalent_equal)

# Moderation of Child Gender Externalising Disorganised Model

fit_model_externalising_disorganised <- sem(model = model_externalising_disorganised , data = imp_groups,
                                            group= "children",estimator = "WLSMV", group.equal = c("thresholds", "loadings", "intercepts"))

summary(fit_model_externalising_disorganised)
fitMeasures(fit_model_externalising_disorganised)

fit_model_externalising_disorganised_equal <- sem(model = model_externalising_disorganised_equal, data = imp_groups,
                                                  group= "children",estimator = "WLSMV", group.equal = c("thresholds", "loadings", "intercepts"))

summary(fit_model_externalising_disorganised_equal)
fitMeasures(fit_model_externalising_disorganised_equal)

anova(fit_model_externalising_disorganised, fit_model_externalising_disorganised_equal)
