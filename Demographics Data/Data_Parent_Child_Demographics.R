#Demographics Data
#Author: Magda Matetovici
#install.packages("foreign")
#Load the data
library(foreign)


data <- read.spss("C:/Users/matet/Desktop/Parent and Child Dyad/data_pcd.sav", to.data.frame=TRUE)

# Make the variable dyad from the gender of the child and the gender of the parents
# Conding for the dyads: female - meisje (1), female-jongen (2), male - meisje (3), male-jongen (4)

data$A_parentgender<- as.character(data$A_parentgender)

data$A_childgender <- as.character(data$A_childgender)


dyad <- c()

for (i in 1: length(data$A_parentgender)){
  if (data$A_parentgender[i] == "female" & data$A_childgender[i] == "meisje"){
    dyad <- c(dyad, 1)
  }
  else if (data$A_parentgender[i] == "female" & data$A_childgender[i] == "jongen"){
    dyad <- c(dyad, 2)
  } 
  else if (data$A_parentgender[i] == "male" & data$A_childgender[i] == "meisje"){
    dyad <- c(dyad, 3)
  }
  else {
    dyad <- c(dyad, 4)
  }
}

data$dyad <- dyad
data$dyad_name <- factor(data$dyad, labels = c("mother_daughter", "mother_son", "father_daughter", "father_son"))

# Obtain the size of each group

table(data$A_parentgender)
table(data$dyad_name)

# Obtain the mean and standard deviation of parents and children
mean(as.numeric(data$A_parentage), na.rm = TRUE)
sd(as.numeric(data$A_parentage), na.rm = TRUE)

mean(as.numeric(data$A_childage), na.rm = TRUE)
sd(as.numeric(data$A_childage), na.rm = TRUE)



