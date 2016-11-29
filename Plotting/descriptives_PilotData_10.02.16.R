

pptData <- tempData[!duplicated(tempData$ID),]

cond1 <- pptData[pptData$CONDITION==1,]
nrow(cond1)
cond2 <- pptData[pptData$CONDITION==3,]
nrow(cond2)
control <- pptData[pptData$CONDITION==2,]
nrow(control)
nrow(pptData)

cond1male <- cond1[cond1$SEX==1,]
nrow(cond1male)
cond1female <- cond1[cond1$SEX==2,]
nrow(cond1female)

cond2male <- cond2[cond2$SEX==1,]
nrow(cond2male)
cond2female <- cond2[cond2$SEX==2,]
nrow(cond2female)

controlMale <- control[control$SEX==1,]
nrow(controlMale)
controlFemale <- control[control$SEX==2,]
nrow(controlFemale)
