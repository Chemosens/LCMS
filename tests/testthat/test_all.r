# Loading functions and libraries
#================================
library(MSnbase)
library(data.table)
library(openxlsx)
library(ggplot2)
library(LCMS)
#source("C:/Users/CalculCaro/Desktop/lcmsFunctions2.r")
#
# Setting parameters
#======================
# setwd("C:/INRA/LCMS/tests/testthat")
repoData="./../../extdata"
# Where are the .xml ?
reposaveQC="./../../tmp/centroid"
# Which files to use ?
qcFiles=c("CSCQ005.mzXML","CSCQ006.mzXML","CSCQ007.mzXML")
# Which integration table to use ?
integrationTable=read.xlsx(paste0(repoData,"/hilic_neg_table111022.xlsx"))
colnames(integrationTable)=c("class","name","compo","mz","std")
# Which class table to use ?
classTable=read.xlsx("./../../extdata/rt_hilic_NEG.xlsx")


# Is there duplicates in integration Table
checkingIntegrationTable(integrationTable,duplicatedName="name")

# Are the class table and the integration table containing the same class names ?
checkingClassTable(integrationTable=integrationTable,classTable=classTable)


# Let's calculate the intensity for all files in R
resQC=calculateIntensity(repoData=repoData,files=qcFiles,integrationTable=integrationTable,classTable=classTable)

# Let's save same in a specific repository
getCsvOfIntensity(resQC, reposave=reposaveQC)

# Let's obtain an excel file with all the results
getExcelOfIntensity(reposaveQC,nameFile="QC.xlsx",output="int")
getExcelOfIntensity(reposaveQC,nameFile="QC_pct.xlsx",output="pct")


# Is that also working for one single file ?
reposaveBL="./../../tmp/centroid1"
blankFiles=c("CSCQ001.mzXML")
resBlanks=calculateIntensity(repoData=repoData,files=blankFiles,integrationTable=integrationTable,classTable)
resBlanks$call
 getCsvOfIntensity(resBlanks, reposave=reposaveBL)
 getExcelOfIntensity(reposaveBL,nameFile="BL.xlsx")

# Is that also working for dataset with no standards ?
#======================================================
# Where are the .xml ?
integrationTable=read.xlsx("./../../extdata/hilic_neg_table111022.xlsx")
integrationTable=integrationTable[,1:4]
colnames(integrationTable)=c("class","name","compo","mz")
classTable=read.xlsx("./../../extdata/rt_hilic_NEG.xlsx")

# Is there duplicates in integration Table
checkingIntegrationTable(integrationTable)

# Are the class table and the integration table containing the same class names ?
checkingClassTable(integrationTable=integrationTable,classTable=classTable)


# Let's calculate the intensity for all files in R
resQC=calculateIntensity(repoData=repoData,files=qcFiles,integrationTable=integrationTable,classTable=classTable)

# Let's save same in a specific repository
getCsvOfIntensity(resQC, reposave=reposaveQC)

# Let's obtain an excel file with all the results
e2=getExcelOfIntensity(reposaveQC,nameFile="QC.xlsx",output="int",includeStd=T)
e1=getExcelOfIntensity(reposaveQC,nameFile="QC_pct2.xlsx",output="pct")
test_that("no error in computation",{expect_true(!is.null(e1))})

e_noStd=getExcelOfIntensity(reposaveQC,nameFile="QCnoStd.xlsx",output="int",includeStd=F)

test_that("check includeStd option",{expect_true(
e_noStd$df_class[[1]][1,"CSCQ005.mzXML"]==sum(e2$df_class[[1]][-1,"CSCQ005.mzXML"],na.rm=T)-e2$df_class[[1]][!is.na(e2$df_class[[1]][,"std"])&e2$df_class[[1]][,"std"]=="yes","CSCQ005.mzXML"]
)})
test_that("check includeStd option 2",{expect_true(
e_noStd$df_class[[1]][1,"CSCQ005.mzXML"]==e_noStd$total[1,"CSCQ005.mzXML"]
)})