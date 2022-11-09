# Loading functions and libraries
#================================
library(MSnbase)
library(data.table)
library(openxlsx)
library(ggplot2)
library(LCMS)
#source("C:/Users/CalculCaro/Desktop/lcmsFunctions2.r")

# Setting parameters
#======================
repoData="C:/Users/CalculCaro/Desktop/Rpackages/LCMS/extdata"
# Where are the .xml ?
reposaveQC="E:/PhD Glenda/PhD Glenda221011/HILIC/dataAnalysis/NEGHilicTest/QC"
# Which files to use ? 
qcFiles=c("CSCQ005.mzXML","CSCQ006.mzXML","CSCQ007.mzXML")
# Which integration table to use ?
integrationTable=read.xlsx("E:/PhD Glenda/PhD Glenda221011/HILIC/hilic_neg_table111022.xlsx")
colnames(integrationTable)=c("class","name","compo","mz","std")
# Which class table to use ? 
classTable=read.xlsx("E:/PhD Glenda/PhD Glenda221011/HILIC/rt_hilic_NEG.xlsx")


# Is there duplicates in integration Table
checkingIntegrationTable(integrationTable)

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
reposaveBL="E:/PhD Glenda/PhD Glenda221011/HILIC/dataAnalysis/NEGHilicTest/BL"
blankFiles=c("CSCQ001.mzXML")
resBlanks=calculateIntensity(repoData=repoData,files=blankFiles,integrationTable=integrationTable,classTable)
resBlanks$call
getCsvOfIntensity(resBlanks, reposave=reposaveBL)
getExcelOfIntensity(reposaveBL,nameFile="BL.xlsx")

# Is that also working for dataset with no standards ?
#======================================================
# Where are the .xml ?
reposaveQC="E:/PhD Glenda/PhD Glenda221011/HILIC/dataAnalysis/NEGHilicTest/QC"
# Which files to use ? 
qcFiles=c("CSCQ005.mzXML","CSCQ006.mzXML","CSCQ007.mzXML")
# Which integration table to use ?
integrationTable=read.xlsx("E:/PhD Glenda/PhD Glenda221011/HILIC/hilic_neg_table111022.xlsx")
integrationTable=integrationTable[,1:4]
colnames(integrationTable)=c("class","name","compo","mz")
# Which class table to use ? 
classTable=read.xlsx("E:/PhD Glenda/PhD Glenda221011/HILIC/rt_hilic_NEG.xlsx")

# Is there duplicates in integration Table
checkingIntegrationTable(integrationTable)

# Are the class table and the integration table containing the same class names ?
checkingClassTable(integrationTable=integrationTable,classTable=classTable)


# Let's calculate the intensity for all files in R
resQC=calculateIntensity(repoData=repoData,files=qcFiles,integrationTable=integrationTable,classTable=classTable)

# Let's save same in a specific repository
getCsvOfIntensity(resQC, reposave=reposaveQC)

# Let's obtain an excel file with all the results
getExcelOfIntensity(reposaveQC,nameFile="QC.xlsx",output="int")
getExcelOfIntensity(reposaveQC,nameFile="QC_pct2.xlsx",output="pct")


