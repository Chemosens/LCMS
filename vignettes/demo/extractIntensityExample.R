#================================
library(MSnbase)
library(data.table)
library(openxlsx)
library(ggplot2)
library(LCMS)
#source("C:/Users/CalculCaro/Desktop/lcmsFunctions2.r")

# Setting parameters
#======================
# Where are the .xml ?
repoData="E:/ROCOR/Meibum_221108/xml/NEG"
reposave="E:/ROCOR/Meibum_221108/dataAnalysis/NEG"
centroided=FALSE
ppmThreshold=ppm=15

# Which files to use ? 
setwd(repoData)
files=list.files()
# Which integration table to use ?
integrationTable=read.xlsx("E:/ROCOR/Meibum_221108/20221108_IonsTable_Neg.xlsx")
colnames(integrationTable)=c("class","name","compo","mz")
# Which class table to use ? 
classTable=read.xlsx("E:/ROCOR/Meibum_221108/20221108_RT_Neg.xlsx")


# Is there duplicates in integration Table
checkingIntegrationTable(integrationTable,duplicatedName="name")

# Are the class table and the integration table containing the same class names ?
checkingClassTable(integrationTable=integrationTable,classTable=classTable)

# Let's calculate the intensity for all files in R
res=calculateIntensity(repoData=repoData,files=files,integrationTable=integrationTable,classTable=classTable,centroided=FALSE,ppmThreshold = ppmThreshold,ppm=ppm)

# Let's save same in a specific repository
getCsvOfIntensity(res, reposave=reposave)

# Let's obtain an excel file with all the results
excel=getExcelOfIntensity(reposave,nameFile="rocor_221108.xlsx",output="int")
excel2=getExcelOfIntensity(reposave,nameFile="rocor_221108_pct.xlsx",output="pct")



