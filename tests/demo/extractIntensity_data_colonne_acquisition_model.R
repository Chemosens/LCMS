# This is an easy way to compute the pipeline for LCMS data


# Loading libraries
#================================
library(LCMS)
rm(integrationTable) # Remove any old integrationTable
rm(classTable) # Remove any old classTable

# Setting all parameters - this part should be modified by the user
#=========================

repoData="E:/Test_221129/mzXML/CentroidedNeg" # Where are the .xml ?
reposave="E:/Test_221129/dataAnalysis/CentroidedNeg"# Where to save the results
centroided=T # Are the data centroided ?
ppmThreshold=ppm=15 # Choice of ppm
tableIntegration="E:/Test_221129/20221123_IonsTable_HILIC_Neg.xlsx" # Path for integrationTable
classTable=read.xlsx("E:/Test_221129/20221115_RT_HILIC_Neg.xlsx") # Path for classTable
nameOutputFileIntensity="test_221125_Centroid_NEG_HILIC.xlsx" # Name of final excel file (intensities)
nameOutputFilePct="test_221125_Centroid_NEG_HILIC_pct.xlsx" # Name of final excel file  (pourcent)
standards=FALSE # Are there standards in the integrationTable ? T for yes, F for no

# Running functions
# this part should not be modified by the user
# Click on ctrl+Enter to run each line of code
#===================
# Which files to use ?
setwd(repoData) # Going in the working directory
files=list.files() #Listing all .mzXML
# Which integration table to use ?
integrationTable=read.xlsx(tableIntegration) # Reading the integrationTable
integrationTable[1,] # displaying the first line for checks
if(!standards){colnames(integrationTable)=c("class","name","compo","mz")} # Renaming the colnames if required (in case of no standards)
if(standards){colnames(integrationTable)=c("class","name","compo","mz","std")} # Renaming the colnames if required (in case of standards)

# Is there duplicates in integration Table ?
checkingIntegrationTable(integrationTable,duplicatedName="name")
checkingIntegrationTable(integrationTable,duplicatedName="compo")

# Are the class table and the integration table containing the same class names ?
checkingClassTable(integrationTable=integrationTable,classTable=classTable)

# Let's calculate the intensity for all files in R
res=calculateIntensity(repoData=repoData,files=files,integrationTable=integrationTable,classTable=classTable,centroided=centroided,ppmThreshold = ppmThreshold,ppm=ppm)
names(res)

# Let's save same in a specific repository
getCsvOfIntensity(res, reposave=reposave)

# Let's obtain an excel file with all the results
e1=getExcelOfIntensity(reposave,nameFile=nameOutputFileIntensity,output="int")
e2=getExcelOfIntensity(reposave,nameFile=nameOutputFilePct,output="pct")



