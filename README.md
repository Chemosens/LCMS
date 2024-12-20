
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LCMS

<!-- badges: start -->
<!-- badges: end -->

The goal of LCMS is to compute semi targeted LCMS data from .mzXML

## Installation

You can install the development version of LCMS from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ChemoSens/LCMS")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(openxlsx)
library(LCMS)
## basic example code
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
```
