#'getExcelOfIntensity
#'
#'Compiles all .csv files obtained with getCsvOfIntensity of one single excel file with some graphs.
#'@param reposave directory where the results will be saved
#'@param samples which samples must be included in the analysis (default NULL corresponds to all samples)
#'@param nameFile name of the excel file containing the results. Default to Results.xlsx
#'@param output 'int' for intensity (absolute) results or 'pct' for data normalized by the sum of the signal per class.
#'@param timeSd  when an intensity is higher (or lower) than avg+timeSd*sd (or -), the excel cell is colored in green (or blue)
#'@param colors allowing to specify whether the default ggplot colors (NULL) or default colors ("default") should be used in the graphs in the excel
#'@param thresholdCV
#'@param includeStd should the standard information be included in statistics (especially the sum by lipid class)
#'@export getExcelOfIntensity
#'@importFrom utils read.table
#'@importFrom openxlsx createWorkbook createStyle addWorksheet writeData
#'@importFrom ggplot2 ggplot aes
#'@examples
#'  # Example of use - to be updated according to the position of your data
#'  #repo=getwd()  # Here in the current working directory
#'  #files=list.files() # Here on all the files present in the working directory
#'  # integrationTable=read.table("C:/integrationTable.csv")
#'  # integrationTable=read.table("C:/integrationTable.csv")
#'  #res=calculateIntensity(repo, files, integrationTable,classTable)
#'  # reposave="D:/MyResults"#Where the .csv of results will be produced
#'  #getCsvOfIntensity(res,reposave=reposave)
#'  # getExcelOfIntensity(reposave)
#'
getExcelOfIntensity=function(reposave,samples=NULL,nameFile="Results.xlsx",output="int",timeSd=3, colors="default",thresholdCV=30,includeStd=F)
{
  setwd(reposave)
  parameters=read.table(file="parameters.csv",sep=";",header=T)
  classTable=read.table(file="classTable.csv",sep=";",header=T)
  integrationTable=read.table(file="integrationTable.csv",sep=";",header=T)
  classPos=c()
  for(i in 1:dim(classTable)[1])
  {
    classPos=c(classPos,paste0(classTable[i,"class"],"_",paste0(classTable[i,"rtmin"],"-",classTable[i,"rtmax"])))
  }
 if("std"%in%colnames(integrationTable)){not_samples=c("name","class","compo","mz","avg","nNA","sd","CV","std")}else{not_samples=c("name","class","compo","mz","avg","nNA","sd","CV")}
 res=get_excel(repo=reposave,name="",classes=classPos,output=output,integrationTable=integrationTable,not_samples=not_samples,samples=samples,nameFile=nameFile,timeSd=timeSd,colors=colors,thresholdCV=thresholdCV,includeStd=includeStd)
  return(res)
}
