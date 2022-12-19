#'getExcelOfIntensity
#'
#'Compiles all .csv files obtained with getCsvOfIntensity of one single excel file with some graphs.
#'@param reposave directory where the results will be saved
#'@param samples which samples must be included in the analysis (default NULL corresponds to all samples)
#'@param nameFile name of the excel file containing the results
#'@param output 'int' for intensity (absolute) results or 'pct' for data normalized by the sum of the signal per class.
#'@param timeSd  when an intensity is higher (or lower) than avg+timeSd*sd (or -), the excel cell is colored in green (or blue)
#'@param colors allowing to specify whether the default ggplot colors (NULL) or default colors ("default") should be used in the graphs in the excel
#'@param thresholdCV
#'@export getExcelOfIntensity
#'@importFrom utils read.table
#'@importFrom openxlsx createWorkbook createStyle addWorksheet writeData
#'@importFrom ggplot2 ggplot aes
getExcelOfIntensity=function(reposave,samples=NULL,nameFile=NULL,output="int",timeSd=3, colors="default",thresholdCV=30)
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
 res=get_excel(repo=reposave,name="",classes=classPos,output=output,integrationTable=integrationTable,not_samples=not_samples,samples=samples,nameFile=nameFile,timeSd=timeSd,colors=colors,thresholdCV=thresholdCV)
  return(res)
}
