#'getCsvOfIntensity
#'
#'Saves all results obtained with calculateIntensity into several .csv files. Mandatory before running getExcelOfIntensity
#'@param res results of calculateIntensity
#'@param reposave repository where the .csv will be saved
#'@export getCsvOfIntensity
#'@importFrom utils write.table
#'@examples
#'#'  # Example of use - to be updated according to the position of your data
#'  #repo=getwd()  # Here in the current working directory
#'  #files=list.files() # Here on all the files present in the working directory
#'  # integrationTable=read.table("C://integrationTable.csv")
#'  # integrationTable=read.table("C://integrationTable.csv")
#'  #res=calculateIntensity(repo, files, integrationTable,classTable)
#'  # reposave="D://MyResults"#Where the .csv of results will be produced
#'  #getCsvOfIntensity(res,reposave=reposave)

getCsvOfIntensity=function(res,reposave)
{
  classTable=res$call$classTable
  integrationTable=res$call$integrationTable
  if("std"%in%colnames(integrationTable)){columnForIntegrationTable=c("class","compo","name","mz","std")}else{columnForIntegrationTable=c("class","compo","name","mz")}
  for(class in classTable[,"class"])
  {
    print(class)
    write_ibm_save(res[[class]],reposave=reposave,integrationTable_class=integrationTable[integrationTable[,"class"]==class,],transform=T,columnIntegrationTable=columnForIntegrationTable,centroid=res$call$centroided)
  }
  #getting log parameters
  setwd(reposave)
  param=res$call[!names(res$call)%in%c("integrationTable","classTable","listFiles")]
  print(param)
  param["listFiles"]=paste(res$call$listFiles,collapse=";")
  write.table(x=param,file="parameters.csv",sep=";",row.names=FALSE)
  write.table(x=res$call$integrationTable,file="integrationTable.csv",sep=";",row.names=F)
  write.table(x=res$call$classTable,file="classTable.csv",sep=";",row.names=F)
}
