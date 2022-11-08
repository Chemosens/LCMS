#' normalizeWithStandards
#' 
#' Allows to standardize data with the standard in each class
#' @param nameXlsx name of the obtained .xlsx to normalize with standards: this excel must have been computed with an integrationTable including standard (a 'std' column)
#' @param nameWb name of the new file to be produced (corrected with standards)
#' @param concentrationTable if not null, a table containing two columns: a column 'class' and a column 'concentration_mg_ml'
normalizeWithStandards=function(nameXlsx="intokPOS.xlsx",nameWb=NULL, concentrationTable=NULL)
{
  if(is.null(nameWb))
  {
    if(is.null(concentrationTable))
    {
      nameWb=paste0(substr(nameXlsx,1,nchar(nameXlsx)-4),"NormalizedWithStandards.xlsx")
    }
    else
    {
      nameWb=paste0(substr(nameXlsx,1,nchar(nameXlsx)-4),"NormalizedWithStandardsAndConc.xlsx")
    }
  }
  if(nameWb %in% list.files()){print("warning: the file to be written was already in the directory and was overwritten.");file.remove(nameWb) }
  wb=loadWorkbook(nameXlsx)
  classNames=sheets(wb)
  classNames=classNames[classNames!="total"]
  wb2=createWorkbook()
  for(class in classNames)
  {
    print(class)
    if(!is.null(concentrationTable))
    {
      positionUnderscore=gregexpr("_",class)[[1]]
      ncharNotRt=positionUnderscore[length(positionUnderscore)]
      classWithoutRt=substr(class,1,ncharNotRt-1)
      concentration=concentrationTable[concentrationTable[,"Class"]==classWithoutRt,"concentration_mg_ml"]
    }
    else{concentration=1}
    
    jdd_class=read.xlsx(nameXlsx,sheet=class)
    samples=colnames(jdd_class)[!colnames(jdd_class)%in%c("name","class","compo","mz","avg","nNA","sd","CV","std")]
    std_line =  jdd_class[!is.na(jdd_class[,"std"])&jdd_class[,"std"]=="yes",]
    jdd_class2=jdd_class
    addWorksheet(wb2,class)
    if(dim(std_line)[1]==1)
    {
      for(sample in samples)
      {
        jdd_class2[,sample]=concentration*jdd_class[,sample] /std_line[,sample]
      }
      writeData(wb2,sheet=class,jdd_class2)
    }
    else
    {
      print("warning, no standard in this class")
    }
  }
  saveWorkbook(wb2,nameWb)
  return(wb2)
}