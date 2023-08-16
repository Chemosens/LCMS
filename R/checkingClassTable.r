#' checkingClassTable
#' @param integrationTable A table with at least 3 columns, name, compo, class and mz.
#' @param classTable A table with three columns: class for the class to be analyzed, rtmin for the minimum of retention time, rtmax for the maximale of retention time.
#'@export
checkingClassTable=function(integrationTable,classTable)
{
  wrongClassTable=FALSE
  classNotInIntegrationTable=!classTable[,"class"]%in%unique(integrationTable[,"class"])
  if(sum(classNotInIntegrationTable)!=0)
  {
    print("Warining the classTable contains class(es) that are not in the integration table")
    print(classTable[classNotInIntegrationTable,"class"])
    wrongClassTable=TRUE
  }
  else
  {
    print("OK ! All the class in classTable are in the integration table")
  }
  return(wrongClassTable)
}
