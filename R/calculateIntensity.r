#' calculateIntensity
#'
#' For all the .mzXML specified in files, the intensities of all the molecules defined in the integrationTable are calculated taking the limits of retention times stored in classTable
#'@export
#'@inheritParams lcmsReadListIBM
#'@param repoData repository where the .mzXML files are stored
#'@param files names of the .mzXML files to be processed
#'@param classTable table of retention time (in seconds) for the different classes whose columns are labelled 'class' 'rtmin' 'rtmax
#'@param ppmThreshold limit of ppm required as last step of processing
#'@param msLevel level of MS (depending on the acquisition: could be 2 for SFC)
#'@return a list containing all the results per class
#'@examples
#' {
#'  # Example of use - to be updated according to the position of your data
#'  #repo=getwd()  # Here in the current working directory
#'  #files=list.files() # Here on all the files present in the working directory
#'  # integrationTable=read.table("C://integrationTable.csv")
#'  # integrationTable=read.table("C://integrationTable.csv")
#'  #res=calculateIntensity(repo, files, integrationTable,classTable)
#' }
#'
calculateIntensity=function(repoData=repoData,files,integrationTable,classTable,centroided=T,ppmThreshold=10,ppm=10,limitIntegration=0.1,byCTP=0.001,higherThanNoise=10,minimalIntensityForPeak=50,msLevel=1,minimalNumberOfPoints=0)
{
  res=list()
  a=checkingIntegrationTable(integrationTable,duplicatedName="name")
  if(a){stop("Duplicated names in integrationTable")}
  #b=checkingIntegrationTable(integrationTable,duplicatedName="compo")
  #if(b){stop("Duplicated compo in integrationTable")}
  c=checkingClassTable(integrationTable=integrationTable,classTable=classTable)
  if(c){stop("Error in classTable")}

  for(i in 1:dim(classTable)[1])
  {
    print(classTable[i,"class"])
    res[[classTable[i,"class"]]]=ibm_save(repository=repoData,listFiles=files,rt=c(classTable[i,"rtmin"],classTable[i,"rtmax"]),class=classTable[i,"class"],integrationTable2=integrationTable,ppmThreshold=ppmThreshold,centroided=centroided,ppm=ppm,limitIntegration=limitIntegration,byCTP=byCTP,higherThanNoise=higherThanNoise,minimalIntensityForPeak=minimalIntensityForPeak,msLevel=msLevel,minimalNumberOfPoints=minimalNumberOfPoints)
  }
  res[["call"]]=list(repository=repoData,listFiles=files,classTable=classTable,integrationTable=integrationTable,ppmThreshold=ppmThreshold,centroided=centroided,ppm=ppm,limitIntegration=limitIntegration,minimalNumberOfPoints=minimalNumberOfPoints)
  return(res)
}

