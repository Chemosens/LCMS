#'@export
#'@inheritParams lcmsReadList
calculateIntensity=function(repoData=repoData,files,integrationTable=integrationTable,classTable,nameFile="BL.xlsx",centroided=T,ppmThreshold=10,ppm=10,limitIntegration=0.1)
{
  res=list()
  for(i in 1:dim(classTable)[1])
  {
    print(classTable[i,"class"])
    res[[classTable[i,"class"]]]=ibm_save(repository=repoData,listFiles=files,rt=c(classTable[i,"rtmin"],classTable[i,"rtmax"]),class=classTable[i,"class"],integrationTable2=integrationTable,ppmThreshold=ppmThreshold,centroided=centroided,ppm=ppm,limitIntegration=limitIntegration)
  }
  res[["call"]]=list(repository=repoData,listFiles=files,classTable=classTable,integrationTable=integrationTable,ppmThreshold=ppmThreshold,centroided=centroided,ppm=ppm,limitIntegration=limitIntegration)
  return(res)
}

