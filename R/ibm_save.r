ibm_save=function(repository,rt,class,integrationTable2,listFiles=NULL,ppmThreshold=15,comparisonToPeaks=TRUE,centroided=FALSE,ppm=10,limitIntegration=0.1,name=NULL,repo="",byCTP=0.001,higherThanNoise=10,minimalIntensityForPeak=50)
{
  retTime=paste(rt,collapse="-")
  integrationTable_class=integrationTable2[integrationTable2[,"class"]==class,]
  res=LCMS::lcmsReadListIBM(listFiles=listFiles,rt=rt,integrationTable=integrationTable_class,comparisonToPeaks=comparisonToPeaks,wideFormat=TRUE,centroided=centroided,ppm=ppm,limitIntegration=limitIntegration,repo=repository,byCTP=byCTP,higherThanNoise=higherThanNoise,minimalIntensityForPeak=minimalIntensityForPeak)
  # setwd(reposave)
  if(!is.na(ppmThreshold))
  {
    res[["intensity"]][abs(res[["ppm"]])>ppmThreshold]=NA
  }
  res[["call"]]=list(name=name,rt=rt,class=class,ppmThreshold=ppmThreshold,listFiles=listFiles,repository=repository)
  return(res)
}
