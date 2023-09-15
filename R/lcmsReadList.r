  #' lcmsReadListIBM
  #'@param listFiles list of character containing the file name to be imported
  #'@importFrom MSnbase readMSData
  #'@importFrom MSnbase filterRt
  #'@importFrom MSnbase filterMz
  #'@param wideFormat If TRUE the returned dataframe is wide (ions in columns), ifelse, the returned dataframe is long
  #'@inheritParams lcmsIntensityByMass
  #'@export
  lcmsReadListIBM=function(listFiles,normalization="none",rt=NULL,integrationTable=NULL,breaks=breaks,by=0.1,mz=NULL,agregation="mean",comparisonToPeaks=TRUE,wideFormat=FALSE,centroided=FALSE,ppm=15,limitIntegration=0.1,byCTP=0.001,higherThanNoise=10,minimalIntensityForPeak=50,repo="",msLevel=1,minimalNumberOfPoints=0)
  {
    df_mass=NULL
    listQC=listFiles
    res=res_intensity=res_ppm=res_rt=res_mz=NULL
    for(i in 1:length(listFiles))
    {
      print(paste0(listFiles[i]," (",i,"/",length(listFiles),")"))
      if(repo!="")
      {
        lcms=readMSData(files=paste0(repo,"/",listFiles[i]),msLevel.=msLevel,mode="onDisk")
      }
      if(repo==""){ lcms=readMSData(files=listFiles[i],msLevel.=1,mode="onDisk")}
       int_mass=lcmsIntensityByMass(lcms,integrationTable=integrationTable,rt=rt,normalization = normalization,mz=mz,agregation=agregation,comparisonToPeaks=comparisonToPeaks,byCTP=byCTP,centroided=centroided,ppm=ppm,limitIntegration=limitIntegration,higherThanNoise=higherThanNoise,minimalIntensityForPeak=minimalIntensityForPeak,minimalNumberOfPoints=minimalNumberOfPoints)

      if(wideFormat==FALSE)
      {
        dfi=data.frame(int_mass$df,namefile=listFiles[i],num=i)
        df_mass=rbind(df_mass,dfi)
      }
      if(wideFormat==TRUE&!is.null(integrationTable))
      {
        res_intensity[[listFiles[i]]]=int_mass$df[,"intensity"]
        names( res_intensity[[listFiles[i]]])=int_mass$df[,"name"]
        res_mz[[listFiles[i]]]=int_mass$df[,"mz"]
        names( res_mz[[listFiles[i]]])=int_mass$df[,"name"]
        res_ppm[[listFiles[i]]]=(int_mass$df[,"mz"]-int_mass$df[,"theo_mz"])/(int_mass$df[,"theo_mz"]*1e-6)
        names( res_ppm[[listFiles[i]]])=int_mass$df[,"name"]
        if(centroided)
        {
          res_rt[[listFiles[i]]]=int_mass$df[,"rt"]
          names( res_rt[[listFiles[i]]])=int_mass$df[,"name"]
        }
      }
    }


    if(wideFormat==TRUE&!is.null(integrationTable))
    {
      if(length(listFiles)==1)
      {

        res_intensity=t(as.matrix(x=res_intensity[[listFiles[1]]],ncol=1))
        rownames(res_intensity)=listFiles
        res_mz=t(as.matrix(x=res_mz[[listFiles[1]]],ncol=1))
        rownames(res_mz)=listFiles
        res_ppm=t(as.matrix(x=res_ppm[[listFiles[1]]],ncol=1))
        rownames(res_ppm)=listFiles
        if(centroided)
        {
          res_rt=t(as.matrix(x=res_rt[[listFiles[1]]],ncol=1))
          rownames(res_rt)=listFiles
        }

      }
      if(length(listFiles)>1)
      {
        res_intensity=Reduce(rbind,res_intensity)
        rownames(res_intensity)=listFiles
        res_mz=Reduce(rbind,res_mz)
        rownames(res_mz)=listFiles
        res_ppm=Reduce(rbind,res_ppm)
        rownames(res_ppm)=listFiles
        if(centroided)
        {
          res_rt=Reduce(rbind,res_rt)
          rownames(res_rt)=listFiles
        }
      }

      df_mass=list(intensity=res_intensity,mz=res_mz,ppm=res_ppm)
      if(centroided){df_mass[["rt"]]=res_rt}
    }
    return(df_mass)
  }