#' Sums intensities according to specific masses along a period of time
#' @param lcms a lcms object stemming from lcmsRead()
#' @param breaks a vector of breaks
#' @param integrationTable table of integration whose columns are named 'name','mz','inf','sup' and 'class'
#' @param rt vector of size 2 containing the interval of time to be studied
#' @param mz vector of size 2 containing the interval of mass to be studied
#' @param normalization "none" for no normalization, "max" for a maximale value of the peak to 100, "sum" for a total sum of the spectra of 100
#' @param by a numeric value. If breaks and integrationTable are NULL, breaks are automatically computed from min to max by a step defined by this parameter
#' @param spar 0 by default. Smoothing parameter between 0 (no smoothing) and 1
#' @param agregation "none" "sum" or "mean".
#' @param comparisonToPeaks If TRUE, the intensity value is the one of the closest mass peak contained in the integration table limits
#' @param limitIntegration 0.1 by default: window where to research peaks for each ion of integrationTable
#' @param minimalIntensityForPeak in profile (when peakPicking); select only peaks which are  higherThanNoise times higher than the noise and higher than minimalIntensityForPeak
#' @param higherThanNoise in profile (when peakPicking);select only peaks which are  higherThanNoise times higher than the noise and higher than minimalIntensityForPeak
#' @param byCTP intervall in mass for the sum in profile mode
#' @param centroided TRUE by default (is the data centroided). If false, the profile mode is selected and the algorithm of peak detection is different.
#' @param ppm select the peak with the minimal distance within the ppm distance.
#' @param minimalNumberOfPoints if centroid, minimal number of peaks to be detected in the retention time window (if lower, the returned intensity is NA)
#' @return an ibm object containing a data.frame (accessible by ibmObject[[1]])
#'@export
#'@importFrom data.table data.table
#'@examples
#' #data(lcms)
#' #int_mass=lcmsIntensityByMass(lcms,rt=c(450,455))
#' #head(int_mass)
#'@importFrom MSnbase filterRt rtime
#'@importFrom MSnbase estimateNoise
lcmsIntensityByMass=function(lcms,breaks=NULL,integrationTable=NULL,rt=NULL,mz=NULL,by=0.0005,normalization="none",spar=0,agregation="sum",comparisonToPeaks=FALSE,higherThanNoise=10,minimalIntensityForPeak=50,byCTP=0.001,centroided=TRUE,ppm=15,limitIntegration=0.1,minimalNumberOfPoints=0)
{
  name=NULL
  myWhichMin=function(mz_obs,mz_theo,ppm,centroided=centroided)
  {
    if(centroided)
    {
      ppm_vec=(abs(mz_obs-mz_theo)/mz_theo)*1e6
      ind=ppm_vec<ppm
    }
    if(!centroided)
    {
      ind=abs(mz_obs-mz_theo)==min(abs(mz_obs-mz_theo))
    }
    return(ind)
  }
  if(!is.null(rt))
  {
     lcms=filterRt(lcms,rt)
  }
  if(!is.null(mz))
  {
    lcms=filterMz(lcms,mz)
  }
   if(agregation!="none")
  {
  if(is.null(integrationTable))
  {
    lcms_df=as(lcms,"data.frame")
    if(is.null(breaks))
    {
      mi=min(lcms_df[,"mz"])
      ma=max(lcms_df[,"mz"])
      se=seq(mi,ma,by=by)
      breaks=se[-c(1,length(se))]
    }

      labs=1/2*(breaks[2:length(breaks)]+breaks[1:(length(breaks)-1)])
      massbreaks=cut(lcms_df[,"mz"],breaks=breaks,labels=labs)
      resdt=data.table("name"=massbreaks,"i"=lcms_df[,"i"],"mz"=lcms_df[,"mz"])
      if(agregation=="sum")
      {
        resdf= resdt[,list("mz"=mean(mz),"int"= sum(i)*by),by = name]
      }
      if(agregation=="mean")
      {
        resdf= resdt[,list("mz"=mean(mz),"int"= mean(i,na.rm=TRUE)),by = name]
      }

      resdf=as.data.frame(resdf)
      resdf=resdf[!is.na(resdf[,"name"]),c("mz","int","name")]
      colnames(resdf)=c("mz","intensity","name")
  }
  if(!is.null(integrationTable))
  {
    integrationTable[,"inf"]=integrationTable[,"mz"]-limitIntegration;
    integrationTable[,"sup"]=integrationTable[,"mz"]+limitIntegration;
    if(!comparisonToPeaks) # if we don't compare to peakPicking peaks: sum of all the mz in the retention window
    {
      lcms_df=as(lcms,"data.frame")
      resdf=NULL
      for(i in 1:dim(integrationTable)[1])
      {
         MassAxis=lcms_df[,"mz"]
        masses_label=
        MassAxis>=integrationTable[i,"inf"] &MassAxis<=integrationTable[i,"sup"]

        dfSubset=lcms_df[masses_label,]
        if(agregation=="sum")
        {
          sumSubset=c(integrationTable[i,"mz"],sum(dfSubset[,"i"]))
        }
        if(agregation=="mean")
        {
          sumSubset=c(integrationTable[i,"mz"],mean(dfSubset[,"i"]))
        }

        resdf=rbind(resdf,sumSubset)
      }
      resdf=as.data.frame(resdf)
      colnames(resdf)=c("mz","intensity")
      resdf[,"name"]=integrationTable[,"name"]
    }
    if(comparisonToPeaks) # if we compare to peaks
    {
      if(!centroided)
      {
        ibm2=lcmsIntensityByMass(lcms,by=byCTP,agregation = "sum",rt=rt) # we calculate a spectra by cutting by byCTP.
        sp1 <- new("Spectrum1",intensity = ibm2$df[,"intensity"], mz =ibm2$df[,"mz"], centroided = FALSE)
        # Uniquement valide pour le profile !
        noise=estimateNoise(sp1)[1,"intensity"] # estimation of the noise on this window
        peaks=pickingPeaks(ibm2$df) # Selection of peaks
        peaks2=peaks[peaks[,"intensity"]>max(higherThanNoise*noise,minimalIntensityForPeak),]
        peaks2=peaks
      }
      if(centroided)
      {
        peaks=as(lcms,"data.frame")
        colnames(peaks)=c("file","rt","x","intensity")
        peaks2=peaks[peaks[,"rt"]<rt[2]&peaks[,"rt"]>rt[1],]
      }
      res=mzmat=mztheo=rt_obs=rep(NA,length(unique(integrationTable[,"name"])))
      names(res)=names(rt_obs)=names(mzmat)=names(mztheo)=unique(integrationTable[,"name"])
      for(name_ion in unique(integrationTable[,"name"]))
      {
        line_int=integrationTable[,"name"]==name_ion
        intens=peaks2[peaks2[,"x"]< integrationTable[line_int,"sup"]&peaks2[,"x"]>integrationTable[line_int,"inf"],]

        if(dim(intens)[1]>minimalNumberOfPoints)
        {
          if(dim(intens)[1]==1)
          {
            res[name_ion]=intens[,"intensity"]
            mzmat[name_ion]=intens[,"x"]
            mztheo[name_ion]=integrationTable[integrationTable[,"name"]==name_ion,"mz"]

            if(1e6*abs(mzmat[name_ion]-mztheo[name_ion])/mztheo[name_ion]>ppm){res[name_ion]=NA}
            if(centroided)
            {
              rt_obs[name_ion]=intens[,"rt"]
            }
          }
          if(dim(intens)[1]>1)
          {
            rtimeObs=MSnbase::rtime(lcms)
            pasRtMoyen=mean(diff(rtimeObs),na.rm=T)/((rt[2]-rt[1]))
            wm=myWhichMin(mz_obs=intens[,"x"],mz_theo=integrationTable[line_int,"mz"],ppm=ppm,centroided=centroided)
            mzmat[name_ion]=mean(intens[wm,"x"],na.rm=T)
            mztheo[name_ion]=integrationTable[integrationTable[,"name"]==name_ion,"mz"]
            if(centroided)
            {
              if(sum(wm)>minimalNumberOfPoints)
              {
                rt_obs[name_ion]=mean(intens[wm,"rt"])
                res[name_ion]=sum(intens[wm,"intensity"],na.rm=T)*pasRtMoyen
              }

            }
            else
            {
              res[name_ion]=sum(intens[wm,"intensity"],na.rm=T)
            }
          }
        }

      }
      resdf=data.frame(name=names(res),theo_mz=mztheo,mz=mzmat,intensity=res)
      if(centroided){resdf[,"rt"]=rt_obs}
    }
  }
  #resdf=as.data.frame(resdf)
  rownames(resdf)=NULL
  if(normalization=="max")
  {
    resdf[,"intensity"]=100*resdf[,"intensity"]/max(resdf[,"intensity"])
  }
  if(normalization=="sum")
  {
    resdf[,"intensity"]=100*resdf[,"intensity"]/sum(resdf[,"intensity"])
  }
  }
  if(agregation=="none")
  {
    lcms_df=as(lcms,"data.frame")
    resdf=data.frame(mz=lcms_df[,"mz"],intensity=lcms_df[,"i"],name=as.character(lcms_df[,"mz"]))
  }
  ibm=list(df=resdf)
  if(!centroided&comparisonToPeaks){ibm$noise=noise}
  class(ibm)<-"ibm"
  return(ibm)
}