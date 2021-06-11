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
#' @return an ibm object containing a data.frame (accessible by ibmObject[[1]])
#'@export
#'@importFrom data.table data.table
#'@examples
#' #data(lcms)
#' #int_mass=lcmsIntensityByMass(lcms,rt=c(450,455))
#' #head(int_mass)
#'@importFrom MSnbase filterRt 
#'@importFrom MSnbase estimateNoise
lcmsIntensityByMass=function(lcms,breaks=NULL,integrationTable=NULL,rt=NULL,mz=NULL,by=0.0005,normalization="none",spar=0,agregation="sum",comparisonToPeaks=FALSE)
{ name=NULL
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
      # resdf[,"mz"]=as.numeric(as.character(resdf[,"mz"]))
      #resdf2=aggregate(lcms_df[,"i"],by=list(massbreaks),FUN=sum)

      #df=(mz=m)
      #resdt=data.table()
      #colnames(resdf)=c("mz","int")
      #resdf[,"mass"]=NA
      #for(i in 1:nrow(resdf))
      #{
      #  chaine_char=str_split(resdf[i,"mz"],",",simplify=T)[1]
      #  resdf[i,"mass"]=as.numeric(substr(chaine_char,2,nchar(chaine_char)))
      #}
      colnames(resdf)=c("mz","intensity","name")
  }
  if(!is.null(integrationTable))
  {
    if(!comparisonToPeaks)
    {
      lcms_df=as(lcms,"data.frame")
      resdf=NULL
      for(i in 1:dim(integrationTable)[1])
      { 
        #print(paste0(i,"/",dim(integrationTable)[1]))
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
    if(comparisonToPeaks)
    { 
      ibm2=lcmsIntensityByMass(lcms,by=0.001,agregation = "sum",rt=rt)
      sp1 <- new("Spectrum1",intensity = ibm2$df[,"intensity"], mz =ibm2$df[,"mz"], centroided = FALSE)
      noise=estimateNoise(sp1)[1,"intensity"] 
      peaks=pickingPeaks(ibm2$df)
      peaks2=peaks[peaks[,"intensity"]>10*noise,]
      
      res=mzmat=mztheo=rep(NA,length(unique(integrationTable[,"name"])))
      names(res)=names(mzmat)=names(mztheo)=unique(integrationTable[,"name"])
      for(name_ion in unique(integrationTable[,"name"]))
      {
        line_int=integrationTable[,"name"]==name_ion
        intens=peaks2[peaks2[,"x"]< integrationTable[line_int,"sup"]&peaks2[,"x"]>integrationTable[line_int,"inf"],]
        if(dim(intens)[1]==1)
        {
          res[name_ion]=intens[,"intensity"]
          mzmat[name_ion]=intens[,"x"]
          mztheo[name_ion]=integrationTable[integrationTable[,"name"]==name_ion,"mz"]
        }
        if(dim(intens)[1]>1)
        {
          ind_closest= which.min(abs(intens[,"x"]-integrationTable[line_int,"mz"]))
          res[name_ion]=intens[ind_closest,"intensity"]
          mzmat[name_ion]=intens[ind_closest,"x"]
          mztheo[name_ion]=integrationTable[integrationTable[,"name"]==name_ion,"mz"]
        }
      }
      resdf=data.frame(name=names(res),theo_mz=mztheo,mz=mzmat,intensity=res) 
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
  #res=list(df=df)
  class(ibm)<-"ibm"
  return(ibm)  
}