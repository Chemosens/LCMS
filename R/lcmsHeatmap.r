#'@title lcmsHeatmap
#'@param lcms lcms object
#'@param timebreaks Number of equidistant timepoints
#'@param massbreaks Number of equidistant masspoints
#'@param rt Numeric vector of size 2 representing the interval of time to be plotted
#'@param threshold Intensity below this threshold are not represented
#'@return a data frame with "mz","rt" and "int" columns
#'@export
#'@importFrom MSnbase filterRt
#'@importFrom stats aggregate
lcmsHeatmap=function(lcms,threshold=100,mz=NULL,timebreaks=100,massbreaks=100,rt=NULL)
{
  if(!is.null(rt)){lcms<- filterRt(lcms,rt)}
  if(!is.null(mz)){lcms<- filterMz(lcms,mz)}

  lcms_df=as(lcms,"data.frame")
  lcms_df2=lcms_df[lcms_df[,"i"]>threshold,]
  timebreaks=cut(lcms_df2[,"rt"],breaks=timebreaks)
  massbreaks=cut(lcms_df2[,"mz"],breaks=massbreaks)
  resdf=aggregate(lcms_df2[,"i"],by=list(timebreaks,massbreaks),FUN=sum)
  colnames(resdf)=c("rt","mz","int")
 # class(resdf)="lcmshm"
  hm=list(df=resdf)
  class(hm)="hm"
  return(hm)
}