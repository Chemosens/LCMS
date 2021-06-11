#' Computes the intensity by timepoint for specific mz. Required to plot a chromatogram.
#'@param lcms LCMS object (from lcmsRead)
#'@param rt A numeric(2) or two-column matrix defining the lower and upper boundary for the retention time range/window(s) for the chromatogram(s). If a matrix is provided, a chromatogram is extracted for each row. If not specified, a chromatogram representing the full retention time range is extracted. 
#'@param mz A numeric(2) or two-column matrix defining the mass-to-charge (mz) range(s) for the chromatogram(s). For each spectrum/retention time, all intensity values within this mz range are aggregated to result in the intensity value for the spectrum/retention time. If not specified, the full mz range is considered.
#'@param timeUnits "s" or "min": units of the time column
#'@export
#'@examples
#'#data(lcms)
#'#int_time=lcmsIntensityByTime(lcms,rt=c(450,460))
#'#head(int_time)
#'@importFrom MSnbase rtime 
#'@importFrom MSnbase intensity 
#'@importFrom MSnbase chromatogram
lcmsIntensityByTime=function(lcms,rt=NULL,mz=NULL,timeUnits="s")
{
  if(is.null(rt)&is.null(mz)){ chrom=chromatogram(lcms)[[1]]}
  if(is.null(rt)&!is.null(mz)){ chrom=chromatogram(lcms,mz=mz)[[1]]}
  if(!is.null(rt)&is.null(mz)){ chrom=chromatogram(lcms,rt=rt)[[1]]}
  if(!is.null(rt)&!is.null(mz)){ chrom=chromatogram(lcms,rt=rt,mz=mz)[[1]]}
  
  x=rtime(chrom);
  y=intensity(chrom)
  df=data.frame(time=x,intensity=y)
  if(timeUnits=="min"){df$time=df$time/60}
  rownames(df)=NULL
  ibt=list(df=df)
  class(ibt)="ibt"
  return(ibt)
}