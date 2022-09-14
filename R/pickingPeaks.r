#' @title pickingPeaks
#'@importFrom MSnbase pickPeaks
#'@param df a dataframe containing two columns: "mz" and "intensity" corresponding respectively to abscissa and ordinates.
#'@param col name of the column containing the abscissa ("mz" by default)
#'@param signalPercentage of the centroid's intensity are used to calculate the refined m/z. By default the descend is stopped when the first signal that is equal or larger than the last observed one is encountered.
#'@export
pickingPeaks=function(df,col="mz",signalPercentage=10,refineMz="descendPeak")
{ sp <- new("Spectrum1",
            intensity =df[,"intensity"],
            mz = df[,col],
            centroided = FALSE)
sp2 <- MSnbase::pickPeaks(sp, refineMz = refineMz, signalPercentage = signalPercentage)
                          #,method=method,halfWindowSize=halfWindowSize, SNR=SNR)
df=data.frame(x=MSnbase::mz(sp2),intensity=MSnbase::intensity(sp2))
df2=df[order(df[,"intensity"],decreasing=T),]
df2[,"relative"]=100*df2[,"intensity"]/max(df2[,"intensity"])
return(df2)
}
