#'@title lcmsRead
#'@param file name of the file to be read
#'@param rt  a vector of size two indicating a range of retention time to be selected
#'@param mz a vector of size two indicating a range of mass to be selected
#'@param msLevel 1 for MS1, 2 for MS2 and so on...
#'@return a MSnExp object if integrationTable is NULL, a dataframe ifelse
#'@export 
#'@importFrom MSnbase readMSData
#'@importFrom MSnbase filterRt
#'@importFrom MSnbase filterMz
#'@examples
#' #  DO NOT RUN
#' #  file <- dir(system.file(package = "chemosensR", dir = "extdata"),full.name=TRUE,pattern="mzXML$")
#' # lcms=lcmsRead(file)
lcmsRead=function(file,rt=NULL,mz=NULL,msLevel=1)
{
  lcms=readMSData(files=file, msLevel.=msLevel,mode="onDisk")
  if(!is.null(rt)){lcms=filterRt(lcms,rt)}
  if(!is.null(mz)){lcms=filterMz(lcms,mz)}
  return(lcms)
}