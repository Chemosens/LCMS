#'@title lcmsReadListIBT
#'@param listFiles vector of character contaning the file names to be read
#'@param rt a vector of length two representing an interval of rt to be analyzed  
#'@param mz a vector of length two representing an interval of mz to be analyzed  
#'@param integrationTable integrationTable
#'@param timeUnits Unit of time "s"
#'@export
lcmsReadListIBT=function(listFiles,rt=NULL,mz=NULL,integrationTable=NULL,timeUnits="s")
{
  df_mass=NULL
  listQC=listFiles
  for(i in 1:length(listFiles))
  {
    print(paste0(i,"/",length(listFiles)))
    tryCatch({
      lcms=readMSData(files=listFiles[i],msLevel.=1,mode="onDisk")
      int_mass=lcmsIntensityByTime(lcms,rt=rt,mz=mz,timeUnits=timeUnits)
      dfi=data.frame(int_mass$df,namefile=listFiles[i],num=i)
      df_mass=rbind(df_mass,dfi)
    },error=function(e){print(e)})
    
  }
  return(df_mass)
}