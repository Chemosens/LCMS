
write_ibm_save=function(res,reposave,integrationTable_class,transform=T,columnIntegrationTable=c("class","compo","name","mz","std"),centroid=FALSE)
{
  setwd(reposave)
  rt=res$call$rt
  retTime=paste(rt,collapse="-")
  class=res$call$class
  if(!transform)
  {
    intens=pct=ppm=mzobs=NULL
    write.table(data.frame(name=rownames(res[["intensity"]]),res[["intensity"]]),file=paste0("res",retTime,class,name_repo,"intensity.csv"),sep=";",row.names=F)
    write.table(data.frame(name=rownames(res[["mz"]]),res[["mz"]]),file=paste0("res",retTime,class,name_repo,"mz.csv"),sep=";",row.names=F)
    write.table(data.frame(name=rownames(res[["ppm"]]),res[["ppm"]]),file=paste0("res",retTime,class,name_repo,"ppm.csv"),sep=";",row.names=F)
  }
  if(transform)
  {
    intens=transform_files(res[["intensity"]],integrationTable=integrationTable_class,columnIntegrationTable=columnIntegrationTable)
    pct=transform_files(res[["intensity"]],integrationTable=integrationTable_class,percent=TRUE,columnIntegrationTable=columnIntegrationTable)
    ppm=transform_files(res[["ppm"]],integrationTable=integrationTable_class,columnIntegrationTable=columnIntegrationTable)
    mzobs=transform_files(res[["mz"]],integrationTable=integrationTable_class,columnIntegrationTable=columnIntegrationTable)

    write.table(intens,file=paste0("int_",class,"_",retTime,".csv"),sep=";",row.names = FALSE)
    write.table(pct,file=paste0("pct_",class,"_",retTime,".csv"),sep=";",row.names = FALSE)
    write.table(mzobs,file=paste0("mzo_",class,"_",retTime,".csv"),sep=";",row.names = FALSE)
    write.table(ppm,file=paste0("ppm_",class,"_",retTime,".csv"),sep=";",row.names = FALSE)
    if(centroid)
    {
      rtobs=transform_files(res[["rt"]],integrationTable=integrationTable_class,columnIntegrationTable=columnIntegrationTable)
      write.table(rtobs,file=paste0("rto_",class,"_",retTime,".csv"),sep=";",row.names = FALSE)
    }
   }
  
  return(list(res=res,intens=intens,pct=pct,ppm=ppm,mzobs=mzobs))
  
} 
