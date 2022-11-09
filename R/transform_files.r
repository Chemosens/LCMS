transform_files=function(tableToUse,integrationTable,percent=FALSE,class=NULL,columnIntegrationTable=c("class","compo","name","mz","std"))
{
  # tableToUse=read.csv(file=file,sep=";",header=TRUE)
  transposedTable=t(tableToUse)
  # transposedTable2=transposedTable[-1,]
  if(nrow(transposedTable)>1)
  {
    transposedTable2=apply(transposedTable,2,as.numeric)
  }
  else{transposedTable2=t(as.matrix(as.numeric(transposedTable)))}
  
  if(percent==TRUE){transposedTable2=100*sweep(transposedTable2,2,apply(transposedTable2,2,function(x){return(sum(x,na.rm=T))}),"/")}
  colnames(transposedTable2)=samples=colnames(transposedTable)
  rownames(transposedTable2)=rownames(transposedTable)
  tableWithIonAsColumn=data.frame(ion=rownames(transposedTable2),transposedTable2)
  colnames(tableWithIonAsColumn)=c("ion",colnames(transposedTable2))
  integrationTableWithPoint=integrationTable
  
  # integrationTableWithPoint[,"name"]=gsub(" ",".",integrationTableWithPoint[,"name"])
  resMerged=merge(integrationTableWithPoint[,columnIntegrationTable],tableWithIonAsColumn,by.x="name",by.y="ion",all.y=TRUE)
  resMerged=resMerged[order(resMerged[,"mz"]),]
  resMerged["sum",samples]=apply(as.matrix(resMerged[,samples]),2,function(x){return(sum(x,na.rm=T))})
  resMerged[,"nNA"]=apply(as.matrix(resMerged[,samples]),1,function(x){return(sum(is.na(x)))})
  resMerged[,"avg"]=apply(as.matrix(resMerged[,samples]),1,function(x){return(mean(x,na.rm=T))})
  resMerged[,"sd"]=apply(as.matrix(resMerged[,samples]),1,function(x){return(sd(x,na.rm=T))})
  resMerged[,"CV"]=apply(as.matrix(resMerged[,samples]),1,function(x){return(100*sd(x,na.rm=T)/mean(x,na.rm=T))})
  if(!is.null(class))
  {
    resMerged["sum","class"]=class
    resMerged=resMerged[resMerged[,"class"]==class,]
  }
  #  write.table(file=paste0(substr(file,1,nchar(file)-4),"_transposed",ifelse(percent,"_pct",""),".csv"),resMerged,row.names=FALSE,sep=";")
  return(resMerged)
}