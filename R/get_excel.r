#' @importFrom ggplot2 geom_col theme theme_bw element_text scale_color_manual geom_errorbar
#' @importFrom openxlsx insertPlot conditionalFormatting saveWorkbook
#' @importFrom stats reshape sd
get_excel=function(repo,name="",classes=c("PE","PI"),output="int",integrationTable,not_samples=c("name","class","compo","mz","std","avg","nNA","sd","CV"),samples=NULL,nameFile=NULL,timeSd=3,colors="default",thresholdCV=30)
{
  compo=avg=upper=lower=NULL
  setwd(repo)
  # Checks whether the file to create is already existing
  list_p=list()
  if(is.null(nameFile)){nameFile=paste0(name,output,".xlsx")}
  print(nameFile)
  if(nameFile%in%list.files()){print("warning: the file to be written was already in the directory and was overwritten.");file.remove(nameFile)}

  # Creates workbook
  wb=createWorkbook()
  styleRed=createStyle(fontColour="#9C0006",bgFill="#FFC7CE")
  styleGreen=createStyle(fontColour="#009C06",bgFill="#CEFFCE")
  styleBlue=createStyle(fontColour="#00069C",bgFill="#CECEFF")
  df_class=data.frame()
  for(classe in classes)
  {
    addWorksheet(wb,classe)
    namefile=paste0(output,"_",classe,".csv")
    x=read.table(namefile,sep=";",header=TRUE)
    if(is.null(samples))
    {
      samples=colnames(x)[!colnames(x)%in%not_samples]
    }
    else
    {
      if(!sum(samples%in%colnames(x))==length(samples))
      {

        print(samples[!samples%in%colnames(x)])
        stop("Error: some samples are not in colnames(x)")
      }
    }
    x=as.data.frame(x)
    x2=x[,c(not_samples,samples)]
    x2=x2[c(nrow(x2),1:(nrow(x2)-1)),]
    x2[1,"name"]="sum"
    if(length(samples)!=1)
    {
      x2[,(length(not_samples)+1):ncol(x2)]=apply(x2[,(length(not_samples)+1):ncol(x2)],2,as.numeric)
      x2[,c("avg","nNA","sd","CV")]=apply(x2[,c("avg","nNA","sd","CV")],2,as.numeric)
      x2[,"avg"]=apply(x2[,samples],1,function(x){return(mean(x,na.rm=T))})
      x2[,"sd"]=apply(x2[,samples],1,function(x){return(sd(x,na.rm=T))})
      x2[,"CV"]=100*x2[,"sd"]/x2[,"avg"]
      x2[,"nNA"]=apply(x2[,samples],1,function(x){return(sum(is.na(x)))})
      line_to_add=x2[x2[,"name"]=="sum",]
      line_to_add["name"]=classe
      df_class=rbind(df_class,line_to_add)
    }
    if(length(samples)==1)
    {
      x2[,"avg"]=x2[,samples]
      x2[,"sd"]=0
      x2[,"CV"]=NA
      x2[,"nNA"]=sum(is.na(x2[,samples]))
      line_to_add=x2[x2[,"name"]=="sum",]
      line_to_add["name"]=classe
      df_class=rbind(df_class,line_to_add)
    }
    conditionalFormatting(wb=wb,sheet=classe,cols=which(colnames(x2)=="CV"),rows=2:(nrow(x2)+1),style=styleRed,type="expression",rule=paste0(">",thresholdCV))
    if("std" %in% not_samples)
    {
      conditionalFormatting(wb=wb,sheet=classe,cols=10:ncol(x2),rows=2:(nrow(x2)+1),style=styleBlue,type="expression",rule=paste0("<$E2-",timeSd,"*$G2"))
      conditionalFormatting(wb=wb,sheet=classe,cols=10:ncol(x2),rows=2:(nrow(x2)+1),style=styleGreen,type="expression",rule=paste0(">$E2+",timeSd,"*$G2"))
    }else
    {
      conditionalFormatting(wb=wb,sheet=classe,cols=9:ncol(x2),rows=2:(nrow(x2)+1),style=styleBlue,type="expression",rule=paste0("<$E2-",timeSd,"*$G2"))
      conditionalFormatting(wb=wb,sheet=classe,cols=9:ncol(x2),rows=2:(nrow(x2)+1),style=styleGreen,type="expression",rule=paste0(">$E2+",timeSd,"*$G2"))
    }
    x3=x2
    x3[,"upper"]=x3[,"avg"]+x3[,"sd"]
    x3[,"lower"]=x3[,"avg"]-x3[,"sd"]
    writeData(wb,sheet=classe,x2)

    # Graphique
    #p1=ggplot(x3[-1,],aes(x=compo,y=avg))+geom_col(fill="light blue")+geom_errorbar(data=x3, mapping=aes(x=compo, ymin=upper, ymax=lower))+theme_bw()+ theme(axis.text.x = element_text( angle=45,hjust=1))
    #print(p1) # plot needs to be showed
    #insertPlot(wb, sheet=classe, width = 10, height = 3.5, fileType = "png", units = "in",startRow=nrow(x2)+3,startCol=1)
    castedres=reshape(x2[-1,],direction="long",v.names="intensity",times=samples,varying=list(samples),timevar="sample")
    if(is.null(colors))
    {
      p=ggplot(castedres, aes(x=sample,y=intensity,fill=compo,group=sample,name=name))+geom_col() +theme_bw()+ theme(axis.text.x = element_text( angle=45,hjust=1))
    }
    else
    {
      classUnique=unique(castedres[,"compo"])
      if(length(classUnique)>100){legendBelow=TRUE}else{legendBelow=FALSE}
      if(colors=="default")
      {
        if(length(classUnique)<8){colorsv=rainbow(length(classUnique))}
        else{
        colorsv=rep(c(rainbow(8),rainbow(8,v=0.7),rainbow(8,s=0.4)),10)[1:length(classUnique)]
        }
        names(colorsv)=classUnique
      }
      if(!legendBelow)
      {
        p=ggplot(castedres, aes(x=sample,y=intensity,fill=compo,group=sample,name=name))+geom_col() +theme_bw()+ theme(axis.text.x = element_text( angle=45,hjust=1))+scale_fill_manual(values=colorsv)
      }
      if(legendBelow)
      {
        p=ggplot(castedres, aes(x=sample,y=intensity,fill=compo,group=sample,name=name))+geom_col() +theme_bw()+ theme(axis.text.x = element_text( angle=45,hjust=1),legend.position="none")+scale_fill_manual(values=colorsv)
        p2=ggplot(castedres, aes(x=sample,y=intensity,fill=compo,group=sample,name=name)) +geom_col()+theme_bw()+scale_fill_manual(values=colorsv)
      }
    }
    list_p[[classe]]=p
    print(p)
    insertPlot(wb, sheet=classe, width = 10, height = 7, fileType = "png", units = "in",startRow=nrow(x2)+4,startCol=1)
    if(legendBelow)
    {print(p2)
      insertPlot(wb, sheet=classe, width = 10, height = 7, fileType = "png", units = "in",startRow=nrow(x2)+40,startCol=1)
    }
  }

  if(output=="int")
  {
    # Writing total sheet
    addWorksheet(wb,"total")
    df_class2=df_class[,-which(colnames(df_class)%in%c("class","compo","mz","std"))]
    conditionalFormatting(wb=wb,sheet="total",cols=which(colnames(df_class2)=="CV"),rows=2:(nrow(df_class2)+1),style=styleRed,type="expression",rule=paste0(">",thresholdCV))
    conditionalFormatting(wb=wb,sheet="total",cols=6:ncol(df_class2),rows=2:(nrow(df_class2)+1),style=styleBlue,type="expression",rule=paste0("<$B2-",timeSd,"*$D2"))
    conditionalFormatting(wb=wb,sheet="total",cols=6:ncol(df_class2),rows=2:(nrow(df_class2)+1),style=styleGreen,type="expression",rule=paste0(">$B2+",timeSd,"*$D2"))
    writeData(wb,sheet="total",df_class2)

    # Graphique
    castedres=reshape(df_class2,direction="long",v.names="intensity",times=samples,varying=list(samples),timevar="sample")
    if(is.null(colors))
    {
      p=ggplot(castedres, aes(x=sample,y=intensity,fill=name,group=sample,name=name))+geom_col() +theme_bw()+ theme(axis.text.x = element_text( angle=45,hjust=1))
    }
    else
    {
      classUnique=unique(castedres[,"name"])
      if(colors=="default")
      {
        if(length(classUnique)<8){colorsv=rainbow(length(classUnique))}
        else{ colorsv=rep(c(rainbow(8),rainbow(8,v=0.7),rainbow(8,s=0.4)),10)[1:length(classUnique)]}

        names(colorsv)=classUnique
      }
      p=ggplot(castedres, aes(x=sample,y=intensity,fill=name,group=sample,name=name))+geom_col() +theme_bw()+ theme(axis.text.x = element_text( angle=45,hjust=1))+scale_fill_manual(values=colorsv)
    }
    print(p)
    insertPlot(wb, sheet="total", width = 10, height = 5, fileType = "png", units = "in",startRow=nrow(df_class)+3,startCol=1)
    list_p[["total"]]=p
    # Writing total sheet with percentage
    addWorksheet(wb,"total_pct")

    df_class_pct=df_class2
    if(length(samples)>1)
    {
      df_class_pct[,samples]=100*sweep(df_class_pct[,samples],2,apply(df_class_pct[,samples],2,function(x){return(sum(x,na.rm=T))}),"/")
      df_class_pct[,"avg"]=apply(df_class_pct[,samples],1,function(x){mean(x,na.rm=T)})
      df_class_pct[,"sd"]=apply(df_class_pct[,samples],1,function(x){sd(x,na.rm=T)})
      df_class_pct[,"CV"]=100*df_class_pct[,"sd"]/df_class_pct[,"avg"]
      conditionalFormatting(wb=wb,sheet="total_pct",cols=which(colnames(df_class2)=="CV"),rows=2:(nrow(df_class2)+1),style=styleRed,type="expression",rule=paste0(">",thresholdCV))
      conditionalFormatting(wb=wb,sheet="total_pct",cols=6:ncol(df_class2),rows=2:(nrow(df_class2)+1),style=styleBlue,type="expression",rule=paste0("<$B2-",timeSd,"*$D2"))
      conditionalFormatting(wb=wb,sheet="total_pct",cols=6:ncol(df_class2),rows=2:(nrow(df_class2)+1),style=styleGreen,type="expression",rule=paste0(">$B2+",timeSd,"*$D2"))
    }
    if(length(samples)==1)
    {
      df_class_pct[,samples]=100*df_class_pct[,samples]/sum(df_class_pct[,samples],na.rm=T)
      df_class_pct[,"avg"]=df_class_pct[,samples]
      df_class_pct[,"sd"]=0
      df_class_pct[,"CV"]=NA
    }
    writeData(wb,sheet="total_pct",df_class_pct)

    castedres_pct=reshape(df_class_pct,direction="long",v.names="intensity",times=samples,varying=list(samples),timevar="sample")
    if(is.null(colors))
    {
      p=ggplot(castedres_pct, aes(x=sample,y=intensity,fill=name,group=sample,name=name))+geom_col() +theme_bw()+ theme(axis.text.x = element_text( angle=45,hjust=1))
    }
    else
    {
      classUnique=unique(castedres_pct[,"name"])
      if(colors=="default")
      {
        if(length(classUnique)<8){colorsv=rainbow(length(classUnique))}
        else{
        colorsv=rep(c(rainbow(8),rainbow(8,v=0.7),rainbow(8,s=0.4)),10)[1:length(classUnique)]
        }
        names(colorsv)=classUnique
      }
      p=ggplot(castedres_pct, aes(x=sample,y=intensity,fill=name,group=sample,name=name))+geom_col() +theme_bw()+ theme(axis.text.x = element_text( angle=45,hjust=1))+scale_fill_manual(values=colorsv)
    }
     print(p)
     list_p[["total_pct"]]=p
    insertPlot(wb, sheet="total_pct", width = 10, height = 5, fileType = "png", units = "in",startRow=nrow(df_class)+3,startCol=1)
  }


  if("parameters.csv"%in%list.files())
  {
    parameters=read.table(file="parameters.csv",sep=";",header=T)
    addWorksheet(wb,"parameters")
    writeData(wb,sheet="parameters",parameters)
  }
  if("classTable.csv"%in%list.files())
  {
    classTable=read.table(file="classTable.csv",sep=";",header=T)
    addWorksheet(wb,"classTable")
    writeData(wb,sheet="classTable",classTable)
  }
  if("integrationTable.csv"%in%list.files())
  {
    integrationTable=read.table(file="integrationTable.csv",sep=";",header=T)
    addWorksheet(wb,"integrationTable")
    writeData(wb,sheet="integrationTable",integrationTable)
  }

  saveWorkbook(wb,nameFile)
  return(list(wb=wb,list_p=list_p))
}
