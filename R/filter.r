filterXlsx=function(xlsx,nameWb,classes=NULL, minimalIntensity,percentOfMissing,lipidToRemove)
{
  if(nameWb %in% list.files()){print("warning: the file to be written was already in the directory and was overwritten.");file.remove(nameWb) }
  wb=loadWorkbook(xlsx)
  classNames=sheets(wb)
  classNames=classNames[!classNames%in%c("total","total_pct","parameters","integrationTable","classTable")]
  wb2=createWorkbook()
  styleRed=createStyle(fontColour="#9C0006",bgFill="#FFC7CE")
  styleGreen=createStyle(fontColour="#009C06",bgFill="#CEFFCE")
  styleBlue=createStyle(fontColour="#00069C",bgFill="#CECEFF")

  df_class=data.frame()
  for(class in classNames)
  {
    print(class)
    jdd_class=read.xlsx(xlsx,sheet=class)
    samples=colnames(jdd_class)[!colnames(jdd_class)%in%c("name","class","compo","mz","avg","nNA","sd","CV","std")]
    addWorksheet(wb2,class)
    for(i in 1:nrow(jdd_class))
    {
      if(jdd_class[i,"name"]!="sum")
      {
        blankAvg=jdd_class_blank[jdd_class_blank[,"name"]==jdd_class[i,"name"],"avg"]
        if(!is.na(blankAvg))
        {
          for(sample in samples)
          {

            if(!is.na(jdd_class[i,sample]))
            {
              if(jdd_class[i,sample]<timeNoise*blankAvg)
              {
                jdd_class[i,sample]=NA
              }
            }
          }
          jdd_class[i,"avg"]=mean(jdd_class[i,samples],na.rm=T)
          jdd_class[i,"sd"]=sd(jdd_class[i,samples],na.rm=T)
          jdd_class[i,"nNA"]=sum(is.na(jdd_class[i,samples]),na.rm=T)
        }

      }

    }
    jdd_class[jdd_class[,"name"]=="sum",samples]=apply(jdd_class[,samples],2,function(x){return(sum(x,na.rm=T))})
    df_class[,]=jdd_class[jdd_class[,"name"]=="sum",samples]
    writeData(wb2,sheet=class,jdd_class)
  }
  addWorksheet(wb2,"total")


  saveWorkbook(wb2,nameWb)
  return(wb2)

}