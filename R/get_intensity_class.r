#'@importFrom stats sd 
get_intensity_class=function(repo, samples=NULL,standards=FALSE,method="global",output="int",relative=F,log=F,infoSubject2=NULL,mode=c("POS","NEG"),classToRemove=NULL,columnNotSamples=c("name","class","compo","mz","std","average","nNA","avg","CV","sd"))
{
  setwd(repo)
  csvs=list.files(pattern="*.csv")
  csvInt=csvs[substr(csvs,1,nchar(output))==output]
  classes=substr(csvInt,13,nchar(csvInt)-4)
  print(classToRemove)
  print(classes%in%classToRemove)
  print(classes)
  classes=classes[!classes%in%classToRemove]
  for(classe in classes)
  {
    print(classe)
    x=read.table(paste0(output,"_",classe,".csv"),sep=";",header=TRUE)
    if(classe==classes[1])
    {
      if(is.null(samples))
      {
        not_samples=columnNotSamples
        samples=colnames(x)[!colnames(x)%in%not_samples]
      }
      x3=data.frame()
    }
    if(!standards)
    {
      if(method=="global")
      {
        if("std" %in% colnames(x))
        {
          sumWithoutStandards=apply(x[!is.na(x[,"std"])&x[,"std"]=="no",samples],2,function(x){return(sum(x,na.rm=T))})
        }
        else
        {
          sumWithoutStandards=apply(x[,samples],2,function(x){return(sum(x,na.rm=T))})
        }
        
        x3=rbind(x3,sumWithoutStandards)
      }
      if(method=="all")
      {
        print("all")
        if("std" %in% colnames(x))
        {
          x3=rbind(x3,x[!is.na(x[,"std"])&x[,"std"]=="no",c("name","class","compo","mz","std",samples)])
        }
        else
        {
          x3=rbind(x3,x[,c("name","class","compo","mz","std",samples)])
          
        }
      }
      
    }
    if(standards)
    {
      method="all"
      std=x[!is.na(x[,"std"])& x[,"std"]=="yes",c("name","class","compo","mz","std",samples)]
      print(std)
      x3=rbind(x3,std)
      
    }
  }
  if(!standards&method=="global"){colnames(x3)=samples;rownames(x3)=classes}
  # if(standards){colnames(x3)=c("name","class","compo",samples)}
  if(log==T &method=="global"){x3[,samples]=log(x3[,samples]+1)}
  if(relative){x3[,samples]=100*sweep(x3[,samples],2,apply(x3[,samples],2,function(x){return(sum(x,na.rm=T))}),"/")}
  totalIntensity=x3
  
  # Reshaping data
  if(method=="global")
  {
    totalIntensity=totalIntensity[,samples]
    longIntensity=reshape(cbind(class=rownames(totalIntensity),totalIntensity),direction="long",varying=list(colnames(totalIntensity)),times=substr(colnames(totalIntensity),1,9))
    colnames(longIntensity)=c("class","subject","intensity")
  }
  if(method=="all")
  {
    totalIntensity=cbind(totalIntensity[,c("name","class","compo","mz","std")],totalIntensity[,samples])
    longIntensity=reshape(totalIntensity,direction="long",varying=list(samples),times=substr(samples,1,9))
    colnames(longIntensity)=c("name","class","compo","mz","std","subject","intensity","id")
    longIntensity=longIntensity[,c("name","class","compo","subject","intensity")]
  }
  
  return(list(df=totalIntensity,samples=samples))
}
