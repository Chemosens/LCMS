# Comparing Results
#======================

# Loading libraries
library(openxlsx)
library(LCMS)

library(ggplot2)
library(testthat)

print(getwd())
comparaisonXlsx=function(name_auto,name_manu,nameFileToCompare="CSCQ131011.mzXML")
{
  wb_auto=loadWorkbook(name_auto)
  classes=sheets(wb_auto)
  wb_manu=loadWorkbook(name_manu)
  
  vec_manu=c()
  vec_auto=c()
  vec_name=c()
  classWithoutTotal=classes[!classes%in%c("total","total_pct","parameters","classTable","integrationTable")]
  for(class in classWithoutTotal)
  {
    print(class)
    class_manu=read.xlsx(name_manu,sheet=class) 
    manu=class_manu[,"intensity"];names(manu)=class_manu[,"compo"]
  
    class_auto=read.xlsx(name_auto,sheet=class)[-1,] 
    auto=class_auto[,nameFileToCompare];names(auto)=class_auto[,"compo"]
    in_manu=names(manu)
    #if(sum(class_auto[,"name"]!=class_manu[,"name"])==0)
    #{
      vec_manu=c(vec_manu,(manu[in_manu]))
      vec_auto=c(vec_auto,(auto[in_manu]))
      vec_name=c(vec_name,in_manu)
    #}
    # else
    # {
    #   print(class)
    #   print(class_auto[,"name"]==class_manu[,"name"])
    # }
    # 
    # plot(log(class_manu[,"intensity"]),log(class_auto[,"CSCQ131011.mzXML"]),main=class,xlab="manual",ylab="auto")
  }
  
  df=data.frame(manuel=vec_manu,auto=vec_auto,name=vec_name)
  df_log=data.frame(manuel=log(vec_manu,base=10),auto=log(vec_auto,base=10),name=vec_name)
  library(ggplot2)
  library(plotly)
  p=ggplot(df,aes(x=manuel,y=auto,name=name))+geom_point()+theme_bw()
  p_log=ggplot(df_log,aes(x=manuel,y=auto,name=name))+geom_point()+theme_bw()
  #p=ggplotly(p)
  #p_log=ggplotly(p_log)
  correlation=cor(log(vec_manu+1),log(vec_auto+1),use="pairwise.complete.obs")
  correlationLog=cor((vec_manu+1),(vec_auto+1),use="pairwise.complete.obs")
  
  return(list(p=p,correlationLog=correlationLog,correlation=correlation, df=df,df_log=df_log,p_log=p_log))
}
#originalRepo=getwd()
reposave="./../../tmp/correlation"
repoData="./../../extdata"
#setwd(repoData)
listFiles=c("CSCQ131011.mzXML","CSCQ131012.mzXML")

# Integration Table
integrationTablePos=read.xlsx(paste0(repoData,"/Pos_table06092022.xlsx"))
colnames(integrationTablePos)=c("class","name","compo","mz")
integrationTablePos2=integrationTablePos

# Class Table
classTable=data.frame(class=c("GPCho","GPEtn_Pos","GPSer","Cer","GluCer","LacCer","LPC","Sphingomyelin","LPE_Pos","Sphingosine"),
                      rtmin=60*c(3,2,2.5,0.25,0.35,0.6,4.6,4,2.9,0.55),
                      rtmax=60*c(4,2.7,3.5,0.5,0.55,1,5,4.5,3.5,1.33))

# Calculating intensity with LCMS packages
res=calculateIntensity(repoData=repoData,files=listFiles,integrationTable=integrationTablePos,classTable=classTable)
getCsvOfIntensity(res, reposave=reposave)
getExcelOfIntensity(reposave,nameFile="ResForTest.xlsx",output="int")

#Comparison
name_auto=paste0(reposave,"/ResForTest.xlsx")
name_manu=paste0(repoData,"/manuel_pos_ionsMOD16092022.xlsx")
resComp=comparaisonXlsx(name_auto=name_auto,name_manu=name_manu)

resComp$p
resComp$p_log
#ggplotly(resComp$p)
#ggplotly(resComp$p_log)

resComp$correlation
resComp$correlationLog

test_that('correlation higher than 0.97',{expect_true(resComp$correlation>0.97)})
test_that('correlation log higher than 0.99',{expect_true(resComp$correlationLog>0.996)})
#setwd(originalRepo)
