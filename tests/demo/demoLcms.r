#rm(list=ls())

# 1- Installer les librairies 
# Loading libraries
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("MSnbase")library(MSnbase)

#library(gridExtra)
library(rhdf5)
library(ggplot2)
#library(reshape2)
#library(pheatmap)
library(stringr)
library(plotly)

# Loading my functions
#setwd("C:/INRA/ChemoSensPrivate/R")
#lapply(list.files(),source)

# Setting the working directory (where the datasets are)
setwd("../../Data")

# Loading data
lcms=readMSData(file="CSBJ150009_Retine.mzXML",msLevel=1,mode="onDisk")

integrationTable=read.csv(file="TestRetine.csv",header=TRUE,sep=";",dec=",")
head(integrationTable)


# See the chromatogram
int_time=lcmsIntensityByTime(lcms)
head(int_time)
p1 <- plot_fullspectra(x=int_time[,"time"],y=int_time[,"intensity"],main="Chromatogram",xlab="Time")
ggplotly(p1)

# See the sum spectra
int_mass=lcmsIntensityByMass(lcms,integrationTable=integrationTable)
p1 <- plot_fullspectra(x=int_mass[,"mz"],y=int_mass[,"intensity"])

# See the map
hm=lcmsHeatmap(lcms,threshold=100,timebreaks=100,massbreaks=100)
p_hm=plot_lcmsHeatmap(hm,log=T,scale_limits=c(10,20),main="Heatmap")
ggplotly(p_hm)

# Help to find interesting periods ? 
peakSelection=selectPeaksForAdjustement(x=int_time[,"time"],y=int_time[,"intensite"],ignore_threshold=0.2,span=25,selection=NULL,removeBL=T,removeNoise=T,main="Zone selection for analysis")
head(peakSelection)
p=plot_peakselection(peakselection)
ggplotly(p)

# Manual interesting period
#for(i in 1:dim(res3$df)[1])
#{
  i=1
  interestingPeriod=c(peakSelection$df[i,"inf"],peakSelection$df[i,"sup"])
  p1<-p1+geom_vline(aes(xintercept=interestingPeriod[1]),color="green")
  p1<-p1+geom_vline(aes(xintercept=interestingPeriod[2]),color="green")
  ggplotly(p1)
  
  # Filtering only the selected period
  retine_period=filterRt(retine,interestingPeriod)
  
  # Aggregating the corresponding masses 
  mz_period=as.vector(mz(retine_period[[1]]))
  results_accurate=lcmsAggregateMasses(retine_period,breaks=seq(min(mz_period),max(mz_period),0.01))
  p1<-plot_fullspectra(masses=results_accurate[,3],spectrum=results_accurate[,2],type="l")
#  p1
  
  # Agregate according to the integration table
  results_aggregate=lcmsAggregateMasses(retine_period,integrationTable=integrationTable)
  p1<-plot_fullspectra(x=results_aggregate[,1],y=results_aggregate[,2],type="bar")
  p1
  
  # Visualizing the results
  results_aggregate2=as.data.frame(results_aggregate)
  results_aggregate2$time=res3$df[i,"time"]
  
  # Write the results
  write.table(results_aggregate2,paste("test",i,".csv"),sep=";",row.names = F)
#}

  
