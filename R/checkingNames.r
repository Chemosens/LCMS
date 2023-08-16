checkingNames=function(integrationTable,mode="Neg")
{
  atomicMz=c("C"=12.01074,"H"=1.00784,"O"=15.9994,"P"=30.9738,"N"=14.0067)
  atomicMz=c("C"=12,"H"=1.0078,"O"=15.9949,"P"=30.9738,"N"=14.0031)

  mz=mz_th=rep(NA,dim(integrationTable)[1])
  for(i in 1:dim(integrationTable)[1])
  {
    name=integrationTable[i,"name"]
    mz[i]=integrationTable[i,"mz"]
    if(mode=="Neg")
    {
      mz_th[i]=getMolecularMz(name,atomicMz)-atomicMz["H"]
    }
    if(mode=="Pos")
    {
      mz_th[i]=getMolecularMz(name,atomicMz)+atomicMz["H"]
    }
  }
  return(list(mz=mz,mz_th=mz_th))
}
getMolecularMz=function(name,atomicMz)
{
  n=nchar(name)
  charVect=unlist(strsplit(name,NULL))
  atomicMzObs=which(charVect%in%names(atomicMz))
  atomicMzK=numberMzK=rep(NA,length(atomicMzObs))
  for( k in 1:length(atomicMzObs))
  {
    atomicMzK[k]=charVect[atomicMzObs[k]]
    if(k<length(atomicMzObs))
    {
      followingLetter=atomicMzObs[k+1]
    }
    if(k==length(atomicMzObs))
    {
      followingLetter=nchar(name)+1
    }
      if(atomicMzObs[k]+1<followingLetter)
      {
        numberMzK[k]=as.numeric(substr(name,atomicMzObs[k]+1,followingLetter-1))
      }
      if(atomicMzObs[k]+1==followingLetter)
      {
        numberMzK[k]=1
      }



  }

  return(sum(numberMzK*atomicMz[atomicMzK]))
}
# a=checkingNames(integrationTable,mode="Pos")
# summary(a$mz-a$mz_th)
#
# a$mz[a$mz-a$mz_th!=0]
