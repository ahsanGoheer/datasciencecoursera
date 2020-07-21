pollutantmean<-function(directory,pollutant,id=1:322)
{
  currentWorkingDirectory<-paste('./',directory,sep="")
  filesInDirectory<-list.files(currentWorkingDirectory)
  monitorData<-vector()
  for(i in id)
  {
    pollutantData<-read.csv(paste(currentWorkingDirectory,'/',filesInDirectory[i],sep=""))
    pollutantData<-pollutantData[,pollutant]
    bad<-is.na(pollutantData)
    pollutantData<-pollutantData[!bad]
    monitorData<-c(monitorData,pollutantData)
  }
  
  mean(monitorData)
    
}




