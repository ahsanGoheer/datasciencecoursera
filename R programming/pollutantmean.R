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

complete<-function(directory,id=1:322)
{
  currentWorkingDirectory<-paste('./',directory,sep="")
  filesInDirectory<-list.files(currentWorkingDirectory)
  id_vect<-vector()
  cases_vect<-vector()
  for(i in id)
  {
    pollutantData<-read.csv(paste(currentWorkingDirectory,'/',filesInDirectory[i],sep=""))
    x<-complete.cases(pollutantData)
    x<-sum(x)
    id_vect<-c(id_vect,i)
    cases_vect<-c(cases_vect,x)
  }
  complete_cases<-data.frame(id=id_vect,nob=cases_vect)
  complete_cases
}


corr<-function(directory,threshold=0)
{
  currentWorkingDirectory<-paste('./',directory,sep="")
  filesInDirectory<-list.files(currentWorkingDirectory)
  complete_cases=complete(directory)
  complete_cases<-complete_cases[complete_cases$nob>=threshold,]
  result<-numeric(0)
  if(nrow(complete_cases)>0)
  {
    for(i in complete_cases$id)
    {
      loaded_data<-read.csv(paste(currentWorkingDirectory,'/',filesInDirectory[i],sep=""))
      filtered_data<-loaded_data[(!is.na(loaded_data$nitrate)),]
      filtered_data<-filtered_data[(!is.na(filtered_data$sulfate)),]
      result<-c(result,cor(filtered_data['sulfate'],filtered_data['nitrate']))
    }
  }
  result
}