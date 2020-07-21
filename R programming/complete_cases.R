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