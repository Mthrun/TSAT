HiddenMarkovModel=function(Data,ClusterNo,PriorClassification,PlotIt=T,Silent=F){
#Doku, Under Development
#author MT 2014
  requireNamespace('RHmm') 
  requireNamespace('AdaptGauss') 
  requireNamespace('dbt.ClassAnalysis') 
  requireNamespace('Classifiers') 
  
  warning('Under Development')
  HMMmodell <- RHmm::HMMFit(obs=Data, nStates=ClusterNo,control=list(verbose=1, init= 'KMEANS'),# K-means initialisierung
                      asymptCov=TRUE)
  ############################################################
  # analyse des Modells
  #HMMmodell
  if(!Silent)
    summary(HMMmodell)
  
  HMMmeans = round(HMMmodell$HMM$distribution$mean,1);       print(paste0('HMMmeans ',HMMmeans ))
  HMMsdev  = round(sqrt(HMMmodell$HMM$distribution$var),1);   print(paste0('HMMsdev  ',HMMsdev ))
  
  Uebergangsmatrix = HMMmodell$HMM$transMat
  UebergangsmatrixProzent= round(Uebergangsmatrix*100)
   if(!Silent){
    print(UebergangsmatrixProzent)
    print(HMMmeans)
   }
  ########################################################################
  # viterbi Alg zur vorhersage der Zustaende
  VitPath <- RHmm::viterbi(HMMmodell, Data)
  HMMcls <- VitPath$states ; # die vorhergesagten Klassen des HMM
  
  C <-dbt.ClassAnalysis::ClassCount(HMMcls);
  UniqueClasses <-C$uniqueClasses
  CountPerClass <-C$countPerClass
  NrOfClasses   <-C$numberOfClasses  
  ClassPercentages <-C$classPercentages 
  
  Weight= ClassPercentages/100;
  IsLogDistribution= Weight*0;
  if(PlotIt){

  AdaptGauss::PlotMixtures(Data,HMMmeans ,HMMsdev,Weight,IsLogDistribution,'BaumWelch opt. HMM Output-Gaussians weights = Viterbi','TagNR','pdf(Data)')
  abline(v=HMMmeans,col='red')
  #dbt.ClassAnalysis::ClassPlot(TagNR,Data,HMMcls,'HMM zustaende mit Viterbi ')
  }
  # vergleich der Zustaende der original Clusterung mit den HMM zustanden
  #ContingencyTable(PriorClassification,HMMcls)
  RowCls =PriorClassification
  ColCls =HMMcls 
  XTable  <- dbt.ClassAnalysis::ContingencyTableSummary(PriorClassification,HMMcls);
  if(!Silent)
    print(XTable)
  
  # genauigkeit rechnen
  if(!Silent)
    print(Classifiers::AnalysisOfClassifier(PriorClassification,HMMcls))
  
  V<- Classifiers::AnalysisOfClassifier(PriorClassification,HMMcls);
  Accuracy = sum(V$TotalAccuracy)
  if(!Silent)
    print(paste0('HMM Accuracy: ',round(Accuracy),' Prozent'))
  
  return(list(HMMmodell=HMMmodell,HMMmeans=HMMmeans,HMMsdev=HMMsdev,Uebergangsmatrix=Uebergangsmatrix,VitPath=VitPath,HMMcls=HMMcls,XTable=XTable,Accuracy=Accuracy))
}