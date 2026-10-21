HiddenMarkovModel=function(Data,ClusterNo,DistributionName='NORMAL',Iterations=500,PriorClassification,PlotIt=TRUE,Silent=TRUE){

#author MT 2014
  if (is.list(Data))
    stop('Data is not allowed to be a list.')
  if (!is.matrix(Data)) {
    warning('Data is expected to be a matrix. Trying to transform..')
    Data = as.matrix(Data)
  }
  if (!requireNamespace("RHmm", quietly = TRUE))
    stop("Package 'RHmm' is required. Install it first.", call. = FALSE)
  if (!requireNamespace("dbt.ClassAnalysis", quietly = TRUE))
    stop("Package 'dbt.ClassAnalysis' is required. Install it first.", call. = FALSE)
  # The mixture plot is the only step that needs AdaptGauss.
  if (PlotIt && !requireNamespace("AdaptGauss", quietly = TRUE))
    stop("Package 'AdaptGauss' is required for PlotIt = TRUE.", call. = FALSE)
  
  HMMmodell <-
    RHmm::HMMFit(
      obs = Data,
      nStates = ClusterNo,
      dis = DistributionName,
      control = list(
        init = 'KMEANS',
        iter = Iterations,
        # RHmm requires numeric 0 or 1. Silent = TRUE must disable output.
        verbose = as.integer(!Silent)
      ),
      # K-means initialisierung
      asymptCov = TRUE
    )
  ############################################################
  # analyse des Modells
  #HMMmodell
  if (!Silent)
    print(summary(HMMmodell))
  
  HMM_means = round(HMMmodell$HMM$distribution$mean, 1)
  HMM_SDs  = round(sqrt(HMMmodell$HMM$distribution$var), 1)
  
  Uebergangsmatrix = HMMmodell$HMM$transMat
  UebergangsmatrixProzent = round(Uebergangsmatrix * 100)
  
  ########################################################################
  # viterbi Alg zur vorhersage der Zustaende
  # RHmm prints the data dimension during state decoding. Capture this
  # output so that Silent also applies here. Warnings remain visible.
  stateOutput <- utils::capture.output(
    VitPath <- RHmm::viterbi(HMMmodell, Data)
  )
  if (!Silent && length(stateOutput))
    cat(stateOutput, sep = "\n")
  HMMcls <- VitPath$states
  # die vorhergesagten Klassen des HMM
  
  Cl = dbt.ClassAnalysis::ClassCount(HMMcls)
  
  #UniqueClasses <-Cl$UniqueClasses
  #CountPerClass <-Cl$CountPerClass
  #NrOfClasses   <-Cl$NumberOfClasses
  ClassPercentages <- Cl$ClassPercentages
  
  HMM_weights = ClassPercentages / 100
  
  IsLogDistribution = HMM_weights * 0
  
  
  if (PlotIt) {
    print('Transition Matrix:')
    print(UebergangsmatrixProzent)
    print('Means of states:')
    print(HMM_means)
    print('Standard deviation of states:')
    print(HMM_SDs)
    AdaptGauss::PlotMixtures(
      Data[, 1],
      HMM_means ,
      HMM_SDs,
      HMM_weights,
      IsLogDistribution,
      main = 'BaumWelch opt. HMM Output - Gaussians weights = Viterbi',
      xlab = 'Time',
      ylab = 'pdf(Data) in black, model in red'
    )
    abline(v = HMM_means, col = 'magenta')
    
    #dbt.ClassAnalysis::ClassPlot(TagNR,Data,HMMcls,'HMM zustaende mit Viterbi ')
  }
  # vergleich der Zustaende der original Clusterung mit den HMM zustanden
  #ContingencyTable(PriorClassification,HMMcls)
  if (!missing(PriorClassification)) {
    RowCls = PriorClassification
    ColCls = HMMcls
    XTable  <-
      dbt.ClassAnalysis::ContingencyTableSummary(PriorClassification, HMMcls)
    
    if (!Silent)
      print(XTable)
    
    # genauigkeit rechnen
    if (!Silent)
      print(dbt.ClassAnalysis::AnalysisOfClassifier(PriorClassification, HMMcls))
    
    V <-
      dbt.ClassAnalysis::AnalysisOfClassifier(PriorClassification, HMMcls)
    
    Accuracy = sum(V$TotalAccuracy)
    if (!Silent)
      print(paste0('HMM Accuracy: ', round(Accuracy), ' Prozent'))
  } else{
    XTable = NULL
    Accuracy = NULL
  }
  # Give the weights their documented name. A duplicate HMM_means hides them.
  return(list(HMMmodell=HMMmodell,HMM_means=HMM_means,HMM_SDs=HMM_SDs,HMM_weights=HMM_weights,Uebergangsmatrix=Uebergangsmatrix,VitPath=VitPath,HMMcls=HMMcls,XTable=XTable,Accuracy=Accuracy))
}
