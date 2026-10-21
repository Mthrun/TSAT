blockDBT()
# TSclust is an optional distance fallback. CRAN archived it on 2026-01-16.
# Install it from the archive only if you need its methods.
pp=c('xts', 'imputeTS', 'lubridate','signal', 'chron', 'WaveletComp', 'dlm', 'BIDistances', 'dtwclust', 'SimilarityMeasures', 'jmotif', 'FKmL', 'pdc', 'proxy', 'zoo', 'fanplot', 'forecast', 'prophet', 'lmtest', 'vars', 'fUnitRoots', 'tseries', 'dtw','randomForest','ranger', 'plotly', 'dplyr', 'tibble', 'htmlwidgets','latticeExtra')
install.packages(pp)
unblockDBT()

pp2=c(     'Metrics', 'tensorflow', 'keras', 'wdman', 'bsts', 'ggfortify',
           'echos', 'tsintermittent', 'xgboost', 'RSelenium',
           'SimilarityMeasures', 'jmotif', 'FKmL', 'TSclust', 'tsensembler')
install.packages(pp2)