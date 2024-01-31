GetCashFlow=function(Symbol='SAP',URL='yahoo',Silent=TRUE,Port=4445L,tz="UTC",...){
  #GetCashFlow('SAP')
  
  requireNamespace('xml2')
  requireNamespace('dplyr')
  requireNamespace('rvest')
  if (URL == 'yahoo') {
    tryCatch({
      library(RSelenium)
      #library(wdman)
      #library(dplyr)
      # Intializing the firefox driver at the given port
      remDrServer <- RSelenium::rsDriver(browser = "firefox",port=Port ,...)
      # Intializing the server on the same port
      remDrClient <- RSelenium::remoteDriver$new(port=Port)
      #remDrClient
      #clicks via chrome rightklick on inspect, then you can copy the relevant strings for button clicks
      # cDrv <- chrome(verbose = !Silent)
      # eCaps <-
      #   list(chromeOptions = list(
      #     args = c('--headless', '--disable-gpu', '--window-size=1280,800')
      #   ))
      # remDrClient <-
      #   remoteDriver(
      #     browserName = "chrome",
      #     port = 4567L,
      #     extraCapabilities = eCaps
      #   )
      #suppressMessages(remDrClient$open())
      msg = utils::capture.output(remDrClient$open())
      if (!Silent) {
        print(msg)
      }
      #if you want to see whats happening
      #driver=rsDriver(port = 4567L,browser = "chrome")
      #remDrClient=driver[["client"]]
      
      url = paste0("https://finance.yahoo.com/quote/",
                   Symbol,
                   "/cash-flow?p=",
                   Symbol)
      #url = paste0("https://finance.yahoo.com/quote/SAP/financials?p=SAP")
      remDrClient$navigate(url)
      #copy selector then put it here
      try({
      webElem = remDrClient$findElement(using = 'name','agree')
      # webElem <-
      #   remDrClient$findElement(
      #     using = 'css selector',
      #     'body > div.consent-wizard.eu-single-page > div.consent-wizard-body.eu-single-page > div.consent-steps-container > div > div.single-page-forms.yahoo > form.consent-form.single-page-form.single-page-agree-form > div > input'
      #   )
      webElem$clickElement()
      })
      #author of this part: Hamza Tayyab
      # get annual data
      # annual <- xml2::read_html(remDrClient$getPageSource()[[1]])
      # annual <- annual %>% rvest::html_table(fill = T, header = F)
      #%>% .[[1]]  %>% filter(X1!=X2)
      #annual
      
      # click on Quarterly button
      #copy xpath then put it here
      webElem <-
        remDrClient$findElement(using = 'xpath',
                          '//*[@id="Col1-1-Financials-Proxy"]/section/div[1]/div[2]/button')
      webElem$clickElement()
      
      #Extract all tables
      quarterly <- xml2::read_html(remDrClient$getPageSource()[[1]])
      remDrClient$close()
      remDrServer$server$stop()
      quarterly = rvest::html_table(quarterly,fill = T, header = F) 
      #select correct table
      quarterly=quarterly[[1]]
      #quarterly <- quarterly %>% rvest::html_table(fill = T, header = F) %>% .[[1]]  %>% filter(X1 != X2)
      #delete yahoo headers of variables without any input
      quarterly=dplyr::filter(quarterly,X1 != X2)
      # get quarterly data
      
      #end of author of this part: Hamza Tayyab
      Features = sapply(quarterly, function(x)
        gsub(',', '', x))
      Features[1, 1] = "Time"
      #Features=as.tibble(Features)
      Time = as.character(as.vector(Features[1, 2:ncol(Features)]))
      names(Time) = NULL
      Header = Features[, 1]
      Data = t(Features[2:nrow(Features), 2:ncol(Features)])
      mode(Data) = 'numeric'
      FeaturesT = data.frame(Time = as.Date(strptime(Time, format = '%m/%d/%Y',tz=tz),tz=tz), Data)
      colnames(FeaturesT) = Header
      Liste=list(FeaturesT)
      try({
        names(Liste)=Symbol
      })
      return(Liste)
    }, error = function(e){
      try({
        remDrClient$close()
      })
      try({
        remDrServer$server$stop()
      })
      print(e)
      return(list(url))
    }
    )
  }#URL=='yahoo'
  
  if (URL == 'investing') {
    url = 'https://www.investing.com/equities/sap-ag-income-statement'
  }
  if (URL == 'msn') {
    url = 'https://www.msn.com/en-us/money/stockdetails/financials/fi-126.1.SAP.NYS'
  }
  if (URL == 'morningstar') {
  }
}