GetCashFlow=function(Symbol='SAP',URL='yahoo',Silent=TRUE){
  #GetCashFlow('SAP')
  
  library(tidyverse)
  library(rvest)
  if (URL == 'yahoo') {
    tryCatch({
      library(RSelenium)
      library(wdman)
      library(dplyr)
      #clicks via chrome rightklick on inspect, then you can copy the relevant strings for button clicks
      cDrv <- chrome(verbose = !Silent)
      eCaps <-
        list(chromeOptions = list(
          args = c('--headless', '--disable-gpu', '--window-size=1280,800')
        ))
      remDr <-
        remoteDriver(
          browserName = "chrome",
          port = 4567L,
          extraCapabilities = eCaps
        )
      #suppressMessages(remDr$open())
      msg = utils::capture.output(remDr$open())
      if (!Silent) {
        print(msg)
      }
      #if you want to see whats happening
      #driver=rsDriver(port = 4567L,browser = "chrome")
      #remDr=driver[["client"]]
      
      url = paste0("https://finance.yahoo.com/quote/",
                   Symbol,
                   "/cash-flow?p=",
                   Symbol)
      remDr$navigate(url)
      #copy selector then put it here
      webElem <-
        remDr$findElement(
          using = 'css selector',
          'body > div.consent-wizard.eu-single-page > div.consent-wizard-body.eu-single-page > div.consent-steps-container > div > div.single-page-forms.yahoo > form.consent-form.single-page-form.single-page-agree-form > div > input'
        )
      webElem$clickElement()
      
      #author of this part: Hamza Tayyab
      # get annual data
      annual <- xml2::read_html(remDr$getPageSource()[[1]])
      annual <- annual %>% html_table(fill = T, header = F)
      #%>% .[[1]]  %>% filter(X1!=X2)
      #annual
      
      # click on Quarterly button
      #copy xpath then put it here
      webElem <-
        remDr$findElement(using = 'xpath',
                          '//*[@id="Col1-1-Financials-Proxy"]/section/div[1]/div[2]/button')
      webElem$clickElement()
      
      # get quarterly data
      quarterly <- xml2::read_html(remDr$getPageSource()[[1]])
      quarterly <- quarterly %>% html_table(fill = T, header = F) %>% .[[1]]  %>% filter(X1 != X2)
      
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
      FeaturesT = data.frame(Time = as.Date(strptime(Time, format = '%m/%d/%Y')), Data)
      colnames(FeaturesT) = Header
      return(FeaturesT)
    }, error = function(e)
      return(url))
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