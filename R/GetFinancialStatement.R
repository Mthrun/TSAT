GetFinancialStatement=function(Symbol='SAP',URL='yahoo',Silent=TRUE,Port=4445L,tz="UTC",...){
#   #GetFinancialStatement('SAP')
  #Symbol='SAP'
  #URL='morningstar'
  requireNamespace('xml2')
  requireNamespace('dplyr')
  requireNamespace('rvest')
  if (URL == 'yahoo') {
    tryCatch({
      library(RSelenium)
      #library(wdman)
      
     # library(dplyr)
      
      # Intializing the firefox driver at the given port
      remDrServer <- RSelenium::rsDriver(browser = "firefox",port=Port,...)
      # Intializing the server on the same port
      remDrClient <- RSelenium::remoteDriver$new(port=Port)
      #remDr
      
      #suppressMessages(remDr$open())
      
      msg = utils::capture.output(remDrClient$open(silent = TRUE))
      if (!Silent) {
        print(msg)
      }
      #if you want to see whats happening
      #driver=rsDriver(port = 4567L,browser = "chrome")
      #remDrClient=driver[["client"]]
      url = paste0("https://finance.yahoo.com/quote/",
                   Symbol,
                   "/financials?p=",
                   Symbol)
      #url = paste0("https://finance.yahoo.com/quote/SAP/financials?p=SAP")
      remDrClient$navigate(url)
      #copy selector then put it here
      ######################### This paart here is not needed. It was the requirement in Chrome, not in FireFox.###############
      try({
      webElem = remDrClient$findElement(using = 'name','agree')
      # webElem <-
      #   remDrClient$findElement(
      #     using = 'css selector',
      #     'body > div.consent-wizard.eu-single-page > div.consent-wizard-body.eu-single-page
      #     > div.consent-steps-container > div > div.single-page-forms.yahoo >
      #     form.consent-form.single-page-form.single-page-agree-form > div > input')
      webElem$clickElement()
      })
      #author of this part: Hamza Tayyab
      # get annual data
      
      # annual <- xml2::read_html(remDrClient$getPageSource()[[1]])
      # annual <- annual %>% rvest::html_table(fill = T, header = F)%>% .[[1]]  %>% filter(X1!=X2)
      # annual
      
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
      
      # quarterly <- quarterly %>% rvest::html_table(fill = T, header = F) %>% .[[1]]  %>% filter(X1 != X2)
      # quarterly
      #return(quarterly)
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
    })
  }
  #URL=='yahoo'
  
  if (URL == 'investing') {
    url = 'https://www.investing.com/equities/sap-ag-income-statement'
  }
  if (URL == 'msn') {
    url = 'https://www.msn.com/en-us/money/stockdetails/financials/fi-126.1.SAP.NYS'
  }
  if (URL == 'morningstar') {
    url = paste0(
      'http://financials.morningstar.com/ajax/ReportProcess4CSV.html?t=XFRA:',
      Symbol,
      '&reportType=is&period=3&dataType=A&order=asc&denominatorView=raw&columnYear=5&number=3'
    )
    
    tryCatch({
      raw = read.csv(
        file = url,
        header = T,
        sep = ',',
        skip = 1,
        stringsAsFactors = F
      )
      DF = raw[, c(2:6)]
      Header = raw[, 1]
      Time = gsub('X', '', colnames(raw)[2:6])
      DF = t(DF)
      colnames(DF) = Header
      rownames(DF) = Time
      DF[!is.finite(DF)] = NaN
      
      inddel = c()
      share = 'Earnings per share'
      indshare = which(Header == share)
      if (length(indshare) > 0) {
        colnames(DF)[indshare + 1] = paste(colnames(DF)[indshare + 1], share)
        colnames(DF)[indshare + 2] = paste(colnames(DF)[indshare + 2], share)
        inddel = c(inddel, indshare)
      }
      weight = 'Weighted average shares outstanding'
      indweight = which(Header == weight)
      if (length(indweight) > 0) {
        colnames(DF)[indweight + 1] = paste(colnames(DF)[indweight + 1], weight)
        colnames(DF)[indweight + 2] = paste(colnames(DF)[indweight + 2], weight)
        inddel = c(inddel, indweight)
      }
      
      other1 = 'Other income (expense)'
      indother1 = which(Header == other1)
      if (length(indweight) > 0) {
        indother2 = which(colnames(DF) == other1)
        indtemp1 = which(Header == "Total operating expenses")
        indtemp2 = which(Header == "Total nonoperating income, net")
        if (length(indtemp1))
          colnames(DF)[indother2[1]] = paste(colnames(DF)[indtemp1], other1)
        else
          colnames(DF)[indother2[1]] = paste('FirstListed', other1[1])
        if (length(indtemp1))
          colnames(DF)[indother2[2]] = paste(colnames(DF)[indtemp2], other1)
        else
          colnames(DF)[indother2[2]] = paste('SecondListed', other1)
      }
      
      indoperating = which(Header == "Operating expenses")
      if (length(indoperating) > 0)
        inddel = c(inddel, indoperating)
      
      if (length(inddel) > 0)
        DF = DF[, -inddel]
      
      HeaderNew = colnames(DF)
      
      indOther = which(HeaderNew == "Other operating expenses")
      if (length(indOther) > 0)
        DF[is.nan(DF[, indOther]), indOther] = 0
      
      indOther2 = which(HeaderNew == "Other")
      if (length(indOther2) > 0)
        DF[is.nan(DF[, indOther2]), indOther2] = 0
      
      indOther3 = which(HeaderNew == "Preferred dividend")
      if (length(indOther3) > 0)
        DF[is.nan(DF[, indOther3]), indOther3] = 0
      
      
      print (DF)
    }, error = function(e)
      print (url))
  }
}
