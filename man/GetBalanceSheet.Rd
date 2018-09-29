\name{GetBalanceSheet}
\alias{GetBalanceSheet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GetBalanceSheet
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GetBalanceSheet(Symbol = "SAP", URL = "yahoo", Silent = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Symbol}{
%%     ~~Describe \code{Symbol} here~~
}
  \item{URL}{
%%     ~~Describe \code{URL} here~~
}
  \item{Silent}{
%%     ~~Describe \code{Silent} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Symbol = "SAP", URL = "yahoo", Silent = TRUE) 
{
    library(tidyverse)
    library(rvest)
    if (URL == "yahoo") {
        tryCatch({
            library(RSelenium)
            library(wdman)
            library(dplyr)
            cDrv <- chrome(verbose = !Silent)
            eCaps <- list(chromeOptions = list(args = c("--headless", 
                "--disable-gpu", "--window-size=1280,800")))
            remDr <- remoteDriver(browserName = "chrome", port = 4567L, 
                extraCapabilities = eCaps)
            msg = utils::capture.output(remDr$open())
            if (!Silent) {
                print(msg)
            }
            url = paste0("https://finance.yahoo.com/quote/", 
                Symbol, "/balance-sheet?p=", Symbol)
            remDr$navigate(url)
            webElem <- remDr$findElement(using = "css selector", 
                "body > div.consent-wizard.eu-single-page > div.consent-wizard-body.eu-single-page > div.consent-steps-container > div > div.single-page-forms.yahoo > form.consent-form.single-page-form.single-page-agree-form > div > input")
            webElem$clickElement()
            annual <- xml2::read_html(remDr$getPageSource()[[1]])
            annual <- annual \%>\% html_table(fill = T, header = F)
            webElem <- remDr$findElement(using = "xpath", "//*[@id=\"Col1-1-Financials-Proxy\"]/section/div[1]/div[2]/button")
            webElem$clickElement()
            quarterly <- xml2::read_html(remDr$getPageSource()[[1]])
            quarterly <- quarterly \%>\% html_table(fill = T, header = F) \%>\% 
                .[[1]] \%>\% filter(X1 != X2)
            Features = sapply(quarterly, function(x) gsub(",", 
                "", x))
            Features[1, 1] = "Time"
            Time = as.character(as.vector(Features[1, 2:ncol(Features)]))
            names(Time) = NULL
            Header = Features[, 1]
            Data = t(Features[2:nrow(Features), 2:ncol(Features)])
            mode(Data) = "numeric"
            FeaturesT = data.frame(Time = as.Date(strptime(Time, 
                format = "\%m/\%d/\%Y")), Data)
            colnames(FeaturesT) = Header
            return(FeaturesT)
        }, error = function(e) return(url))
    }
    if (URL == "investing") {
        url = "https://www.investing.com/equities/sap-ag-income-statement"
    }
    if (URL == "msn") {
        url = "https://www.msn.com/en-us/money/stockdetails/financials/fi-126.1.SAP.NYS"
    }
    if (URL == "morningstar") {
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
