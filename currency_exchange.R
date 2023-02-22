## Script to convert cost in local currency to USD
## written by Anna J. Turbelin, November 14, 2022 
## aturbelin@gmail.com

library(knitr)
library(dplyr)
library(ggplot2)
library(devtools)
library(mgcv)
library(ggpubr)
library(readxl)
library(tidyverse)
library(ggpmisc)
install.packages("wbstats")
library(wbstats)
library(readr)
# library(OECD)
# library(countrycode)
# library(ISOcodes)


options(stringsAsFactors=FALSE)


## function 2.0 to adjust costs for inflation with option to have a 'neutral' fixed year' original CPI column
##' @param costdb Name of database to pull costs from that need to be converted. 
##' @param costcolumn Name of the cost column to use in \code{costdb}. Original currency raw costs to be converted.
##' @param yearcolumn Name of the year column to use. This is the column that records the year when the costs were reported or when the currency is applicable.
##' @param currencycolumn Name of the currency column to use. This is the column that records the currency in which original costs were reported.
##' @param originalyear Year of the current cost value - use when @param yearcolumn is null because costs have already been standardized to a given year.  
##' @param method Name of the method to use to convert costs to USD - currently method used in "WB" which is just the data from worldbank



currencyXchange <- function(costdb, costcolumn, yearcolumn = "Applicable_year",  currencycolumn = "Currency", originalyear = NULL, method = "WB"){
  
  if(is.null(costcolumn))
  {
    stop("Add column name of raw costs in local currency to convert to USD")
  }
  
  if(!(costcolumn %in% colnames(costdb)))
  {
    stop("The 'costcolumn' does not exist in the database, please check spelling.")
  }
  
  if(!(yearcolumn %in% colnames(costdb)))
  {
    stop("The 'yearcolumn' does not exist in the database, please check spelling.")
  }
  
  if(!(currencycolumn %in% colnames(costdb)))
  {
    stop("The 'currencycolumn' does not exist in the database, please check spelling.")
  }
  
  Xrate.dat <- wb_data("PA.NUS.FCRF", country = "all") ## get exchange rate data from world bank
  urlfile = "https://raw.githubusercontent.com/agitea/invacost_FUN/main/data/CurrencyName.csv"
  currency.name <- read_csv(url(urlfile))
  
  
  costUSD <- do.call(rbind,lapply(costdb$Cost_ID, function(x, costdb.,
                                                          cost, 
                                                          year,  
                                                          currency){
    
    currency.code = costdb.[which(costdb.$Cost_ID == x), currency]
    year.app = costdb.[which(costdb.$Cost_ID == x), year]
    currency.country <- currency.name[which(currency.name$Currency_code == currency.code), colnames(currency.name)=="ISO"]
      
     if(length(currency.country$ISO)>1){ 
       if(currency.code == "EUR"){currency.country = "EMU"} else {
         if(currency.code == "USD"){currency.country = "USA"} else {
           currency.country <- currency.name[which(currency.name$Currency_code == currency.code & currency.name$country_type != "0"), colnames(currency.name)=="ISO"]}
       } 
       } else {currency.country <- currency.country[[1:1]]}
    
    currency.country <- currency.country[[1:1]]
  
    Xchange.rate = Xrate.dat[which(Xrate.dat$iso3c == currency.country & Xrate.dat$date == year.app), colnames(Xrate.dat)=="PA.NUS.FCRF"]
    cost.raw.local = costdb.[which(costdb.$Cost_ID == x), cost]
    cost.USD <- cost.raw.local/Xchange.rate
    colnames(cost.USD) <- paste0("cost_USD")
    
    return(cost.USD)
    
    
  }, costdb. = costdb, cost = costcolumn, year = yearcolumn, currency = currencycolumn
  ))
  
  return(df <- data.frame(dplyr::bind_cols(costdb, costUSD))) #this returns a new data frame - probably best to add to current dataset but not 100% how to do it
  
  
}

