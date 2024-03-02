
library(quantmod)

# display a simple bar chart
getSymbols(c("AMZN"))
barChart(AMZN,theme='white.mono',bar.type='hlc')

# display a complex chart
getSymbols(c("^GSPC"))
chartSeries(GSPC, subset='last 3 months')
addBBands(n = 20, sd = 2, ma = "SMA", draw = 'bands', on = -1)

# get market data for all symbols making up the NASDAQ 100 Index
Nasdaq100_Symbols <- c(
  "ADBE", "AMD", "ALGN", "GOOGL", "GOOG", "AMZN", "AEP", "AMGN", 
  "ADI", "ANSS", "AAPL", "AMAT", "ASML", "TEAM", "ADSK", "ADP", "BIDU", "BIIB", 
  "BKNG", "AVGO", "CDNS", "CDW", "CHTR", "CHKP", "CTAS", "CSCO", 
  "CTSH", "CMCSA", "CPRT", "COST", "CSX", "DXCM", "DOCU", "DLTR", "EBAY", "EA", 
  "EXC", "FAST", "FOXA", "FOX", "GILD", "IDXX", "ILMN", "INCY", 
  "INTC", "INTU", "ISRG", "JD", "KDP", "KLAC", "KHC", "LRCX", "LULU", "MAR", 
  "MRVL", "MTCH", "MELI", "MCHP", "MU", "MSFT", "MRNA", "MDLZ", "MNST", 
  "NTES", "NFLX", "NVDA", "NXPI", "ORLY", "OKTA", "PCAR", "PAYX", "PYPL", "PTON", 
  "PEP", "PDD", "QCOM", "REGN", "ROST", "SIRI", "SWKS", "SPLK", "SBUX", 
  "SNPS", "TMUS", "TSLA", "TXN", "TCOM", "VRSN", "VRSK", "VRTX", "WBA", "WDAY", 
  "XEL", "ZM"
)

getSymbols(Nasdaq100_Symbols)

# merge them all together
nasdaq100 <- data.frame(as.xts(merge(
  ADBE, AMD, ALGN, GOOGL, GOOG, AMZN, AEP, AMGN, 
  ADI, ANSS, AAPL, AMAT, ASML, TEAM, ADSK, ADP, BIDU, BIIB, 
  BKNG, AVGO, CDNS, CDW, CHTR, CHKP, CTAS, CSCO, 
  CTSH, CMCSA, CPRT, COST, CSX, DXCM, DOCU, DLTR, EBAY, EA, 
  EXC, FAST, FOXA, FOX, GILD, IDXX, ILMN, INCY, 
  INTC, INTU, ISRG, JD, KDP, KLAC, KHC, LRCX, LULU, MAR, 
  MRVL, MTCH, MELI, MCHP, MU, MSFT, MRNA, MDLZ, MNST, 
  NTES, NFLX, NVDA, NXPI, ORLY, OKTA, PCAR, PAYX, PYPL, PTON, 
  PEP, PDD, QCOM, REGN, ROST, SIRI, SWKS, SPLK, SBUX, 
  SNPS, TMUS, TSLA, TXN, TCOM, VRSN, VRSK, VRTX, WBA, WDAY, 
  XEL, ZM
)))
head(nasdaq100[,1:12],2)

# set outcome variable
outcomeSymbol <- 'MSFT.Volume'

# shift outcome value to be on same line as predictors
library(xts)
nasdaq100 <- xts(nasdaq100,order.by=as.Date(rownames(nasdaq100)))
# print(rownames(nasdaq100))
nasdaq100 <- as.data.frame(merge(nasdaq100, lm1=lag(nasdaq100[,outcomeSymbol],-1)))
nasdaq100$outcome <- ifelse(nasdaq100[,paste0(outcomeSymbol,'.1')] > nasdaq100[,outcomeSymbol], 1, 0)

# remove shifted down volume field as we don't care by the value
nasdaq100 <- nasdaq100[,!names(nasdaq100) %in% c(paste0(outcomeSymbol,'.1'))]

# cast date to true date and order in decreasing order
nasdaq100$date <- as.Date(row.names(nasdaq100))
nasdaq100 <- nasdaq100[order(as.Date(nasdaq100$date, "%m/%d/%Y"), decreasing = TRUE),]

# calculate all day differences and populate them on same row
GetDiffDays <- function(objDF,days=c(10), offLimitsSymbols=c('outcome'), roundByScaler=3) {
  # needs to be sorted by date in decreasing order
  ind <- sapply(objDF, is.numeric)
  for (sym in names(objDF)[ind]) {
    if (!sym %in% offLimitsSymbols) {
      print(paste('*********', sym))
      objDF[,sym] <- round(scale(objDF[,sym]),roundByScaler)
      
      print(paste('theColName', sym))
      for (day in days) {
        objDF[paste0(sym,'_',day)] <- c(diff(objDF[,sym],lag = day),rep(x=0,day)) * -1
      }
    }
  }
  return (objDF)
}

# call the function with the following differences
nasdaq100 <- GetDiffDays(nasdaq100, days=c(1,2,3,4,5,10,20), offLimitsSymbols=c('outcome'), roundByScaler=2)

# drop most recent entry as we don't have an outcome
nasdaq100 <- nasdaq100[2:nrow(nasdaq100),]

# take a peek at YHOO features:
dput(names(nasdaq100)[grepl('AAPL.',names(nasdaq100))])

# well use POSIXlt to add day of the week, day of the month, day of the year
nasdaq100$wday <- as.POSIXlt(nasdaq100$date)$wday
nasdaq100$yday <- as.POSIXlt(nasdaq100$date)$mday
nasdaq100$mon<- as.POSIXlt(nasdaq100$date)$mon

# remove date field and shuffle data frame
nasdaq100 <- subset(nasdaq100, select=-c(date))
#nasdaq100 <- nasdaq100[sample(nrow(nasdaq100)),]

# let's model
#install.packages("caret")
library(caret)
predictorNames <- names(nasdaq100)[names(nasdaq100) != 'outcome']

set.seed(1234)
split <- sample(nrow(nasdaq100), floor(0.7*nrow(nasdaq100)))
train <-nasdaq100[split,]
test <- nasdaq100[-split,]
#install.packages("xgboost")
library(xgboost)
bst <- xgboost(data = as.matrix(train[,predictorNames]),
               label = train$outcome,
               verbose=0,
               eta = 0.08,
               gamma = 50,
               nround = 55,
               missing = NaN,
               colsample_bytree = 0.5,
               subsample = 0.99,
               objective="binary:logistic")

predictions <- predict(bst, as.matrix(test[,predictorNames]), outputmargin=TRUE)

#install.packages("pROC")
library(pROC)
auc <- auc(test$outcome,predictions)
print(paste('AUC score:', auc))

