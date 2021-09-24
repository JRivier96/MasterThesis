############################################################################
############################################################################
###                                                                      ###
###                             Define Commodity                         ###
###                              Price Shocks                            ###
###                                                                      ###
############################################################################
############################################################################

setwd("/Users/julienrivier/Desktop/Master Thesis/R-Code/R-Inputs")

library(readxl)
library(xts)
library(ggplot2)
library(forecast)
library(dynlm)
library(lmtest)
library(sandwich)
library(BatchGetSymbols)
library(testthat)
library(eventstudies)
library(tidyverse)
library(writexl)
library(xlsx)
library(tseries)
library(xtable)
library(stargazer)
library(matrixStats)
library(lubridate)
library(Hmisc)
library(corrplot)
library(plm)
library(naniar)
library(PerformanceAnalytics)
library(tidyquant)
library(tidyverse)

##Download Index, depending on the index, ar model will need to be adapted - CHANGE NUMBERS
#2 = All Commodities
#3 = Agriculture
#4 = WTI Crude Oil
#5 = Brent Crude
#6 = Natural Gas
#7 = Coffee
#8 = Soybeans
#9 = Copper
#10 = Energy
#11 = Gold
#12 = Industrial Metals
#13 = Precious Metals
#14 = Corn
#12 = Silver
#13 = Wheat
#17 = all from 2010
WorldCommodityIndex <- read_excel("BCOM.xlsx",2)

##Check data type
sapply(WorldCommodityIndex, class)

#Format date column as well as convert BCOMIndexPrice to time series data, we only keep the date
#we have for the stock price (S&P 500).
Index <- xts(WorldCommodityIndex[,-1], order.by=as.Date(WorldCommodityIndex$Date, format = "%m/%d/%Y"))
IndexDF <- zoo(Index, order.by = index(Index))
BCOMIndexPrice <- IndexDF$IndexPrice

#Plot the index (Youtube video)
plot(BCOMIndexPrice, main = "CPI", ylab = "Index Price", xlab = "Time")
summary(BCOMIndexPrice)
BCOMIndexPrice %>% diff() %>% ggtsdisplay(main="", lag.max = 10)
pacf(ts(BCOMIndexPrice %>% diff()), lag.max = 10, main = "PACF CPI", ylab = "Correlation")

#Check for stationarity - Dickey Fueller Test
adf.test(BCOMIndexPrice, alternative = "stationary", k=0)

#Next step is to determine the optimal number of lags, then isolate the residuals 
#AR(p)
#Define a function with inputs that will be needed to compute needed outputs. Round to 3 decimals
#Define p as the number of lags, BIC and AIC as criteria for model selection, and the R2
IC <- function(model) {
  # Input: dynlm object
  ssr <- sum(model$residuals^2)
  t <- length(model$residuals)
  npar <- length(model$coef)
  
  return(
    round(c("p" = npar - 1,
            "BIC" = log(ssr/t) + npar * log(t)/t,
            "AIC" = log(ssr/t) + npar * 2/t,
            "R2" = summary(model)$r.squared), 3)
  )
}

#Create a loop to generate AR with different lags (1:i) 
for (i in 1:8){
  model_tmp <- dynlm(log(BCOMIndexPrice) ~ 
                       L(log(BCOMIndexPrice), 1:i) + trend(BCOMIndexPrice), 
                     data = IndexDF)
  results_tmp <- IC(model_tmp)
}

#Display returned variables for all regression and compare across different lags
order <- 1:8
ICs <- sapply(order, function(x) IC(dynlm(log(BCOMIndexPrice) ~ 
                                            L(log(BCOMIndexPrice), 1:x) + trend(BCOMIndexPrice), 
                                          data = IndexDF)))
t(ICs)

#We decide to choose 3 lags as the BIC and AIC remain relatively similar
#We add a time trend to follow Kinda practices
ar3 <- dynlm(log(BCOMIndexPrice) ~  L(log(BCOMIndexPrice), 1:3) + trend(BCOMIndexPrice))
coeftest(ar3, vcov. = sandwich)

#Define residuals as shocks, based on the paper of Kinda et al. (2018)
residuals <- residuals(ar3)
checkresiduals(ar3)
plot(residuals)
#Plot all residuals
ggplot(residuals, aes(x=residuals)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="grey", binwidth = 0.001) +
  geom_density(alpha=.1, color = "red") + theme_classic()

#Check how many values are above 0.03 and below -0.03 and define them as shocks
No_Shocks <- sum(residuals <= quantile(residuals, .02), 1, 
                 ifelse(residuals >= quantile(residuals, .98), 1, 0))

#NEGATIVE SHOCKS ONLY
No_Neg_Shocks <- sum(residuals <= quantile(residuals, .02), 1, 0)

#Create a dummy variable for the shocks, namely 1 if in the upper/ lower 1% quantile and 0 otherwise
#shocks <- ifelse(residuals >= 0.03 | residuals <= -0.03, 1, 0)

#shocks <- ifelse(residuals <= quantile(residuals, .01), 1, 
#                 ifelse(residuals >= quantile(residuals, .99), 1, 0))

#NEGATIVE SHOCKS ONLY
shocks <- ifelse(residuals <= quantile(residuals, .02), 1, 0)

#Merge residuals with original IndexPrice of the corresponding year, and add the dummy previously created 
Final <- cbind(BCOMIndexPrice, residuals)
FinalDF <- cbind(Final, shocks)
rm(Final)

FinalShockDF <- data.frame(FinalDF)
rm(FinalDF)
############################################################################
############################################################################
###                                                                      ###
###                             Retrieve Stock Price                     ###
###                             Of S&P 500 Companies                     ###
###                                                                      ###
############################################################################
############################################################################

#Retrieve S&P500 component stock prices between 2002 and 2020
#install.packages("BatchGetSymbols")

first.date <- as.Date("2002-01-01")
last.date <- as.Date("2020-12-31")

#Get all stocks from the S&P500 that have enough data (threshold is put at 75%)
#df.SP500 contains industries of each stock
df.SP500 <- GetSP500Stocks()
tickers <- df.SP500$Tickers

l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date)

#clean dataset by deleting, renaming and reordering columns
SP500_Stocks <- subset(l.out$df.tickers, select = -c(price.open, 
                                                     price.high, price.low, 
                                                     volume, price.adjusted, 
                                                     ret.adjusted.prices, 
                                                     ret.closing.prices))

SP500_Stocks <- SP500_Stocks %>% rename(ClosePriceStock = price.close, DateStock = ref.date , Ticker =  ticker)

SP500_Stocks_Clean <- SP500_Stocks[c(2,3,1)]

rm(SP500_Stocks)

#Split the dataframe into individual dataframe corresponding to each stock (incl. stocks that
#have been added to the S&P between first and last date, we look back to 2002)
d <- split(SP500_Stocks_Clean, SP500_Stocks_Clean$Ticker)

#Merge all stocks by the date
SP500Stocks_Final <- d[c(1:length(d))] %>% reduce(left_join, by = "DateStock")
rm(SP500_Stocks_Clean)
rm(d)

#Remove stocks that have no data for all dates
SP500Stocks_Final1 <- SP500Stocks_Final[ , colSums(is.na(SP500Stocks_Final)) == 0]
rm(SP500Stocks_Final)

#Clean dataset by removing columns with tickers and use these latters as column titles
idx <- grep('Ticker', names(SP500Stocks_Final1))
names(SP500Stocks_Final1)[idx+1] <- sapply(SP500Stocks_Final1[idx], unique)
SP500Stocks_Final1[idx] <- NULL

#Create a dataset with daily ACTUAL returns of all S&P500 Stock
SP500_Returns <- data.frame(cbind.data.frame(SP500Stocks_Final1$DateStock[-1],diff(as.matrix(log(SP500Stocks_Final1[,-1])))))
SP500_Returns_Final <- SP500_Returns %>% rename(Date = SP500Stocks_Final1.DateStock..1.)

#Remove intermediary objects for the sake of simplicity
rm(SP500_Returns)
rm(SP500Stocks_Final1)

#Create the date format to correspond with the commodity price index
SP500 <- xts(SP500_Returns_Final[,-1], order.by=as.Date(SP500_Returns_Final$Date, format = "%m/%d/%Y"))
SP500Final <- zoo(SP500, order.by = index(SP500))
SP500Clean <- data.frame(SP500Final)
rm(SP500Final)
rm(SP500_Returns_Final)
rm(SP500)

############################################################################
############################################################################
###                                                                      ###
###                     Check the impact of Commodity                    ###
###                     Price Shocks on Stock Returns                    ###
###                                                                      ###
############################################################################
############################################################################

#Merge the two datasets, namely the commodity index and the stock returns by date
library(timeSeries)
X <- as.zoo(merge(as.timeSeries(FinalShockDF), as.timeSeries(SP500Clean)))
MergedDF <- data.frame(X)
rm(X)

#Remove all rows containing NAs, due to missing data (e.g., closed stock market)
VF1 <- MergedDF[complete.cases(MergedDF), ]
rm(MergedDF)
#Shows that we had 27 rows with missing data

#Create a real row for dates with the name "Date"
VF <- cbind(rownames(VF1), VF1)
rownames(VF) <- NULL
colnames(VF) <- c(names(VF))
colnames(VF)[1] <- "Date" 
rm(VF1)

#Transform into panel data
VFF <- pivot_longer(VF, cols=5:length(VF), names_to = "Stock", values_to = "Return")
VFF <- VFF %>% relocate(Stock, .before = Date)
rm(VF)
rm(SP500Clean)

#plot some data to illustrate what we currently have
#plot a specific stock across the entire time frame
#ggplot(VF, aes(x=Date, y= ACN), axis.ticks = element_blank())

#ggplot(VF, aes(x = Date, y = AIG, group = shocks, color = shocks), axis.ticks = element_blank())+ geom_line()

#We wish to distinguish between different indsutries, therefore we download an external excel file with
#SP500 stocks tickers and their respective sector
SP500Sectors <- subset(df.SP500, select = -c(Company, SEC.filings, HQ.Location, Date.First.Added,
                                             CIK, Founded))

SP500Sectors <- SP500Sectors %>% rename(Sector = GICS.Sector, SubIndustry = GICS.Sub.Industry,
                                        Stock = Tickers)

#Merge VFF with the sector and subindustry dataset
dataset <- merge(VFF, SP500Sectors, by = "Stock")
rm(SP500Sectors)
rm(VFF)
rm(df.SP500)
rm(l.out)

#Create a table with analysed stocks as well as their sector and sub-industry
StockIndustry <- subset(dataset, select = c(Stock, Sector, SubIndustry))
#Remove duplicates
StockIndustry <- StockIndustry %>% distinct() 

table_2 <- dataset %>% distinct(Stock, Sector) %>%
  group_by(Sector) %>% summarise(Stock=n())

#Export to LatEx
print(xtable(table_2, type = "latex"), file = "table_2.tex")
write_xlsx(StockIndustry,"/Users/julienrivier/Desktop/Master Thesis/R-Inputs\\StockIndustry.xlsx")

###########################################################################################
#Now we wish to compute abnormal returns, and thus compute expected returns with FamaFrench
###########################################################################################
#Import control variables Fama & French
FamaFrench <- as.data.frame(read_xls("F-F_Research_Data_Factors_daily.xls", sheet = 2))
#rename the date column to be consistent with remaining datasets
FamaFrench <- FamaFrench  %>% rename(Date = ...1)
FamaFrench1 <- xts(FamaFrench[], order.by=as.Date(FamaFrench$Date, format = "%m/%d/%Y"))

#stargazer(FamaFrench,type="html",rownames = FALSE, title="FF",out="latex/FF.html", summary=FALSE)
#stargazer(FamaFrench,type="latex", rownames = FALSE, title="FF",out="latex/FF.tex", summary=FALSE)

#Merge FamaFrench with our main dataset
Dataset <- merge.data.frame(dataset, FamaFrench1, by = "Date", all.x = TRUE) 
rm(dataset)
rm(FamaFrench)
#Transform returns in percentage to correspond to Fama French Factors
Dataset$AcReturns <- as.numeric(as.character(Dataset$Return)) * 100
Dataset$Return <- NULL
Dataset <- Dataset %>% relocate(AcReturns, .after = Stock)

#Compute Excess Returns
Dataset$ExcessReturn <- (as.numeric(as.character(Dataset$AcReturns)) - as.numeric(as.character(Dataset$RF)))
Dataset <- Dataset %>% relocate(ExcessReturn, .after = AcReturns)
Dataset$`Mkt-RF` <- as.numeric(as.character(Dataset$`Mkt-RF`))
Dataset$SMB <- as.numeric(as.character(Dataset$SMB))
Dataset$HML <- as.numeric(as.character(Dataset$HML))

#Store the different betas of each stock, in order to further predict the expected return
Dataset %>%
  group_by(Stock) %>%
  summarise(model =  list(lm(ExcessReturn ~ `Mkt-RF` + SMB + HML, data = cur_data()))) %>%
  mutate(coeff = map(model, ~.x$coefficients[-1])) %>%
  unnest_wider(coeff) -> betas
betas
betas$model <- NULL
betas <- betas  %>% rename(Bmkt =`\`Mkt-RF\``, Bsmb = SMB, Bhml = HML)

#Merge the betas with the main Dataset
Dataset <- merge(Dataset, betas, by = "Stock")

#Add a column with the Expected Return by following the Fama French Formula, add the abnormal returns and
#rearrange the dataset
Dataset$ExReturn <- (as.numeric(as.character(Dataset$RF)) + as.numeric(as.character(Dataset$`Mkt-RF`)) *
                           Dataset$Bmkt + as.numeric(as.character(Dataset$SMB)) * Dataset$Bsmb +
                           as.numeric(as.character(Dataset$HML)) * Dataset$Bhml)

Dataset <- Dataset %>% relocate(ExReturn, .after = AcReturns)

Dataset$AbnormalReturn <- (Dataset$AcReturns - Dataset$ExReturn)

Dataset <- Dataset %>% relocate(AbnormalReturn, .after = ExReturn)

Dataset <- Dataset %>% relocate(Date, .before = Stock)

Dataset$ExcessReturn <- NULL

#Download ESG Score of the S&P500 firms from Eikon, sheet depends on the score we want to use
#2 = ESG Combined Score
#3 = ESG Score - YES
#4 = Resource Use Score - YES
#5 = Resource Use Score from 2010- YES

ESG_Scores <- read_excel("SP500_ESG_Scores.xlsx",4)
ESG_Scores$`Company Name` <- NULL
ESG_Scores <- ESG_Scores  %>% rename(Stock =`Identifier (RIC)`)

Dataset_ESG <- merge(Dataset, ESG_Scores, by = "Stock")

#Convert the date column to character to be able to plot some data
Dataset_ESG$Date <- as.character(Dataset_ESG$Date)

#Remove Stocks that have no 2020 ESG Scores available
Dataset_clean <- Dataset_ESG[!is.na(Dataset_ESG$Score2020),]

#Summary stats for ESG resource Scores
summary(unique(Dataset_clean$Score2020))
unique(Dataset_clean$Score2020)

#######Run Event-Study Now

ShocksDates <- as.data.frame(unique(Event$when))
TotalDates <- as.data.frame(unique(as.Date(Dataset_clean$Date)))
#write_xlsx(ShocksDates,"/Users/julienrivier/Desktop/Master Thesis/R-Inputs\\ShocksDates.xlsx")
#write_xlsx(TotalDates,"/Users/julienrivier/Desktop/Master Thesis/R-Inputs\\TotalDates.xlsx")
