###########################################################################################
#Robustness Test 1
###########################################################################################
#Get the list of stock index options
tq_index_options()
#Get all stock symbols from SP600
df.SP600 <- tq_index("SP600")
tickers <- df.SP600$symbol
l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date)

#Get all stock symbols from Dow Global
df.SP400 <- tq_index("SP400")
tickers <- df.SP400$symbol
l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date)

###########################################################################################
#Robustness Test 2
###########################################################################################
es_good <- eventstudy(firm.returns = StockPriceReturn,
                      event.list = Good,
                      event.window = 10,
                      type = "None",
                      to.remap = TRUE,
                      remap = "cumsum",
                      inference = TRUE,
                      inference.strategy = "bootstrap")

par(mai=c(.8,.8,.2,.2), cex=.7)
plot(es_good)

es_middle <- eventstudy(firm.returns = StockPriceReturn,
                        event.list = Middle,
                        event.window = 10,
                        type = "None",
                        to.remap = TRUE,
                        remap = "cumsum",
                        inference = TRUE,
                        inference.strategy = "bootstrap")

par(mai=c(.8,.8,.2,.2), cex=.7)
plot(es_middle)

es_bad <- eventstudy(firm.returns = StockPriceReturn,
                     event.list = Bad,
                     event.window = 10,
                     type = "None",
                     to.remap = TRUE,
                     remap = "cumsum",
                     inference = TRUE,
                     inference.strategy = "bootstrap")

par(mai=c(.8,.8,.2,.2), cex=.7)
plot(es_bad)

###########################################################################################
#Robustness Test 3
###########################################################################################
Dataset %>%
  group_by(Sector) %>%
  summarise(model =  list(lm(ExcessReturn ~ `Mkt-RF` + SMB + HML, data = cur_data()))) %>%
  mutate(coeff = map(model, ~.x$coefficients[-1])) %>%
  unnest_wider(coeff) -> betas
betas
betas$model <- NULL
betas <- betas  %>% rename(Bmkt =`\`Mkt-RF\``, Bsmb = SMB, Bhml = HML)

#Merge the betas with the main Dataset
Dataset <- merge(Dataset, betas, by = "Sector")

#Event-study with industry betas
data2 = Dataset_clean[Dataset_clean$shocks == "1",]
Event <- data2[ -c(4:7, 11:17)]
Event$Date <- as.Date(Event$Date)
Event <- Event  %>% rename(when = Date)
Event <- Event  %>% rename(name = Stock)

#Replace NAs by = as the package eventstudy does not deal with NAs
Event[is.na(Event)] <- 0

rm(data2)

#Create a second dataset with a time series of returns
TS <- Dataset_clean[ -c(4:5, 7:17)]
TSS <- tapply(TS$AbnormalReturn, list(date=TS$Date, Stock=TS$Stock), mean)
TSS <- cbind(rownames(TSS),TSS)
rownames(TSS) <- NULL
colnames(TSS)[1] <- "Date"

StockPriceReturn <- zoo(TSS, order.by = as.Date(TSS[,1]))
StockPriceReturn$Date <- NULL
rm(TS)
rm(TSS)

#Ensure format is adapted to package eventstudy
str(Event)
str(StockPriceReturn)
