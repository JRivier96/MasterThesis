############################################################################
############################################################################
###                                                                      ###
###                     Run the Event Study                              ###
###                                                                      ###
###                                                                      ###
############################################################################
############################################################################

#Create a first dataset with all the dates of the shocks and ESG Scores
data2 = Dataset_clean[Dataset_clean$shocks == "1",]
Event <- data2[ -c(3:6, 11:17)]
Event$Date <- as.Date(Event$Date)
Event <- Event  %>% rename(when = Date)
Event <- Event  %>% rename(name = Stock)

#Replace NAs by = as the package eventstudy does not deal with NAs
Event[is.na(Event)] <- 0

rm(data2)

#Create a second dataset with a time series of returns
TS <- Dataset_clean[ -c(3:4, 6:17)]
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

#Aim at representing only one industry, one shock, or one year
X <- Event[Event$Sector == "Energy",]

Good <- X[X$Score2015 >= quantile(unique(X$Score2015), .66) & 
            X$when <= "2015-07-06" & X$when >= "2015-01-02",]

Middle <- X[X$Score2015 > quantile(unique(X$Score2015), .33) & 
              X$Score2015 < quantile(unique(X$Score2015), .66) & 
              X$when <= "2015-07-06" & X$when >= "2015-01-02",]

Bad <- X[X$Score2015 <= quantile(unique(X$Score2015), .33) & X$Score2015 > 0 & 
           X$when <= "2015-07-06" & X$when >= "2015-01-02",]

#Run the Event Study and plot it
quartz()
par(mfrow=c(3,1))

#es <- phys2eventtime(z=StockPriceReturn, events=Good, width=10)
#es.w <- window(es$z.e, start=-10, end=10)
#es.cs <- remap.cumsum(es.w,is.pc=FALSE,base=0)

es_good <- eventstudy(firm.returns = StockPriceReturn,
                      event.list = Good,
                      event.window = 5,
                      type = "None",
                      to.remap = TRUE,
                      remap = "cumsum",
                      inference = TRUE,
                      inference.strategy = "bootstrap")

par(mai=c(.8,.8,.2,.2), cex=.7)
plot(es_good)

es_middle <- eventstudy(firm.returns = StockPriceReturn,
                      event.list = Middle,
                      event.window = 5,
                      type = "None",
                      to.remap = TRUE,
                      remap = "cumsum",
                      inference = TRUE,
                      inference.strategy = "bootstrap")

par(mai=c(.8,.8,.2,.2), cex=.7)
plot(es_middle)

es_bad <- eventstudy(firm.returns = StockPriceReturn,
                     event.list = Bad,
                     event.window = 5,
                     type = "None",
                     to.remap = TRUE,
                     remap = "cumsum",
                     inference = TRUE,
                     inference.strategy = "bootstrap")

par(mai=c(.8,.8,.2,.2), cex=.7)
plot(es_bad)

write.csv(es_good$result, "testG.csv")
write.csv(es_middle$result, "testM.csv")
write.csv(es_bad$result, "testB.csv")

#Excel is used for results overview

