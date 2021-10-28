library(tidyverse)
##set up BAC returns 
price<-as.data.frame(BAC)
row.names(price)<-price$Date
head(price)
BAC_RETURNS<-diff(log(price$'Adj Close'))
BAC_rtn<-cbind(price[-1,], BAC_RETURNS)
head(BAC_rtn)

##market basket
MB<-as.data.frame(NASDAQ)
row.names(MB)<-MB$Date
head(MB)
MB_RETURNS<-diff(log(MB$'Adj Close'))
MB_rtn<-cbind(MB[-1,], MB_RETURNS)
head(MB_rtn)
              
##10-Year Treasury Bill

TreasuryBill<-as.data.frame(X_IRX)
head(TreasuryBill)
TreasuryB_Clean<-na.omit(TreasuryBill)
head(TreasuryB_Clean)
row.names(TreasuryB_Clean)<-TreasuryB_Clean$Date
head(TreasuryB_Clean)
TB_Price<-(1-(91*(TreasuryB_Clean$`Adj Close`/100)/360))*100
head(TB_Price)
TB_Return<-diff(log(TB_Price))
head(TB_Return)
TB_rtn<-cbind(TreasuryB_Clean[-1,], TB_Return)
head(TB_rtn)

##merge the data sets together 
MrktBasTreasuryB<-merge(MB_rtn, TB_rtn, by = 0)
head(MrktBasTreasuryB)
MrktBasTreasuryB<-MrktBasTreasuryB[,-1]
row.names(MrktBasTreasuryB)<-MrktBasTreasuryB$Date.x
head(MrktBasTreasuryB)

MBTBBAC<-as.data.frame(merge(MrktBasTreasuryB, BAC_rtn, by = 0))
head(MBTBBAC)
MBTBBAC<-MBTBBAC[,c(1, 4, 7, 10)]
head(MBTBBAC)

MBTBBAC$excessrtn_BAC<-MBTBBAC$BAC_RETURNS-MBTBBAC$TB_Return
MBTBBAC$excessrtn_MB<-MBTBBAC$MB_RETURNS-MBTBBAC$TB_Return

plot(MBTBBAC$excessrtn_MB, MBTBBAC$excessrtn_BAC)

ggplot(MBTBBAC, aes(x=excessrtn_MB, y = excessrtn_BAC)) +
  labs(title = "BAC CAPM") + xlab("Market Excess Returns") +
  ylab("BAC Excess Returns") +
  geom_point() + geom_smooth()

fitCAPM1<-lm(MBTBBAC$excessrtn_BAC ~ MBTBBAC$excessrtn_MB); summary(fitCAPM1)



