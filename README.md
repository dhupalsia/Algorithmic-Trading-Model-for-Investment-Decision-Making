# Algorithmic-Trading-Model-for-Investment-Decision-Making
R-script file for the project 
# 
# 
#### 
########### installing packages ####### install.packages ("httr") 
install.packages ("zoo") 
install.packages ("xts") 
install.packages("XML") 
install.packages ("lubridate") 
install.packages ("curl") 
install.packages ("xml2") 
install.packages ("stringr") 
install.packages("magrittr") 
install.packages ("dplyr") install.packages ("tidyr") 
install.packages("readr") 
install.packages ("pdfetch") 
install.packages ("rugarch") 
install.packages ("ggplot2") 
install.packages ("TTR") 
install.packages ("quantmod") 
install.packages ("rio") 
install formats ("arrow") 
install.packages ("readx1") 
# 
install.packages ("PerformanceAnalytics") 
install.packages ("contrib.url") 
########### loading packages #######: 
library (httr) 
# 
# 
+F 
library (zoo) 
library (XML) 
library (lubridate) 
library (curl) 
library (xm12) 
library (stringr) 
library (magrittr) 
library (dplyr) 
library (tidyr) 
library (readr) 
library (pdfetch) 
library (rugarch) library(ggplot2) library (TTR) 
library (quantmod) 
library (rio) 
# 
# 
### 
library (readx1) 
library (PerformanceAnalytics) 
library (xts) 
library (compiler) 
library (tinytex) 
########### fetching stock data for S&P500 ############# 
gspc<-pdfetch_YAHOO ("^gspc", fields = "close", from = as.Date("2018-01-01"), to = as.Date("2023-01-01"), interval = "1d") 
head (gspc) 
View (gspc) 
autoplot.zoo (gspc) 
#computing continuously compounded returns 
gspcMonthlyReturnsC = na.omit(diff (log (gspc))) 
head (gspcMonthlyReturnsC) 
########### fetching stock data for McDonalds ############# 
mcd<-pdfetch_YAHOO ("MCD", fields = c("open", "close", "volume"), from = as.Date("2018-01- 01"), to as.Date("2023-01-01"), interval 
head (mcd) 
= 
= 
"1d") 
View (mcd) 
## 
E 
autoplot.zoo (mcd$MCD.close) 
########### technical analysis ####: 
#technical indicators 
#simple moving average 
sma<-SMA (mcd$MCD.close,n=20) 
#### 
tail (sma, n=5) 
chartSeries (mcd$MCD.close, subset = '2022-01::2023-01') addSMA (n=30,on=1, col = "blue") #30 DAYS MA 
addSMA (n=200,on=1, col 
#bollinger band 
= 
"red") #200 DAYS MA 
bb<-BBands (mcd$MCD.close, sd 
tail (bb, n=5) 
= 
2) 
chartSeries (mcd$MCD.close, subset='2022-01::2023-01') addBBands (n=20, sd=2) 
#Relative strength index 
rsi<-RSI (mcd$MCD.close,n=14) 
tail (rsi, n=5) 
chartSeries (mcd$MCD.close, subset='2022-01::2023-01') addRSI (n=14, maType="EMA") 
#Performance Evaluation 
#VaR and CVaR 
var_mcd<-VaR (R=mcd, p=.95, method= "historical") cvar mcd<-CVaR (R=mcd, p=.95, method 
#Stats 
= 
table.Stats (R=mcd$MCD.close, ci=0.95) 
#returns 
"historical") #risk is over 100% 
ret<-table.CalendarReturns (R=mcd$MCD.close) 
View (ret) 
#computing continuously compounded returns 
mcdMonthlyReturnsC = na.omit (diff (log (mcd$MCD.close))) 
head (mcdMonthlyReturnsC) 
#specific risk 
risk<-SpecificRisk (Ra-mcdMonthlyReturns C, Rb-gspcMonthlyReturnsC, scale = 12, Rf=0.04) 
risk 
#CAPM 
table.CAPM (Ra-mcdMonthlyReturnsC, Rb-gspcMonthlyReturnsC, scale = 12, Rf=0.04) 
##### charts 
plotps<-charts. PerformanceSummary (R=mcdMonthlyReturnsC, Rf=0.04, main='Performance Summary', geometric=FALSE) 
########### Buy/Sell ############ 
— 
####evaluating trading rules naive method 
#buy signal based on simple filter rule 
#buy and sell signals based on simple filter rule 
#buy signal based on RSI 
#buy signal based on EMA and sell signal based on RSI 
#buy signal based on RSI but trading size depends on price history 
###we consider the rule that does non-day tradin price <-pdfetch_YAHOO ("MCD", fields = 
"close", from 
= 
as.Date("2023-01-01"), interval = "1d") # close price 
r <- price/Lag (price) - 1 # % price change. 
delta <-0.005 #threshold 
signal <-c(0) # first date has no signal 
#Loop over all trading days (except the first) 
for (i in 2: length (price)) { 
} 
if (r[i] > delta) { 
signal [i]<- 1 
} else 
signal [i] <- 0 
# Each data is not attached with time 
as.Date("2018-01-01"), to = 
head (signal, n=3) 
# Assign time to action variable using reclass; signal<-reclass (signal, price) 
# Each point is now attached with time tail (signal, n=3) 
# Charting with Trading rule chartSeries (mcd$MCD.close, 
type 
= 
'line', 
subset="2018-01::2023-01-01", theme-chartTheme('white')) 
addTA (signal,type='S',col='red') 
#We consider trading based on yesterday indicator: trade <- Lag (signal,1) # trade based on yesterday signal 
### Evaluation ### 
#buy at open 
#sell at close 
#trading size: all in 
ret<-((mcd$MCD.close-mcd$MCD.open)/mcd$MCD.open) *trade 
names (ret) <-"filter" 
#Performance Summary 
charts. PerformanceSummary (ret, main="Naive Buy Rule") 
#simple filter buy-sell 
for (i in 2: length (mcd$MCD.close)) { 
} 
if (r[i] > delta) { 
signal [i] <- 1 
} else if (r[i]< -delta) { 
signal [i] <- -1 
} else 
signal [i] <- 0 
signal<-reclass (signal, mcd$MCD.close) 
tradel <- Lag (signal) 
retl<-dailyReturn (mcd$MCD.close) *tradel 
names (ret1) <- 'Naive' 
charts. PerformanceSummary (ret1) 
#using RSI 
#ay-trading strategy based on 14-day RSI #buy one unit if RSI <30 and 
#otherwise no trade. 
day <-14 
rsi < RSI (price, day) 
signal [1: day+1] <- 0 
#initialize vector 
#rsi is the lag of RSI 
#0 because no signal until day+1 
for (i in (day+1): length (price)) { 
if (rsi[i] < 30) { 
signal [i] <- 1 
}else { 
#buy if rsi < 30 
#no trade all if rsi > 30 
signal [i] <- 0 
} 
signal<-reclass (signal, price) 
trade2 <- Lag (signal) 
#construct a new variable ret1 
retl <- dailyReturn (mcd$MCD.close) *tradel 
names (ret1) <- 'Naive' 
# construct a new variable ret2 
ret2 <- dailyReturn (mcd$MCD.close) *trade2 names (ret2) <- 'RSI' 
#comparing strategies with the filter rule retall <- cbind(ret1, ret2) 
charts. PerformanceSummary (retall, 
main="Naive v.s. RSI") 
#buy signal based on exponential moving average rule #sell signal based on RSI rule 
#tie-breaking: buy-signal has priority 
#we use 14-day RSI and use 250 as threshold for selling 
n<-14 
delta<-0.005 
price<-pdfetch_YAHOO ("MCD", fields = "close", 
"close", from = as.Date("2022-01-01"), to as.Date("2023-01-01"), interval = "1d") 
r<-price/Lag (price)-1 
signal<-c() #first signal is NA 
signal [1:n]<-0 
#Generate trading signal 
for (i in (n+1): length (price)) { 
} 
if (r[i]>delta) { 
signal [i] <-1 
} else if (rsi[i]>250) { 
signal [i]<--1 
} else 
signal [i]<-0 
signal<-reclass (signal, price) 
##apply trading rule trade3<-Lag (signal) 
ret3<-dailyReturn (mcd) *trade3 names (ret3) <-'Combine' 
retall<-cbind (retl, ret2, ret3) 
charts. PerformanceSummary( 
retall, main="Naive v.s. RSI v.s. Combine", 
colorset=(c("blue","green","tomato")) ) 
########### importing financials of McDonalds ############# 
= 
mcd_bs<-read_excel ("C:/Users/dhupa/OneDrive/Documents/McDonalds Financials.xlsx", sheet='Balance Sheet') 
View (mcd bs) 
mcd income<-read_excel ("C:/Users/dhupa/OneDrive/Documents/McDonalds Financials.xlsx", 
sheet='Income Statement') 
View (mcd income) 
mcd_cfs<-read_excel ("C:/Users/dhupa/OneDrive/Documents/McDonalds Financials.xlsx", sheet 'Cash Flow Statement') 
View (mcd cfs) 
########### fundamental analysis ###: #### 
###profitability 
#1.ROA, 2.CFROA, 3.Change on return on assets, 4.quality of earnings #ROA: 
ta_mcd_2022<-as.numeric(mcd_bs [12,2])*1000000 
ta mcd 2022 
ta mcd 2021<- as.numeric (mcd bs [12,3])*1000000 
ta mcd 2021 
avg_assets_mcd<-(ta_mcd_2022+ta mcd 2021)/2 avg_assets mcd 
netinc mcd<- as.numeric (mcd income [20,2])*1000000 
netinc_mcd 
roa_mcd<-netinc_mcd/avg_assets_mcd 
roa mcd 
ROA<-1 #since ROA is positive 
#CFROA 
cf_op_mcd<- as.numeric (mcd_cfs [3,2])*1000000 
cf_op_mcd 
cfroa_mcd<-cf_op_mcd/avg_assets_mcd 
cfroa mcd 
CFROA<-1 #since CFROA is positive 
#Change on return on assets 
#Compare this year's return on assets (1) to last year's return on assets. ta_mcd_2020<-as.numeric(mcd_bs [12,4])*1000000 
ta mcd 2020 
avg_assets_mcd1<-(ta_mcd_2020+ta_mcd_2021)/2 
avg_assets mcdl 
netinc mcdl<- as.numeric (mcd income [20,3])*1000000 netinc mcd1 
roa_mcd1<-netinc_mcd1/avg_assets_mcd1 
roa_mcd1 
roa mcd>roa mcd1 
CIROA<-0 #since roa_mcd>roa_mcd1 
#quality of earnings 
#Score 1 if CFROA>ROA, 0 if CFROA<ROA 
roa mcd>cfroa mcd 
QOE<-1 #since CFROA>ROA 
###leverage: 
#5.change in gearing or leverage, 6.change in working capital, 7.change in shares in issue 
#change in gearing or leverage 
equity<-as.numeric (mcd_bs [39,2])*10000000 
equity 
td<-ta_mcd_2022/equity 
td 
equityl<- as.numeric (mcd bs [39,3])*10000000 
equityl 
tdl<-ta mcd 2021/equityl 
td1 
td<td1 
leverage<-1 #since td<tdl 
#change in working capital 
ca<-as.numeric(mcd_bs [2,2])*10000000 
ca 
cal<-as.numeric (mcd bs [2,3])*10000000 
cal 
cl<-as.numeric(mcd bs [22,2])*10000000 
cl 
cl1<- as.numeric (mcd bs [22,3])*10000000 
cl1 
wc mcd<-ca/cl 
wc mcd 
wc mcdl<-cal/cl1 
wc mcd1 
wc_mcd>wc_mcd1 
CIWC<-0 #Score 1 if this year's current ratio is higher, 0 if it's lower 
#Change in shares in issue 
shareissue<-as.numeric(mcd bs [47,2]) 
shareissue 
shareissuel<- as.numeric(mcd bs [47,3]) shareissuel 
shareissuel>shareissue 
CISII<-1 #Score 1 if there is the same number of shares in issue this year, or fewer. Score 0 if there are more shares in issue. 
#operating efficiency 
#8.change in gross margin, 9.change in asset turnover, #Change in gross margin 
gp<- as.numeric (mcd_income [6,2])*10000000 
gp 
gpl<-as.numeric (mcd_income [6,3])*10000000 
gp1 
sales<-as.numeric (mcd_income [2,2])*10000000 
sales 
salesl<-as.numeric(mcd_income [2,3])*10000000 
sales1 
gm<-gp/sales 
gm 
gm1<-gpl/sales1 
gm1 
gm>gm1 
CIGM<-1 #Score 1 if this year's gross margin is higher, 0 if it's lower 
#Change in asset turnover 
at<-sales/ta mcd 2021 
at 
at1<-sales1/ta_mcd_2020 
atl 
at>atl 
CIAT<-0 #Score 1 if this year's asset turnover ratio is higher, 0 if it's lower 
### Piotroski F Score #Piotroski or F-Score #Good or high score #Bad or low score 
= 
= 
= 
1 + 2 + 3 + 4 + 5 + 6 + 7+ 8+ 9 
8 or 9 
0 or 1 
fscore<-ROA+CFROA+CIROA+QOE+leverage+CIWC+CISII+CIGM+CIAT 
fscore 
