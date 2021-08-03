
#installing packages
install.packages("pdfetch")

#loading to environment
library(pdfetch)

# pulling require stocks 

## stock1

stock1=as.data.frame(pdfetch_YAHOO(c("BAC"),	fields	=	c("open",	"high",	"low",	"close",
                                      "adjclose",	"volume"),	from	=	as.Date("2007-01-01"),	to	=	Sys.Date(),
              interval	=	"1d"))

colnames(stock1)=c("open",	"high",	"low",	"close","adjclose",	"volume")
stock1$Stock_name= "Bank of America"
#view 
head(stock1)

## stock2
stock2=as.data.frame(pdfetch_YAHOO("JPM",	fields	=	c("open",	"high",	"low",	"close","adjclose",	"volume"),	from	=	as.Date("2007-01-01"),	to	=	Sys.Date(),interval	=	"1d"))
colnames(stock2)=c("open",	"high",	"low",	"close","adjclose",	"volume")
stock2$Stock_name= "JP Morgan"

#stock3
stock3=as.data.frame(pdfetch_YAHOO("JCP",	fields	=	c("open",	"high",	"low",	"close","adjclose",	"volume"),	from	=	as.Date("2007-01-01"),	to	=	Sys.Date(),interval	=	"1d"))
colnames(stock3)=c("open",	"high",	"low",	"close","adjclose",	"volume")
stock3$Stock_name= "JC Penny"

#stock4
stock4=as.data.frame(pdfetch_YAHOO("C",	fields	=	c("open",	"high",	"low",	"close","adjclose",	"volume"),	from	=	as.Date("2007-01-01"),	to	=	Sys.Date(),interval	=	"1d"))
colnames(stock4)=c("open",	"high",	"low",	"close","adjclose",	"volume")
stock4$Stock_name= "Citi"

##stock5

stock5=as.data.frame(pdfetch_YAHOO("DIS",	fields	=	c("open",	"high",	"low",	"close","adjclose",	"volume"),	from	=	as.Date("2007-01-01"),	to	=	Sys.Date(),interval	=	"1d"))
colnames(stock5)=c("open",	"high",	"low",	"close","adjclose",	"volume")
stock5$Stock_name= "Disney"

##stock6

stock6=as.data.frame(pdfetch_YAHOO("GM",	fields	=	c("open",	"high",	"low",	"close","adjclose",	"volume"),	from	=	as.Date("2007-01-01"),	to	=	Sys.Date(),interval	=	"1d"))
colnames(stock6)=c("open",	"high",	"low",	"close","adjclose",	"volume")
stock6$Stock_name= "General Motors"

##stock7

stock7=as.data.frame(pdfetch_YAHOO("AMZ",	fields	=	c("open",	"high",	"low",	"close","adjclose",	"volume"),	from	=	as.Date("2007-01-01"),	to	=	Sys.Date(),interval	=	"1d"))
colnames(stock7)=c("open",	"high",	"low",	"close","adjclose",	"volume")
stock7$Stock_name= "Amazon"

## stock8

stock8=as.data.frame(pdfetch_YAHOO("IBM",	fields	=	c("open",	"high",	"low",	"close","adjclose",	"volume"),	from	=	as.Date("2007-01-01"),	to	=	Sys.Date(),interval	=	"1d"))
colnames(stock8)=c("open",	"high",	"low",	"close","adjclose",	"volume")
stock8$Stock_name= "IBM"

## stock9
stock9=as.data.frame(pdfetch_YAHOO("GOOGL",	fields	=	c("open",	"high",	"low",	"close","adjclose",	"volume"),	from	=	as.Date("2007-01-01"),	to	=	Sys.Date(),interval	=	"1d"))
colnames(stock9)=c("open",	"high",	"low",	"close","adjclose",	"volume")
stock9$Stock_name= "Google"

## stock10

stock10=as.data.frame(pdfetch_YAHOO("AAPL",	fields	=	c("open",	"high",	"low",	"close","adjclose",	"volume"),	from	=	as.Date("2007-01-01"),	to	=	Sys.Date(),interval	=	"1d"))
colnames(stock10)=c("open",	"high",	"low",	"close","adjclose",	"volume")
stock10$Stock_name= "Apple"

### appending all data frame 

stocks=rbind(stock1,stock2,stock3,stock4,stock5,stock6,stock7,stock8,stock9,stock10)

## counting
count(stocks,Stock_name)

## removing NA
stocks2=na.omit(stocks)
summary(stocks2)
#coorelation matrix for all

cor(stocks2)

## corelation matrix by each group using mutiple variable

library(dplyr)
library(tidyverse)
Corr_for_all=as.data.frame(stocks2 %>%
  split(.$Stock_name) %>% 
  map(select, -c(Stock_name)) %>% 
  map(cor))

setwd("C:\\Users\\Neon\\Desktop\\working\\Project\\Corelation")
write.csv(Corr_for_all,"corelation_all.csv")

## selecting cor for two variable

Cor_require=stocks2 %>% group_by(Stock_name)  %>% summarise(Cor=cor(volume,open))

write.csv(Cor_require,"Cor_require.csv")

mx=cor(stocks2[,-7])
## slecting cor betwen .95 to -.95
mx[mx> .95| mx < -.95 ] <-"NAN"

library(corrplot)

AA=matrix(cor(stocks2$volume,stocks2$open))
corrplot(AA, method = "number")


