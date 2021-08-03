# seeting working directory

setwd("C:\\Users\\Neon\\Desktop\\working\\Session4")

#read the daily gold data
gold_data <- read_excel("Gold Historical Prices.xlsx")

View(gold_data)

gd_require=gold_data[gold_data$Date >= "2000-01-01",]

min(gd_require$Date) # check the minimum value
max(gd_require$Date) # check the maximum value

gd_require$Year = year(gd_require$Date) # it gives error stating no year function 

# to know data type
str(gd_require)

#to create another variable for year
gd_require$year=format(as.Date(gd_require$Date, format="%d/%m/%Y"),"%Y")

View(gd_require)

#frequency table 
print(count(gd_require,year))

summary(gd_require) # there is no NA in summary

#to cross calidate check if any rows has NA
gd_require[rowSums(is.na(gd_require))!=0,] 

# using deplyr function, getting mean and SD by group 
grp <- group_by(gd_require, year)
gd_summary=summarise(grp, mean_price=mean(Price), sd_price=sd(Price),row_count=n())

View(gd_summary)

#population Mean
gd_summary$pop_mean=mean(gold_data$Price)

View(gd_summary)


## Calculation of  confidence interval
attach(gd_summary)
qnorm(0.975)*sd_price/sqrt(row_count)

gd_summary$upper_bound=mean_price+qnorm(0.975)*sd_price/sqrt(row_count)

gd_summary$lower_bound=mean_price-qnorm(0.975)*sd_price/sqrt(row_count)

View(gd_summary)


### Visulaization

plot(gd_summary$year,gd_summary$mean_price)
ggplot(gd_summary,aes(year,mean_price))+geom_point()+geom_text(label=paste("$",round(mean_price,0)),check_overlap = TRUE)+labs(title="Gold Price year Wise",y="Avergae Price")

### output of data

write.csv(gd_summary,file="Gold_summary.csv",row.names = FALSE)



