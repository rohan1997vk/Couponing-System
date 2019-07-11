#Libraries
library(dplyr)
#data
coupon_redempt <- read.csv("coupon_redempt.csv")
hh_demographic <- read.csv("hh_demographic.csv")
transaction_data <- read.csv("transaction_data.csv")
d <- read.csv("C:/Users/rohan/OneDrive/Desktop/Date.csv")
transaction_data$DAY <- d$ï..DATE[transaction_data$DAY]
rfm <- data.frame(HOUSEHOLD_KEY = transaction_data$household_key, DATE = transaction_data$DAY, 
                   SALES = transaction_data$SALES_VALUE, BASKET_id = transaction_data
                  $BASKET_ID)
rfm1 <- data.frame(HOUSEHOLD_KEY = transaction_data$household_key, DATE = transaction_data$DAY, SALES = transaction_data$SALES_VALUE )
coupon_used <- inner_join(coupon_redempt,hh_demographic,  by = "household_key")
r <- dplyr::add_count(coupon_redempt,coupon_redempt$COUPON_UPC)
rc <- dplyr::add_count(coupon_redempt,coupon_redempt$household_key)
coupon_redempt$n <- r
new_red <- data.frame(household_key = coupon_redempt$household_key, 
                      COUPON_UPC = coupon_redempt$COUPON_UPC, 
                      times = coupon_redempt$n)
new_red <- new_red[,-c(3,4,5,6,7)]
new_red$hk_n <- rc$n
rfm1$DATE <- as.numeric(rfm1$DATE)
agg_rfm_date <- aggregate(rfm1, by = list(rfm1$DATE), FUN = sum)
agg_rfm_hk <- aggregate(rfm1, by = list(rfm1$HOUSEHOLD_KEY), FUN = sum)


rfm2 <- rfm %>%
  group_by(rfm$HOUSEHOLD_KEY, rfm$DATE) %>% 
  summarise_each(mean)

library(data.table)
r12 <- setDT(rfm)[, lapply(.SD, sum), by = .(rfm$HOUSEHOLD_KEY, rfm$DATE,rfm$BASKET_id)] 


head(ar)
ar$`rfm1$DATE` <- d$ï..DATE[ar$`rfm1$DATE`]
ar <- ar[-c(3,4)]

#RFM 
analysis_date <- lubridate::as_date('2014-01-01',tz= "UTC")
rfm_result <- rfm_table_order(rfm_data_orders, ar$`rfm1$HOUSEHOLD_KEY`, ar$`rfm1$DATE`, ar$SALES)
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(rmarkdown)

r_final <- r12 %>% 
  group_by(r12$Key) %>% 
  summarise(recency=as.numeric(as.Date("2016-01-01")-max(r12$Date)),
            frequency=n_distinct(r12$Basket_Id), monitery= sum(r12$Sales)/n_distinct(r12$Basket_Id)) 


summary(ar)

kable(head(r_final))
rfm_result <- rfm_table_order(r12, r12$Key, r12$Date,r12$Sales,r12$Date)

library(didrooRFM)
colnames(r12) <- c("customerId","date","basketid","sales")
new1 <- seq(1,276484, by = 1)
new1 <- as.data.frame(new1)
r12_new <- data.frame(new1$new1,r12$customerId,r12$date,r12$sales)
class(r12_new$r12.date)

r <- findRFM(r12_new)



