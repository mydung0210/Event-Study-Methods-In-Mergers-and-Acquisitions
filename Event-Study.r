library(readxl)
library(tidyverse)
library(lubridate)

stocks <- read_excel("D:/MA-K17/Research/Project/Research.xlsx")
# investigate stock market responses to the announcement of M&As
# Descriptive Analysis
# 4 firms
# 1000 observations
min(stocks$Time) # from 2017-01-03
max(stocks$Time) # to 2020-12-31
summary(stocks)
stocks <- stocks[order(stocks[,1]),]

# Calculate Stock returns and Index return
returns <- function(x) {
  r <- c()
  for (i in 1:(length(x)-1)) {
    f <- (x[i+1] - x[i])/x[i]
    r <- c(r,f)
  }
  return(r)
}

# Actual returns table
rt_table <- data.frame(date=stocks$Time[2:1000],
                       index=returns(stocks$VNINDEX),
                       vnm=returns(stocks$VNM),
                       vic=returns(stocks$VIC),
                       gex=returns(stocks$GEX),
                       msn=returns(stocks$MSN))


# Vector of events
# Masan m&a VinMart: 3/12/2019
# Vinamilk M&A Mộc Châu Milk: 20/12/2019. Announcement: 12/3/2019
# Gelex m&a Viglacera: 19/4/2019
# Vingroup: 15/5/2020 (Dai An)
# days before and after
event_window <- function(x,y) {
  t_before <- rt_table %>% filter(date <= x) %>% tail(10) %>% select(date,y,index)
  t_after <- rt_table %>% filter(date > x) %>% head(11) %>% select(date,y,index)
  event <- rbind(t_before,t_after)
  return(event)
}

vnm=event_window("2019-12-20","vnm")
vic=event_window("2020-05-15","vic")
gex=event_window("2019-04-19","gex")
msn=event_window("2019-12-04","msn")

# Estimation period

# T = 250
estimation_window <- function(x) {
  estimation <- c()
  for (i in as.Date(x,format = "%Y-%m-%d",origin="1970-01-01")) {
    n_before <- as.Date(i,format = "%Y-%m-%d",origin="1970-01-01") - 260
    n_after <- n_before %m-% months(12)
    estimation <- as.Date(c(n_after:n_before),format = "%Y-%m-%d",origin="1970-01-01")
  }
  return(estimation)
}

# For calculate CAPM
estimate_table <- data.frame(vnm=estimation_window("2019-12-20"),
                             vic=estimation_window("2020-05-15"), 
                             gex=estimation_window("2019-04-19"),
                             msn=estimation_window("2019-12-04"))

es_vnm <- rt_table %>% as_tibble() %>% 
  filter(date >= as.Date(estimate_table$vnm[1]) & 
           date <= as.Date(estimate_table$vnm[length(estimate_table[,1])])) %>%
  select(date,index,vnm)

vnm1=estimation_window("2019-12-20")
es_vnm1 <- rt_table %>% as_tibble() %>% 
  filter(date >= as.Date(vnm1[1]) & 
           date <= as.Date(vnm1[length(estimate_table[,1])])) %>%
  select(date,index,vnm)

es_vic <- rt_table %>% as_tibble() %>% 
  filter(date >= as.Date(estimate_table$vic[1]) & 
           date <= as.Date(estimate_table$vic[length(estimate_table[,1])])) %>%
  select(date,index,vic)

es_gex <- rt_table %>% as_tibble() %>% 
  filter(date >= as.Date(estimate_table$gex[1]) & 
           date <= as.Date(estimate_table$gex[length(estimate_table[,1])])) %>%
  select(date,index,gex)

es_msn <- rt_table %>% as_tibble() %>% 
  filter(date >= as.Date(estimate_table$msn[1]) & 
           date <= as.Date(estimate_table$msn[length(estimate_table[,1])])) %>%
  select(date,index,msn)

# Calculate the alpha, beta and sigma coefficients (for each event) 

beta <- data.frame(vnm=lm(vnm ~ index,data=es_vnm)$coefficients,
           vic=lm(vic ~ index,data=es_vic)$coefficients,
           gex=lm(gex ~ index,data=es_gex)$coefficients,
           msn=lm(msn ~ index,data=es_msn)$coefficients)

# Estimated Returns: alpha + beta*returns
# Abnormal returns: (stocks returns - Estimated Returns)*100 in percent
# Null Hypothesis for AR: the event having no effect on the stock price AR = 0
# Null Hypothesis for test statistics: the mean of AR if zero
# t-stat = AR/sd(AR)
# Null
vnm <- vnm %>% mutate(estimated_returns=beta$vnm[1]+beta$vnm[2]*index, 
                            abnormal_returns=(vnm-estimated_returns)*100,
                            answer=ifelse(abnormal_returns != 0,"REJECT","NO REJECT"),
                            t_stat=abnormal_returns/sqrt(var(abnormal_returns)),
                            significant=ifelse(abs(t_stat) > 1.96,"REJECT","NO REJECT"))
vic <- vic %>% mutate(estimated_returns=beta$vic[1]+beta$vic[2]*index, 
                            abnormal_returns=(vic-estimated_returns)*100,
                            answer=ifelse(abnormal_returns != 0,"REJECT","NO REJECT"),
                            t_stat=abnormal_returns/sqrt(var(abnormal_returns)),
                            significant=ifelse(abs(t_stat) > 1.96,"REJECT","NO REJECT"))
gex <- gex %>% mutate(estimated_returns=beta$gex[1]+beta$gex[2]*index, 
                            abnormal_returns=(gex-estimated_returns)*100,
                            answer=ifelse(abnormal_returns != 0,"REJECT","NO REJECT"),
                            t_stat=abnormal_returns/sqrt(var(abnormal_returns)),
                            significant=ifelse(abs(t_stat) > 1.96,"REJECT","NO REJECT"))
msn <- msn %>% mutate(estimated_returns=beta$msn[1]+beta$msn[2]*index, 
                            abnormal_returns=(msn-estimated_returns)*100,
                            answer=ifelse(abnormal_returns != 0,"REJECT","NO REJECT"),
                            t_stat=abnormal_returns/sqrt(var(abnormal_returns)),
                            significant=ifelse(abs(t_stat) > 1.96,"REJECT","NO REJECT"))

# Computing the time series cumulative abnormal return over a multi-period event window: T1 & T2
# Null Hypothesis for AR: the event having no effect on the stock price CAR = 0
# Null Hypothesis for test statistics: the mean of AR if zero
car_vnm <- vnm %>% transmute(car=sum(abnormal_returns),
                     answer=ifelse(car != 0,"REJECT","NO REJECT"),
                     sd=sqrt(var(abnormal_returns)*length(date)),
                     test_stat=car/sd,
                     significant=ifelse(abs(test_stat) > 1.96,"REJECT","NO REJECT")) %>% head(1)

car_vic <- vic %>% transmute(car=sum(abnormal_returns),
                                answer=ifelse(car != 0,"REJECT","NO REJECT"),
                     sd=sqrt(var(abnormal_returns)*length(date)),
                     test_stat=car/sd,
                     significant=ifelse(abs(test_stat) > 1.96,"REJECT","NO REJECT")) %>% head(1)

car_msn <- msn %>% transmute(car=sum(abnormal_returns),
                               answer=ifelse(car != 0,"REJECT","NO REJECT"),
                     sd=sqrt(var(abnormal_returns)*length(date)),
                     test_stat=car/sd,
                     significant=ifelse(abs(test_stat) > 1.96,"REJECT","NO REJECT")) %>% head(1)

car_gex <- gex %>% transmute(car=sum(abnormal_returns),
                               answer=ifelse(car != 0,"REJECT","NO REJECT"),
                     sd=sqrt(var(abnormal_returns)*length(date)),
                     test_stat=car/sd,
                     significant=ifelse(abs(test_stat) > 1.96,"REJECT","NO REJECT")) %>% head(1)

car_table <- rbind(car_vnm,car_vic,car_msn,car_gex)
car_table <- cbind(company=c("vnm","vic","msn","gex"),car_table)

# Conclusion: since TS < 1.96, thus we no reject null hypothesis
# All the AR are small, approx to 0.

# For multiple firms: define this average across firms for each separate day t during the event window as
vic_da=event_window("2020-05-15","vic") # Dai An
vic_hh=event_window("2020-03-18","vic") # Huong Hai
vic_pq=event_window("2020-01-01","vic") # Phu Quoc
vic_it=event_window("2020-04-01","vic") # ITIS

es_tbl_vic <- data.frame(vic_da=estimation_window("2020-05-15"),
                     vic_hh=estimation_window("2020-03-18"), 
                     vic_pq=estimation_window("2020-01-01"),
                     vic_it=estimation_window("2020-04-01"))

es_vic_da <- rt_table %>% as_tibble() %>% 
  filter(date >= as.Date(es_tbl_vic$vic_da[1]) & 
           date <= as.Date(es_tbl_vic$vic_da[length(es_tbl_vic[,1])])) %>%
  select(date,index,vic)
es_vic_hh <- rt_table %>% as_tibble() %>% 
  filter(date >= as.Date(es_tbl_vic$vic_hh[1]) & 
           date <= as.Date(es_tbl_vic$vic_hh[length(es_tbl_vic[,1])])) %>%
  select(date,index,vic)
es_vic_pq <- rt_table %>% as_tibble() %>% 
  filter(date >= as.Date(es_tbl_vic$vic_pq[1]) & 
           date <= as.Date(es_tbl_vic$vic_pq[length(es_tbl_vic[,1])])) %>%
  select(date,index,vic)
es_vic_it <- rt_table %>% as_tibble() %>% 
  filter(date >= as.Date(es_tbl_vic$vic_it[1]) & 
           date <= as.Date(es_tbl_vic$vic_it[length(es_tbl_vic[,1])])) %>%
  select(date,index,vic)

beta_vic <- data.frame(vic_da=lm(vic ~ index,data=es_vic_da)$coefficients,
                       vic_hh=lm(vic ~ index,data=es_vic_hh)$coefficients,
                       vic_pq=lm(vic ~ index,data=es_vic_pq)$coefficients,
                       vic_it=lm(vic ~ index,data=es_vic_it)$coefficients)

vic_da <- vic_da %>% mutate(estimated_returns=beta_vic$vic_da[1]+beta_vic$vic_da[2]*index, 
                      abnormal_returns=(vic-estimated_returns)*100,
                      answer=ifelse(abnormal_returns != 0,"REJECT","NO REJECT"),
                      t_stat=abnormal_returns/sqrt(var(abnormal_returns)),
                      significant=ifelse(abs(t_stat) > 1.96,"REJECT","NO REJECT"))
vic_hh <- vic_hh %>% mutate(estimated_returns=beta_vic$vic_hh[1]+beta_vic$vic_hh[2]*index, 
                            abnormal_returns=(vic-estimated_returns)*100,
                            answer=ifelse(abnormal_returns != 0,"REJECT","NO REJECT"),
                            t_stat=abnormal_returns/sqrt(var(abnormal_returns)),
                            significant=ifelse(abs(t_stat) > 1.96,"REJECT","NO REJECT"))
vic_pq <- vic_pq %>% mutate(estimated_returns=beta_vic$vic_pq[1]+beta_vic$vic_pq[2]*index, 
                            abnormal_returns=(vic-estimated_returns)*100,
                            answer=ifelse(abnormal_returns != 0,"REJECT","NO REJECT"),
                            t_stat=abnormal_returns/sqrt(var(abnormal_returns)),
                            significant=ifelse(abs(t_stat) > 1.96,"REJECT","NO REJECT"))
vic_it <- vic_it %>% mutate(estimated_returns=beta_vic$vic_it[1]+beta_vic$vic_it[2]*index, 
                            abnormal_returns=(vic-estimated_returns)*100,
                            answer=ifelse(abnormal_returns != 0,"REJECT","NO REJECT"),
                            t_stat=abnormal_returns/sqrt(var(abnormal_returns)),
                            significant=ifelse(abs(t_stat) > 1.96,"REJECT","NO REJECT"))

firms <- data.frame(vic_da=vic_da$abnormal_returns,vic_hh=vic_hh$abnormal_returns,
                    vic_pq=vic_pq$abnormal_returns,vic_it=vic_it$abnormal_returns)

var_firms <- (var(vic_da$abnormal_returns) + var(vic_hh$abnormal_returns) + 
                var(vic_pq$abnormal_returns) + var(vic_it$abnormal_returns))/16
firms <- firms %>% mutate(ar_firms=(vic_da + vic_hh + vic_pq + vic_it)/4,
                          answer=ifelse(ar_firms != 0,"REJECT","NO REJECT"),
                 test_stat=ar_firms/sqrt(var_firms),
                 significant=ifelse(abs(test_stat) > 1.96,"REJECT","NO REJECT"))

car_firms <- sum(firms$ar_firms)
car_var_firms <- (var(vic_da$abnormal_returns)*21 + var(vic_hh$abnormal_returns)*21 + 
                    var(vic_pq$abnormal_returns)*21 + var(vic_it$abnormal_returns)*21)/16
scar_firms <- car_firms/sqrt(car_var_firms)
ifelse(abs(scar_firms) > 1.96,"REJECT","NO REJECT")



