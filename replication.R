## replication project
## for econometrics
## variable construction
## group mumber: changyi kiat zhenghui
## data: 2019/04

library(rio)
library(foreign)
library(tidyverse)
library(zoo)

##### import data
##
##
#Directory
names(Sys.info())
if (("user" %in% names(Sys.info()))) {
  if (Sys.info()[names(Sys.info()) == "user"] == "apple") {
    setwd("/Users/apple/Downloads")
    
  } else if (Sys.info()[names(Sys.info()) == "user"] == "PB") {
    setwd("/Users/nizhenghui1/Desktop/Github/Short-Selling-and-Earning-Management/Data")
    
  } else {
    warning("neither device")
    
  }
} else {
  stop("Working Directory Not Changed.")
}



fama_french_ind <- read.csv("SIC_to_Fama_French_industry.csv")
pilot <- import("pilot.csv")
data <- read.csv(gzfile("full_data.csv.gz"))
list <- import("russell3000_from_Alex_Young.xls")
crsp <- read.csv(gzfile("crsp1999_2011.csv.gz"))

list[,2] <- NULL

#merge crsp to member list
crsp <- crsp %>% mutate(tic = as.character(TICKER), cusip = substr(as.character(CUSIP) , 1, 6)) %>% rename(permno = PERMNO) %>% 
  inner_join(list, by = c('permno')) 

#get list with permno and cusip
list2 <- crsp %>% select(permno, cusip) %>% distinct()

test <- crsp %>% mutate(sic = as.numeric(SICCD) ) %>% select( SICCD, permno) %>% distinct() %>% group_by(SICCD) %>% 
  summarise(n = n_distinct(permno))
test1 <- test %>% summarise(n=sum(n))
test1 <- crsp %>% select(tic, exchg, V2)
test1 <- distinct(test1) %>% group_by(exchg, V2) %>% summarise(number=n())

summary(data$sic)
summary(data$fyear)
summary(data$cusip)

#merge compustat to member list
data1 <- data %>% mutate(cusip = substr(as.character(cusip) , 1, 6), tic = as.character(tic)  ) %>%  inner_join(list2, by = c('cusip'))
test1 <- data1 %>% select(cusip) %>% distinct()
test1 <- data1 %>% group_by(sic) %>% summarise(n=n_distinct(cusip))
test2 <- test1 %>% summarise(n=sum(n))
test3 <- test1 %>% filter((sic >= 4900 & sic <= 4949) | (sic >= 6000 & sic <= 6999)) %>% summarise(n=sum(n))

#merge matched compustat to pilot list
pilot[,2] <- 1
data1 <- merge(data1, pilot, by.x ="tic" , by.y = "ticker", all.x = TRUE)
summary(data$sic)
test1 <- data1 %>% select(tic) %>% distinct()
test1 <- data1 %>% select(tic, exchg, V2)
test1 <- distinct(test1) %>% group_by(exchg, V2) %>% summarise(number=n())

#delete finance and utility industry
data1 <- data1 %>% filter( !(sic >= 4900 & sic <= 4949) & !(sic >= 6000 & sic <= 6999) ) %>% mutate(pilot = ifelse(is.na(V2)!=1, 1, 0)) 
     #data1 <- data1 %>% filter(  ifelse(fyear == 2004, !(sic >= 4900 & sic <= 4949) & !(sic >= 6000 & sic <= 6999) , fyear > 1997  )  ) %>% mutate(pilot = ifelse(is.na(V2)!=1, 1, 0))

#make sure pilot dummy is assigned to every corresponding cusip
data1 <- data1 %>% group_by(cusip) %>% mutate(pilot = ifelse(sum(pilot) > 0, 1, 0 ))
test1 <- data1 %>% filter(fyear == 2004) %>% select(cusip) %>% distinct()
test1 <- data1 %>% select(tic) %>% distinct()
summary(data1$pilot)


### Part 1
## discretionary accrual
##

#merge to FF industry
library(sqldf)
earning_mng <- sqldf("select a.*, b.*
                     from data1 as a
                     left outer join 
                     fama_french_ind as b
                     on  a.sic = b.SIC")

library(feather)
feather_file <- "earning_mng.feather"
write_feather(earning_mng, "earning_mng.feather")
earning_mng <- read_feather("./earning_mng.feather")
earning_mng_selected <- earning_mng %>% 
  select(tic, fyear,at, sale, ibc, oancf, 
         xidoc, rect, revt, oibdp, ppegt,
         prcc_f, csho, ceq, capx, xrd, dltt, 
         dlc, seq, che, dvc, dvp, sic, FF_48, pilot, cusip) %>% 
  filter(!is.na(fyear)) #some years are NA 

summary(earning_mng_selected)
length(unique(earning_mng_selected$cusip)) #2096 unique firms

#unique firm by year; appear to be some duplicates
earning_mng_selected %>% group_by(fyear) %>% summarise(n=n(), n_distinct=n_distinct(cusip)) 

earning_mng_selected %>% filter(tic=="APOL") #An example of duplicates

#Delete duplicates
earning_mng_selected <- earning_mng_selected %>% 
  arrange(fyear,cusip, sale) %>% 
  group_by(fyear,cusip) %>% 
  mutate(unique_firm=!duplicated(cusip)) %>% 
  filter(unique_firm==TRUE) %>% 
  ungroup()
earning_mng_selected %>% group_by(fyear) %>% summarise(n=n(), n_distinct=n_distinct(cusip)) 

#contruct variables for discretionary accrual
earning_mngment <- earning_mng_selected %>% mutate(tic=as.character(tic)) %>% arrange(cusip, fyear) %>% group_by(cusip) %>% 
  mutate(asset_lag=lag(at),delta_revenue=sale-lag(sale),
         ppe=ppegt,total_accrual=ibc-oancf+xidoc,delta_ar=rect-lag(rect),
         roa=oibdp/asset_lag,size=log(at),mb_ratio=prcc_f*csho/ceq,
         capex=capx/asset_lag,capex_sq=capex*capex,r_and_d=xrd/asset_lag,investment=r_and_d+capex,
         cfo=oancf/asset_lag,lev=(dltt+dlc)/(dltt+dlc+seq),cash=che/asset_lag,
         dividends=(dvc+dvp)/asset_lag,sic2=substr(sic,1,2)) %>% 
  ungroup(cusip) 
summary(earning_mngment)

####get discretionary accruals
## regression

# industry_year <- earning_mngment %>% group_by(FF_48,fyear) %>% 
#   summarise(n=n(), n_distinct=n_distinct(tic)) %>% 
#   mutate(check=n-n_distinct)
# summary(industry_year$n)

library(broom)

acrual_results <-  earning_mngment %>% 
  filter(!is.na(total_accrual)& !is.na(asset_lag)& !is.na(delta_revenue)& !is.na(ppe)) %>% 
  group_by(FF_48,fyear) %>% 
  filter(n()>9) %>% 
  ungroup(FF_48,fyear) %>% 
  nest(-FF_48,-fyear) %>% 
  mutate(fit = map(data, ~ lm( I(total_accrual/asset_lag) ~ I(1/asset_lag) + I(delta_revenue/asset_lag) + I(ppe/asset_lag) , data =.)),
         results1 = map(fit, glance),
         results2 = map(fit, tidy)) %>% 
  unnest(results1) %>% 
  unnest(results2) %>% 
  select(fyear, FF_48, r.squared, estimate, term) %>% 
  spread(key=term, value=estimate) %>% 
  rename(intercept=`(Intercept)`, beta1=`I(1/asset_lag)`, beta2=`I(delta_revenue/asset_lag)`, beta3=`I(ppe/asset_lag)`)

#merge to previous dataset
discrection_acrual <- earning_mngment %>% 
  left_join(acrual_results, by = c("fyear","FF_48")) %>% 
  mutate(normal_acrual=intercept+beta1*1/asset_lag+beta2*(delta_revenue-delta_ar)/asset_lag+beta3*ppe/asset_lag, 
         discrectionary_acrual = total_accrual/asset_lag - normal_acrual) 
summary(discrection_acrual$discrectionary_acrual); summary(discrection_acrual$roa)

discrection_acrual_selected <- discrection_acrual %>% select(fyear, FF_48, discrectionary_acrual, cusip, roa)

discrection_acrual_matched <- discrection_acrual_selected %>% 
  arrange(FF_48, fyear, cusip) %>% 
  full_join(discrection_acrual_selected, by = c("FF_48","fyear")) %>% 
  filter(cusip.x!=cusip.y) %>% group_by(cusip.x, fyear) %>% mutate(t= abs(roa.x-roa.y), dif = discrectionary_acrual.x -discrectionary_acrual.y ) %>%
  filter(!is.na(t)) %>% 
  filter(t == min(t)) %>% 
  group_by(fyear, cusip.x) %>% 
  summarise(DA_ROA_matched=mean(dif)) %>%  #deal with multiple matches
  rename(cusip=cusip.x)
summary(discrection_acrual_matched)

discrection_acrual <- discrection_acrual %>% 
  left_join(discrection_acrual_matched, by = c("cusip","fyear")) %>% 
  rename(DA=discrectionary_acrual)
summary(discrection_acrual)
length(unique(discrection_acrual$cusip)) #2107 unique firms

### Part 2
##  other variables
## balanced and unbalanced sample
##

# contruct other variables for the main table
main_sample <- discrection_acrual %>% 
  arrange(cusip, fyear) %>%
  mutate(size_lag=lag(size), mb_ratio_lag=lag(mb_ratio), roa_lag=lag(roa), lev_lag=lag(lev)) %>% 
  mutate(pre = ifelse(fyear>=2001 & fyear<=2003, 1, 0 ), during = ifelse(fyear>=2005 & fyear<=2007, 1, 0 ),
         post = ifelse(fyear>=2008 & fyear<=2010, 1, 0 )) %>% 
  filter(pre+during+post>0) %>% 
  mutate(asset = at, size = log(at), assetgr = asset/asset_lag -1, r_and_d_square = (xrd/at)^2, investment_square=(r_and_d+capex)^2,
         tic =  as.character(tic), pilot_during=pilot*during, pilot_post=pilot*post) %>% 
  mutate_at(vars(c(DA, DA_ROA_matched, pilot, pilot_during, pilot_post,
                   pre, during, post, asset, size, mb_ratio, assetgr, capex, r_and_d, 
                   roa, cfo, lev, cash, dividends, size_lag, mb_ratio_lag, roa_lag, lev_lag)), 
            funs(DescTools::Winsorize(x=., probs = c(0.01, 0.99), na.rm = T)))
summary(main_sample)
length(unique(main_sample$cusip)) #2100 unique firms
main_sample %>% group_by(fyear) %>% summarise(n=n(), n_distinct=n_distinct(cusip)) 

library(rio)
export(main_sample, "190413_Unbalanced_Sample.feather")

#Balanced Sample
balanced_sample <- main_sample %>%  
  group_by(cusip)%>% 
  filter(n() == 9) %>% 
  ungroup()
summary(balanced_sample)
length(unique(balanced_sample$cusip)) #1416 unique firms
balanced_sample %>% group_by(fyear) %>% summarise(n=n(), n_distinct=n_distinct(cusip)) 
export(balanced_sample, "190413_Balanced_Sample.feather")


### Part 3
## Statistical analysis
##
main_sample <- read_feather("./190413_Unbalanced_Sample.feather")
balanced_sample <- read_feather("./190413_Balanced_Sample.feather")


main_sample_without_NA <- main_sample %>% 
  select(FF_48, fyear, tic, cusip, DA, DA_ROA_matched, pilot, pilot_during, pilot_post,
         pre, during, post, asset, size, mb_ratio, assetgr, capex, r_and_d, 
         roa, cfo, lev, cash, dividends, size_lag, mb_ratio_lag, roa_lag, lev_lag) %>% na.omit()
length(unique(main_sample_without_NA$cusip)) #1424 unique firms

main_sample_without_NA_2 <- main_sample %>%
  select(FF_48, fyear, tic, cusip, DA, DA_ROA_matched, pilot, pre, during, post, pilot_during, pilot_post,
         asset, size, mb_ratio, assetgr, capex,
         roa, cfo, lev, cash, dividends, size_lag, mb_ratio_lag, roa_lag, lev_lag) %>% na.omit() 
length(unique(main_sample_without_NA_2$cusip)) #2020 unique firms

balanced_sample_without_NA <- balanced_sample %>% 
  select(FF_48, fyear, tic, cusip, DA, DA_ROA_matched, pilot, pre, during, post, pilot_during, pilot_post,
         asset, size, mb_ratio, assetgr, capex, r_and_d, 
         roa, cfo, lev, cash, dividends, size_lag, mb_ratio_lag, roa_lag, lev_lag) %>% na.omit()
length(unique(balanced_sample_without_NA$cusip)) #956 unique firms

balanced_sample_without_NA_2 <- balanced_sample %>%
  select(FF_48, fyear, tic, cusip, DA, DA_ROA_matched, pilot, pre, during, post, pilot_during, pilot_post,
         asset, size, mb_ratio, assetgr, capex,
         roa, cfo, lev, cash, dividends, size_lag, mb_ratio_lag, roa_lag, lev_lag) %>% na.omit()
length(unique(balanced_sample_without_NA_2$cusip)) #1326 unique firms
summary(balanced_sample_without_NA_2$pilot)

BS_pilot_rd <- balanced_sample_without_NA %>% filter(pilot==1)
BS_nonpilot_rd <- balanced_sample_without_NA %>% filter(pilot==0)
length(unique(BS_pilot_rd$cusip)) #268 pilot firms
length(unique(BS_nonpilot_rd$cusip)) #688 control firms

BS_pilot <- balanced_sample_without_NA_2 %>% filter(pilot==1)
BS_nonpilot <- balanced_sample_without_NA_2 %>% filter(pilot==0)
length(unique(BS_pilot$cusip)) #380 pilot firms
length(unique(BS_nonpilot$cusip)) #982 control firms

library(rlang)
Figure2 <- function(data, DA){
  DA=enquo(DA)
  
  data %>% group_by(during,post,pilot) %>% 
    summarise(DA_ROA_matched_mean=mean(DA_ROA_matched), DA_mean=mean(DA)) %>% 
    mutate(time=ifelse(during==1,"2-During",ifelse(post==1,"3-Post","1-Pre")), 
           treat=ifelse(pilot==1,"Pilot firms","Nonpilot firms")) %>% 
    arrange(post,during) %>% 
    ggplot()+
    geom_line(aes(x=time, y=!! DA, group=treat, linetype=treat, color=treat))+
    labs(x="",y="Discretionary Accruals", 
         title="Figure 2 Discretionary accruals for pilot vs. nonpilot firms")
}

Figure2(data=balanced_sample_without_NA, DA=DA_ROA_matched_mean)
Figure2(data=balanced_sample_without_NA, DA=DA_mean)
Figure2(data=balanced_sample_without_NA_2, DA=DA_ROA_matched_mean)
Figure2(data=balanced_sample_without_NA_2, DA=DA_mean)


# test <- balanced_sample_without_NA_2 %>% mutate(non=1-pilot, non_during=non*during, non_post=non*post)
# m1 <- felm(DA_ROA_matched ~ non_during + non_post + non + during 
#            + post | 0 | 0 |fyear + tic, data = test)


knitr::kable(summary(BS_pilot), "html") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "summary statistics balanced pilot.html")
knitr::kable(summary(BS_nonpilot), "html") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "summary statistics balanced nonpilot.html")

library(stargazer)
library(lfe)

Table3 <- function(data, match=TRUE){
  if (match==TRUE) {
    m1 <- felm(DA_ROA_matched ~ pilot_during + pilot_post + pilot + during 
               + post | 0 | 0 |fyear + tic, data = data)
    m2 <- felm(DA_ROA_matched ~ pilot_during + pilot_post + pilot + during 
               + post + size_lag + mb_ratio_lag + roa_lag + lev_lag| 0 | 0 |fyear + tic, data = data)
    m3 <- felm(DA_ROA_matched ~ pilot_during + pilot_post + pilot + 
                 size_lag + mb_ratio_lag + roa_lag + lev_lag| fyear | 0 |fyear + tic, data = data)
    # summary(m1)
    # summary(m2)
    # summary(m3)
    
    stargazer(m1, m2, m3, type="html",
              title = "Table 3 The Effect of Pilot Program on Discretionary Accruals",
              dep.var.labels.include = FALSE,
              covariate.labels=c("Pilot*During","Pilot*Post","Pilot",
                                 "During","Post","Size", "MB", "ROA", "LEV", "Intercept"
              ),          
              omit.stat = c("rsq"),
              column.labels   = c("Discretionary accruals"),
              add.lines = list(c("Year Fixed effects", c("","","Yes"))), 
              order = c("^pilot_during$", "^pilot_post$", "^pilot$",
                        "^during$","^post$","^size_lag$",
                        "^mb_ratio_lag$","^roa_lag$","^lev_lag$", "^(Intercept)$"),
              out="Regression Table3.htm")
  } else {
    m1 <- felm(DA ~ pilot_during + pilot_post + pilot + during 
               + post | 0 | 0 |fyear + tic, data = data)
    m2 <- felm(DA ~ pilot_during + pilot_post + pilot + during 
               + post + size_lag + mb_ratio_lag + roa_lag + lev_lag| 0 | 0 |fyear + tic, data = data)
    m3 <- felm(DA ~ pilot_during + pilot_post + pilot + 
                 size_lag + mb_ratio_lag + roa_lag + lev_lag| fyear | 0 |fyear + tic, data = data)
    # summary(m1)
    # summary(m2)
    # summary(m3)
    
    stargazer(m1, m2, m3, type="html",
              title = "Table 3 The Effect of Pilot Program on Discretionary Accruals",
              dep.var.labels.include = FALSE,
              covariate.labels=c("Pilot*During","Pilot*Post","Pilot",
                                 "During","Post","Size", "MB", "ROA", "LEV", "Intercept"
              ),          
              omit.stat = c("rsq"),
              column.labels   = c("Discretionary accruals"),
              add.lines = list(c("Year Fixed effects", c("","","Yes"))), 
              order = c("^pilot_during$", "^pilot_post$", "^pilot$",
                        "^during$","^post$","^size_lag$",
                        "^mb_ratio_lag$","^roa_lag$","^lev_lag$", "^(Intercept)$"),
              out="Regression Table3.htm")
  }
}

Table3(data=balanced_sample_without_NA)
Table3(data=balanced_sample_without_NA_2)
Table3(data=main_sample_without_NA)
Table3(data=main_sample_without_NA_2)
Table3(data=balanced_sample_without_NA,match=FALSE)
Table3(data=balanced_sample_without_NA_2,match=FALSE)
Table3(data=main_sample_without_NA,match=FALSE)
Table3(data=main_sample_without_NA_2,match=FALSE)



