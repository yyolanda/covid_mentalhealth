library(covidcast)
library(dplyr)
library(data.table)
library(tidyr)
library(caret)
library(glmnet)
library(corrplot)
library(naniar)

dat=covidcast_meta()
dat$data_source %>% unique()
signals=dat %>% filter(data_source=='fb-survey') %>% .$signal %>% unique()

res=covidcast_signals("fb-survey", signal = signals,
                     start_day = "2020-03-09", end_day = "2021-04-26")
saveRDS(res,'covid_fbsurveyDat.rds')

res2=aggregate_signals(res, format = "wide")
ind_r=rowSums(!is.na(res2),na.rm = T)/ncol(res2)>0.1
res2=res2[ind_r,]
saveRDS(res2,'covid_fbsurveyDat_aggr.rds')