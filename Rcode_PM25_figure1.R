rm(list=ls())


`%notin%` <- Negate(`%in%`)


library(readxl)
library(tidyverse)
library(boot)
library(ggthemes)
library(gridExtra)
library(forecast)
library(data.table)
library(GGally)

dfpoll_orig <- read.csv('data_alltimepm25.csv')
dfpoll_orig$date <- as.Date(dfpoll_orig$date)

state_policy<- read.csv('state_policy_changes_1.csv')
state_policy <- state_policy %>% filter(State %notin% c('District of Columbia', 'Total with each policy (out of 51 with DC)'))

confounders_daily <- read.csv('confounders_all.csv')
confounders_daily$date <- as.Date(confounders_daily$date)

#################################################
## Select data before April 29, 2020 (inclusive)
## for all datasets
#################################################

maxdate ='2020-04-29'

#################################################
## INPUT PARAMETERS
#################################################


## train data
ldate <- as.Date("2020-01-01")
nweekspred = 16 # # of weeks to predict on
udate <- (ldate+7*nweekspred) # date to predict until

start.time <- Sys.time()
#################################################
## LOOP OVER STATES
#################################################

dfs_tosave = list()
p = list()
i=1

for (state_fullname in unique(state_policy$State)){
  if (state_fullname=='Alaska'){next}
  
  #################################################
  ## DATA WRANGLING
  #################################################
  
  # get abbreviated name
  state_name = state.abb[which(state.name == state_fullname)]
  
  if (state_name %notin% unique(dfpoll_orig$state)) {next}
  
  # get date of state of emergency
  soe= as.Date(state_policy$State.of.emergency[state_policy$State == state_fullname], format= '%m/%d/%Y')
  
  
  dfpoll <- dfpoll_orig %>% filter (state==state_name) %>% group_by(date) %>% summarise(pm25 = mean(pm25))
  
  ## fill missing dates in poll data
  dfpoll<-dfpoll %>%
    complete(date = seq.Date(min(date), max(date), by="day")) %>%
    fill('pm25') %>% filter( date < as.Date(maxdate))
  
  cat("State = ", state_fullname,"  ")
  
  if (nrow(dfpoll)<1940) {print("next ")
    next}

  
  conf_state <- confounders_daily %>% filter(stateabbr == state_name)%>% filter( date < as.Date(maxdate)) %>%
    complete(date = seq.Date(min(date), max(date), by="day")) %>%
    fill('tmmx','pr','rmax')
  

  
  n=7 ## average every seven rows
  m = (nrow(dfpoll)%/%n)*n
  
  
  ## take avg every n days. 
  
  dfweek <- setDT(dfpoll[1:m,])[,.(pm25=mean(pm25)), date-0:(n-1)]
  dfweek$idx <- seq(1, nrow(dfweek))
  
  temp_week <- setDT(conf_state[1:m,])[,.(temp = mean(tmmx)), date-0:(n-1)]
  ppt_week <- setDT(conf_state[1:m,])[,.(ppt = mean(pr)), date-0:(n-1)]
  hum_week <- setDT(conf_state[1:m,])[,.(hum = mean(rmax)), date-0:(n-1)]
  
  xregs <- cbind(temp_week, ppt_week$ppt, hum_week$hum)
  colnames(xregs) <- c('date','temp','ppt','hum')
  
  train = dfweek %>% filter(date<ldate) # ldate not included
  train$idx <- seq(1, nrow(train))
  
  xregs_train <- xregs %>% filter(date<ldate) # ldate not included
  xregs_train <- xregs_train[c('temp','ppt','hum')]
  
  xregs_train <- as.matrix(xregs_train)
  
  ## test data from poll
  test = dfweek %>%  filter(date>=ldate & date <udate)## include ldate and filter(date>=ldate & date <udate)
  
  
  
  ## test data for confounders
  xregs_test <- xregs %>%  filter(date>=ldate & date <udate)
  xregs_test <- xregs_test[c('temp','ppt','hum')]
  xregs_test <- as.matrix(xregs_test)
  
  
  
  
  ts=ts(train$pm25)
  
  num_resamples=1000 # number of bootstraps
  
  sim <- bld.mbb.bootstrap(ts, num_resamples)
  preds = matrix(list(), nrow=num_resamples)
  
  for (j in seq(1, length(sim))) {
    
    model = auto.arima(sim[[j]], xreg = as.matrix(xregs_train), max.p = 100, max.q = 100, max.P = 100, max.Q = 100)
    forecast = forecast(model,h = nweekspred, xreg = xregs_test,level = 0.95)
    
    preds[[j]] = forecast$mean
    
  }
  preds = as.data.frame(preds)
  sd_pred = apply(preds,1,sd)
  mean_pred = apply(preds,1,mean)
  
  mean_diff = test$pm25-mean_pred 
  lower = mean_diff-1.96*sd_pred
  upper = mean_diff + 1.96*sd_pred
  
  df_diff <- as.data.frame(cbind(mean_diff, sd_pred))
  df_diff$date <- as.Date(test$date)

  
  p[[i]] = ggplot(df_diff, aes(x=date, y=mean_diff,color='red')) + 
    geom_line(linetype = 'solid', size = 1.5) + 
    geom_vline(xintercept = soe, color='lightblue', size=1.5)+
    geom_hline(yintercept = 0)+
    geom_point(size=3)+
    theme(axis.title=element_blank())+
    scale_y_continuous(breaks=seq(-10, 10, 10)) + 
    scale_x_date("Date",breaks = c(seq(from=as.Date("2020-01-01"),
                                       to=as.Date("2020-04-30"),by="month")),
                 labels = c('J','F','M','A')) +
    geom_errorbar(data=df_diff, aes(ymin=mean_diff-1.96*sd_pred, ymax=mean_diff+1.96*sd_pred), width=1,color='black',
                  position=position_dodge(0.05), size=1) +
    ggtitle(paste(state_name))+
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14))+theme(legend.position = "none")
  
 
  dfs_tosave[[i]] = df_diff
    
  i = i+1
}


allplots <- marrangeGrob(p, nrow=2, ncol=1)

############################################
##  PLOTS
################################################

ps <- paste('p[[',1:length(p),']]', sep='', collapse=',')
library(cowplot)

#########################
for (i in seq(1,length(p))){
  p[[i]] = p[[i]] + theme(axis.title=element_blank())
}


plot <- plot_grid(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]],p[[7]],p[[8]],p[[9]],p[[10]],
           p[[11]],p[[12]],p[[13]],p[[14]],p[[15]],p[[16]],p[[17]],p[[18]],p[[19]],p[[20]],
           p[[21]],p[[22]],p[[23]],p[[24]],p[[25]],p[[26]],p[[27]],p[[28]],p[[29]],p[[30]],
           p[[31]],p[[32]],p[[33]],p[[34]],p[[35]],p[[36]],p[[37]],p[[38]],p[[39]],p[[40]],
           p[[41]],p[[42]],p[[43]],p[[44]],p[[45]],p[[46]],p[[47]],p[[48]])
library(grid)
y.grob <- textGrob(expression(paste("Difference between actual and predicted PM2.5 concentrations ( ",mu, "g/",m^3,")")), 
                   gp=gpar(fontface="bold", fontsize=15), rot=90)
grid.arrange(arrangeGrob(plot, left = y.grob)) 



################################################
## TO MAKE BOXPLOTS
################################################

df_box <- dfs_tosave[[1]]
df_box$state <- p[[1]]$labels$title
df_box$period <- ifelse(df_box$date<as.Date(p[[1]]$layers[[2]]$data$xintercept), 'before','after')
df_box$period <- factor(df_box$period, levels = c("before",'after'))

for (i in 2:length(p)){
  dffill <- dfs_tosave[[i]]
  dffill$state <- p[[i]]$labels$title
  dffill$period <- ifelse(dffill$date<as.Date(p[[i]]$layers[[2]]$data$xintercept), 'before','after')
  df_box <- rbind(df_box, dffill)
  }


##########################################
##########################################
## CALCULATE MEAN AND MEDIAN CHANGE FOR EACH STATE
##########################################
##########################################

df_change <- df_box %>% group_by(state, period) %>% 
            summarise(median_diff = median(mean_diff)) %>%
            spread(period, median_diff)

df_meanchange_pm25 <- df_box %>% group_by(state, period) %>% 
     summarise(mean_diff = mean(mean_diff)) %>%
     spread(period, mean_diff)

df_meanchange_pm25$meandiffbefore_after <- df_meanchange_pm25$before - df_meanchange_pm25$after
