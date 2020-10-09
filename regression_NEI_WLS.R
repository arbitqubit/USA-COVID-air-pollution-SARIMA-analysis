rm(list=ls())
set.seed(123)
`%notin%` <- Negate(`%in%`)
library(tidyverse)
library(Hmisc)
library(usmap)

########
# read data frames
########

df_change_pm25 <- read.csv("../df_change_pm25.csv")
df_change_pm25$diff <- df_change_pm25$before - df_change_pm25$after

##

df_change_no2<- read.csv("../df_change_no2.csv")
df_change_no2$diff <- df_change_no2$before - df_change_no2$after


df_pop<- read.csv('../pop_density_census2010.csv',fileEncoding="UTF-8-BOM")
df_pop$state <- state.abb[match(df_pop$state, state.name)]
df_pop <- df_pop %>% drop_na()

df_regions <- read.csv('df_regions.csv')
df_regions$state <- trimws(df_regions$ï..state, which = c("both"))
df_regions$state <- state.abb[match(df_regions$state, state.name)]

nei <- read.csv('NEI_sector_report.txt')

## curious to see the major sources of pollutants
nei %>%  filter(POLLUTANT %in% c('PM2.5')) %>%
  group_by(POLLUTANT,MAJOR_SOURCE_TYPE) %>%
  summarise(em = sum(EMISSION_TONS)) %>%
  arrange(POLLUTANT,em)

## curious to see the major sources of pollutants

## select only NO2 and PM2.5
neif <- nei %>% select(c('STATE','MAJOR_SOURCE_TYPE','EMISSION_TONS','POLLUTANT')) %>%
  filter(POLLUTANT %in% c('PM2.5','Nitrogen Oxides')) %>%
  filter(STATE %notin% c('Puerto Rico', 'Virgin Islands','Tribal Land','District Of Columbia'))



## calc tot emission by state
nei_grp_source <- neif %>% group_by(STATE, MAJOR_SOURCE_TYPE) %>% 
  summarise(tot_emission = sum(EMISSION_TONS)) %>% arrange(STATE,tot_emission) %>%
  spread(MAJOR_SOURCE_TYPE, tot_emission) %>%
  replace(is.na(.), 0)

## change state name to abbr
nei_grp_source$state <- state.abb[match(nei_grp_source$STATE, state.name)] 


nei_grp_source_perc <- neif %>% group_by(STATE, MAJOR_SOURCE_TYPE) %>%
  summarise(tot_emission = sum(EMISSION_TONS)) %>% arrange(STATE,tot_emission) %>%
  mutate(emission_perc = 100*tot_emission/sum(tot_emission)) %>%
  # filter(MAJOR_SOURCE_TYPE %in% c("Mobile Sources", 'Stationary Sources')) %>%
  select(-tot_emission)%>%
  spread(MAJOR_SOURCE_TYPE, emission_perc) %>%
  replace(is.na(.), 0)
# 

nei_grp_source_perc$state <- state.abb[match(nei_grp_source_perc$STATE, state.name)] 



## merge with change in pollutants
## obtained from the boxplots


df_all <- nei_grp_source_perc%>% 
  left_join(df_change_no2, by='state')%>% rename( no2_change = diff) %>%
  select(-c('before', 'after')) %>%
  left_join( df_change_pm25, by='state')%>% rename( pm25_change = diff) %>%
  select(-c('before', 'after', 'STATE', 'X.x','X.y'))


names(df_all) <- sub(" ", ".", names(df_all))


###############################
## ADD CONFOUNDERS: pop density,
## landmass and lat and longitude
###############################

df_all<- df_all %>% 
  # filter(state %in% unique(df_all_no2$state)) %>% 
  left_join(df_pop , by='state') %>%
  left_join(df_regions, by='state')


################################
# SCALED PREDICTORS 
################################

cols_to_scale <- c('Fire.Sources','Mobile.Sources',"Stationary.Sources")

df_all_scaled <- df_all
df_all_scaled[,cols_to_scale] <- scale(df_all[, cols_to_scale])


#############
## WLS
#############
library(car) # for qqPlot


df_no2 <- df_all_scaled %>% filter(no2_change!="")

model_no2 <- lm(no2_change ~  Fire.Sources+Mobile.Sources+
                         Stationary.Sources+
                         pop_densitypermile2 + as.factor(region), data=df_no2)
summary(model_no2)

df_no2$resids.ols.no2 <- model_no2$residuals

fit.SDfunc <- lm(abs(resids.ols.no2) ~ Fire.Sources+Mobile.Sources+
                   Stationary.Sources+
                   pop_densitypermile2 + as.factor(region), data=df_no2)

fitted.SDs <- fit.SDfunc$fitted.values #Use fitted standard deviation estimates
weights.d <- 1/fitted.SDs^2 #Weights are inverse variances


fit.WLS.no2 <- lm(no2_change ~  Fire.Sources+Mobile.Sources+
                    Stationary.Sources+
                    pop_densitypermile2 + as.factor(region), data=df_no2, weights = weights.d)
summary(fit.WLS.no2)



## pm2.5
df_pm25 <- df_all_scaled %>% filter(pm25_change!="")

model_pm25 <- lm(pm25_change ~  Fire.Sources+Mobile.Sources+
                  Stationary.Sources+
                  pop_densitypermile2 + as.factor(region), data=df_pm25)
summary(model_pm25)



df_pm25$resids.ols.pm25 <- model_pm25$residuals

fit.SDfuncpm25 <- lm(abs(resids.ols.pm25) ~ Fire.Sources+Mobile.Sources+
                   Stationary.Sources+
                   pop_densitypermile2 + as.factor(region), data=df_pm25)

fitted.SDspm25 <- fit.SDfuncpm25$fitted.values #Use fitted standard deviation estimates
weights.d.pm25 <- 1/fitted.SDspm25^2 #Weights are inverse variances


fit.WLS.pm25 <- lm(pm25_change ~  Fire.Sources+Mobile.Sources+
                    Stationary.Sources+
                    pop_densitypermile2 + as.factor(region), data=df_pm25, weights = weights.d.pm25)
summary(fit.WLS.pm25)


#######################
## export the coefficients
#######################
library(cowplot)
library(jtools)

plot_summs(fit.WLS.no2, fit.WLS.pm25,
           model.names = c("NO2 change","PM2.5 change"), 
           coefs = c('Fire.Sources','Mobile.Sources','Stationary.Sources')) + theme_cowplot()+ylab("")

export_summs(fit.WLS.no2, fit.WLS.pm25,confint = TRUE,  error_format = "[{conf.low}, {conf.high}]", digits=4,
           model.names = c("NO2 change","PM2.5 change"), to.file='docx',file.name = 'model_WLS_all_coefs.docx')


###########################
# discrepancy ratio
###########################

discrepancy <- df_all_scaled %>% select(c('state', 'no2_change','pm25_change')) %>% drop_na()
# discrepancy$change <- log(1 - min(discrepancy$no2_change) + discrepancy$no2_change) - 
#  log(1 - min(discrepancy$pm25_change) + discrepancy$pm25_change)
discrepancy$change <- discrepancy$no2_change/discrepancy$pm25_change

library(usmap)
plot_usmap(data = discrepancy, values = "change", lines = "red") +
  scale_fill_continuous(low = "white", high = "purple",name = "mismatch", label = scales::comma) +
  # scale_fill_discrete(fill = 'changecolour')+
  theme(legend.position = "right")



## plots
discrepancy$changecolour <- ifelse(discrepancy$change <= 0, "coral", "seagreen")
ggplot(discrepancy, aes(x=reorder(state, -change), y= change, fill =changecolour)) +
  geom_bar(stat='identity', alpha=0.9)+
  theme_classic()+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=12))+
  labs(x="", y=expression(paste("Change in ", NO[2]," / Change in PM 2.5"))) +
  theme(legend.position ="" )
