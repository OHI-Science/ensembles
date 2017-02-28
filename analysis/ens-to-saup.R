
## This is a copy of the script 6-sim-to-ram.R in the analysis folder. It defines the ensemble models trained on the simulation data that was used
## in the ensemble paper. The original script takes those models and applies them to the RAM data. Here, I adapt the code to run the models
## on the SAUP data, giving us B/Bmsy estimates for each SAUP stock (at FAO level) from all 4 ensembles.

## 1.23.2017
## JAfflerbach
################################################################################################

# apply the simulation-trained ensemble model to the RAM dataset
setwd('./analysis')

set.seed(123)
library("randomForest")
library("dplyr")
library("tidyr")

source("0-ensemble-functions.R")

# prep SAUP data
# No RAM subs
cmsy <- read.csv('~/github/ohiprep/globalprep/fis/v2016/data/fis_cmsy_bbmsy_noRAM.csv')%>%
  mutate(method = 'CMSY')%>%
  select(-rgn_id)%>%
  unique()
mprm <- read.csv('~/github/ohiprep/globalprep/fis/v2016/data/fis_mprm_bbmsy_noRAM.csv')%>%
  mutate(method = 'mPRM')%>%
  select(-rgn_id)%>%
  unique()
sscom <- read.csv('~/github/ohiprep/globalprep/fis/v2016/data/fis_sscom_bbmsy_noRAM.csv')%>%
  mutate(method = 'SSCOM')%>%
  select(-rgn_id)%>%
  unique()
comsir <- read.csv('~/github/ohiprep/globalprep/fis/v2016/data/fis_comsir_bbmsy_noRAM.csv')%>%
  mutate(method = 'COMSIR')%>%
  select(-rgn_id)%>%
  unique()


##stockid - scientificname - CMSY - COMSIR - mPRM - SSCOM

#let's just do 2010
df <- cmsy%>%rbind(mprm)%>%rbind(sscom)%>%rbind(comsir)%>%
  #filter(year==2010)%>%
  #select(-year)%>%
  group_by(stock_id,method,year)%>%
  mutate(row = 1:n())%>%
  spread(method,bbmsy)%>%
  select(-row)%>%
  unique()

## Note: we are running the models without spectral frequencies (you can see them commented out in the models below, identical to the code
## in the ensemble analysis). While I don't fully understand the implications of including or removing spectral frequencies, Sean recommended
## we don't bother using them as they did not improve the models.

# bring in the simulation formatted data to build the simulation-trained models:
d_mean_sim <- readRDS("generated-data/sim-mean-dat.rds")

# Random Forest ensemble model
m_rf <- randomForest::randomForest(
  log(bbmsy_true_mean) ~ CMSY + COMSIR + mPRM + SSCOM,
    # spec_freq_0.05 + spec_freq_0.2,
  data = d_mean_sim, ntree = 1000L)

m_gbm <- gbm::gbm(
  log(bbmsy_true_mean) ~ CMSY + COMSIR + mPRM + SSCOM,
    # spec_freq_0.05 + spec_freq_0.2,
  data = d_mean_sim, distribution = "gaussian",
  n.trees = 2000L, interaction.depth = 6, shrinkage = 0.01)

m_lm <- lm(
  log(bbmsy_true_mean) ~ (CMSY + COMSIR + mPRM + SSCOM)^2,
      # spec_freq_0.05 + spec_freq_0.2)^2,
  data = d_mean_sim)

# Run each of the models on the data and add the output to the dataset
df$rf_ensemble <- exp(predict(m_rf, newdata = df))
df$gbm_ensemble <- exp(predict(m_gbm, newdata = df, n.trees = m_gbm$n.trees))
df$lm_ensemble <- exp(predict(m_lm, newdata = df))

out<-df%>%mutate(mean_ens = mean(c(CMSY,COMSIR,SSCOM,mPRM),na.rm=T))

## Now we need to save this data in ohiprep in order to rerun calculate scores
## need to join back in the rgn_ids for each stock. Filter all of the 4 COM datasets to get unique stock - rgn matching. some of the COM are missing
## stocks included in others. there's definitely a better way to do this but I'm hacking it for now.

c <- read.csv('~/github/ohiprep/globalprep/fis/v2016/data/fis_cmsy_bbmsy_noRAM.csv', stringsAsFactors = F)%>%
              select(rgn_id,stock_id)%>%
              unique()

m <- read.csv('~/github/ohiprep/globalprep/fis/v2016/data/fis_mprm_bbmsy_noRAM.csv', stringsAsFactors = F)%>%
  select(rgn_id,stock_id)%>%
  unique()

s <- read.csv('~/github/ohiprep/globalprep/fis/v2016/data/fis_sscom_bbmsy_noRAM.csv', stringsAsFactors = F)%>%
  select(rgn_id,stock_id)%>%
  unique()

cs <- read.csv('~/github/ohiprep/globalprep/fis/v2016/data/fis_comsir_bbmsy_noRAM.csv',stringsAsFactors = F)%>%
  select(rgn_id,stock_id)%>%
  unique()

stock_rgns <- rbind(c,m,s,cs)%>%unique()

#saving random forest output
df_rf <- out%>%
         select(stock_id,year,rf_ensemble)%>%
         rename(bbmsy = rf_ensemble)%>%
        left_join(stock_rgns,by = "stock_id")%>%
        select(rgn_id,stock_id,year,bbmsy)
write.csv(df_rf,file = "~/github/ohiprep/globalprep/fis/v2016/data/fis_rf_ens_bbmsy_noRAM.csv", row.names=F)

#saving gbm output
df_gbm <- out%>%
  select(stock_id,year,gbm_ensemble)%>%
  rename(bbmsy = gbm_ensemble)%>%
  left_join(stock_rgns,by = "stock_id")%>%
  select(rgn_id,stock_id,year,bbmsy)
write.csv(df_gbm,file = "~/github/ohiprep/globalprep/fis/v2016/data/fis_gbm_ens_bbmsy_noRAM.csv", row.names=F)

# saving lm output
df_lm <- out%>%
  select(stock_id,year,lm_ensemble)%>%
  rename(bbmsy = lm_ensemble)%>%
  left_join(stock_rgns,by = "stock_id")%>%
  select(rgn_id,stock_id,year,bbmsy)
write.csv(df_lm,file = "~/github/ohiprep/globalprep/fis/v2016/data/fis_lm_ens_bbmsy_noRAM.csv", row.names=F)

#saving mean ensemble output
df_mean <- out%>%
  select(stock_id,year,mean_ens)%>%
  rename(bbmsy = mean_ens)%>%
  left_join(stock_rgns,by = "stock_id")%>%
  select(rgn_id,stock_id,year,bbmsy)
write.csv(df_mean,file = "~/github/ohiprep/globalprep/fis/v2016/data/fis_mean_ens_bbmsy_noRAM.csv", row.names=F)
