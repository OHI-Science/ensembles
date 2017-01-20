# prep data for ensembles.

#needs to look like this
# prep the RAM data:
ram <- readRDS("generated-data/ram_fits.rds") %>%
  as.data.frame() %>%
  filter(!is.na(b_bmsy_true))

# No RAM subs
cmsy <- read.csv('~/github/ohiprep/globalprep/fis/v2016/data/fis_cmsy_bbmsy_noRAM.csv')%>%
          mutate(method = 'CMSY')%>%
          select(-rgn_id)
mprm <- read.csv('~/github/ohiprep/globalprep/fis/v2016/data/fis_mprm_bbmsy_noRAM.csv')%>%
  mutate(method = 'mPRM')%>%
  select(-rgn_id)
sscom <- read.csv('~/github/ohiprep/globalprep/fis/v2016/data/fis_sscom_bbmsy_noRAM.csv')%>%
  mutate(method = 'SSCOM')%>%
  select(-rgn_id)
comsir <- read.csv('~/github/ohiprep/globalprep/fis/v2016/data/fis_comsir_bbmsy_noRAM.csv')%>%
  mutate(method = 'COMSIR')%>%
  select(-rgn_id)

##stockid - scientificname - CMSY - COMSIR - mPRM - SSCOM

#let's just do 2010
df <- cmsy%>%rbind(mprm)%>%rbind(sscom)%>%rbind(comsir)%>%
      filter(year==2010)%>%
      select(-year)%>%
      group_by(stock_id,method)%>%
      mutate(row = 1:n())%>%
      spread(method,bbmsy)%>%
      select(-row)%>%
      unique()

# These models were run in 6-sim-to-ram.R. TODO: bring that code over here or merge this into 6-sim-to-ram.R copy
# for more clarity. These models were trained on sim data then run here on SAUP data.

# try to run the randomforest model
df$rf_ensemble <- exp(predict(m_rf, newdata = df))  ##THIS IS WHAT WE NEED TO RUN (make sure our data is identical in format to d_test)
df$gbm_ensemble <- exp(predict(m_gbm, newdata = df, n.trees = m_gbm$n.trees))
df$lm_ensemble <- exp(predict(m_lm, newdata = df))

out<-df%>%mutate(mean_ens = mean(c(CMSY,COMSIR,SSCOM,mPRM),na.rm=T))
