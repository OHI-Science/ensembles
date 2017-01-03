#comparing RAM database used for OHI and RAM database used for ensemble

ohi_ram    <- read.csv('~/github/ohiprep/globalprep/fis/v2016/ram/ram_extended.csv')

ram <- read.csv("raw-data/RAM_bmsy_Ctousev4.csv", stringsAsFactors=FALSE)

#compare
length(unique(ram$assessid)) #370
length(unique(ohi_ram$assessid)) #237

# off the bat, the RAM data used in the ensemble paper has 370 unique stock assess ids while the one used for ohi has 237

setdiff(ram$assessid, ohi_ram$assessid)
setdiff(ohi_ram$assessid, ram$assessid)



ram_fits <- readRDS("generated-data/ram-orig-fits.rds")
#ran cmsy on ram database
load("raw-data/cmsy_rlegacy_results_table_v0.RData")
cmsy_refit <- cmsy.rlegacy.df0
cmsy_refit <- cmsy_refit %>%
  rename(stockid = stock_id, year = yr, b2bmsy = b_bmsy) %>%
  select(stockid, year, b2bmsy) %>%
  mutate(method = "CMSY")

ram_fits <- ram_fits %>% rename(stockid = stock)
ram_fits <- filter(ram_fits, !method %in% c("CMSY_old_prior", "CMSY_new_prior"))
ram_fits <- dplyr::bind_rows(ram_fits, cmsy_refit)
ram_fits <- rename(ram_fits, tsyear = year)

################################################################################################
## Look at how the ensembles performed when predicting B/Bmsy for RAM stocks after being trained on sim stocks
## Same as in paper, but looking at a stock by stock basis
################################################################################################
library(tidyr)
library(reshape2)
ram_outputs <- readRDS("generated-data/ram-ensemble-predicted.rds")%>%
  mutate(method = as.character(method))%>% #dealign with factors
  mutate(bbmsy_est = as.numeric(bbmsy_est))

ram_names <- ram_outputs%>%
  filter(method == "scientificname")

#ram_outputs$row <- 1:nrow(ram_outputs) ## added explicit row numbers
ram_data <- ram_outputs%>%
  unique()%>%
  filter(method %in% c("mPRM","CMSY","COMSIR","SSCOM","rf_ensemble","gbm_ensemble",
                       "mean_ensemble","lm_ensemble")) %>%
  group_by(stockid, bbmsy_true,method)%>%
  summarize(mean = mean(bbmsy_est,na.rm=T))%>%
  spread(method, mean)

ggplotRegression <- function (fit) {

  require(ggplot2)

  ggplot(fit$model, aes(x = fit$model[,1], y = fit$model[,2])) +
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("R2 = ",signif(summary(fit)$adj.r.squared, 3),
                       #"Int =",signif(fit$coef[[1]],2 ),
                       #" Slope =",signif(fit$coef[[2]], 2),
                       " P =",signif(summary(fit)$coef[2,4], 3)))
}

a<-ggplotRegression(lm(ram_data$bbmsy_true~ram_data$mean_ensemble))+labs(y = "Mean Ensemble", x = "RAM")
b<-ggplotRegression(lm(ram_data$bbmsy_true~ram_data$gbm_ensemble))+labs(y = "GBM Ensemble", x = "RAM")
c<-ggplotRegression(lm(ram_data$bbmsy_true~ram_data$lm_ensemble))+labs(y = "LM Ensemble", x = "RAM")
d<-ggplotRegression(lm(ram_data$bbmsy_true~ram_data$rf_ensemble))+labs(y = "Random Forest Ensemble", x = "RAM")
e<-ggplotRegression(lm(ram_data$bbmsy_true~ram_data$CMSY))+labs(y = "CMSY", x = "RAM")
f<-ggplotRegression(lm(ram_data$bbmsy_true~ram_data$COMSIR))+labs(y = "COMSIR", x = "RAM")
g<-ggplotRegression(lm(ram_data$bbmsy_true~ram_data$SSCOM))+labs(y = "SSCOM", x = "RAM")
h<-ggplotRegression(lm(ram_data$bbmsy_true~ram_data$mPRM))+labs(y = "Costello", x = "RAM")

cowplot::plot_grid(a,b,c,d,e,f,g,h)

## Now plot rank order






