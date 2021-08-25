# Define  working directory
#setwd("/data/user/g/cgrasland/frontier/hypernews")

# Load functions
source("pgm/hypernews_functions_V5.R")

# Load hypercube(s)
dts1<-readRDS("data/hc_EUR_foreign_border_week.Rdata")
dts2<-readRDS("data/hc_EUR_foreign_mobil_week.Rdata")
dts3<-readRDS("data/hc_EUR_foreign_pandemic_week.Rdata")
dts<-list("border"=dts1,"mobil"=dts2, "pande"=dts3)



# Load location map
map<-readRDS("data/world_ctr_4326.Rdata")


