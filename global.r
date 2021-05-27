# Define  working directory
#setwd("/data/user/g/cgrasland/frontier/hypernews")

# Load functions
source("hypernews_functions_V4.R")

# Load hypercube(s)
dts1<-readRDS("hc_EUR_foreign_border_week.Rdata")
dts2<-readRDS("hc_EUR_foreign_mobil_week.Rdata")
dts3<-readRDS("hc_EUR_foreign_pandemic_week.Rdata")
dts<-list("border"=dts1,"mobil"=dts2, "pandemic" = dts3)



# Load location map
map<-readRDS("world_ctr_4326.Rdata")


