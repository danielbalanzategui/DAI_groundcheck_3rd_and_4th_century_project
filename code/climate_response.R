project.packages<-c("dplR","ggplot2","treeclim","ncdf4","terra","tidyverse","raster",
                    "lubridate","reshape2","RColorBrewer")
lapply(project.packages, require, character.only = TRUE)

tmx.loc<-nc_open("~/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/icru4_tmx_13.1925E_53.3305N_n.nc")
tmx.reg<-nc_open("~/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/icru4_tmx_8-15E_51-54N_n.nc")
tmn.loc<-nc_open("~/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/icru4_tmn_13.1925E_53.3305N_n.nc")
tmn.reg<-nc_open("~/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/icru4_tmn_8-15E_51-54N_n.nc")
pre.loc<-nc_open("~/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/icru4_pre_13.1925E_53.3305N_n.nc")
pre.reg<-nc_open("~/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/icru4_pre_8-15E_51-54N_n.nc")

vap.loc<-nc_open("~/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/icru4_vap_13.1925E_53.3305N_n.nc")
vap.reg<-nc_open("~/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/icru4_vap_8-15E_51-54N_n.nc")
spei.loc<-nc_open("~/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/ispei_04_13.1925E_53.3305N_n.nc")
spei.reg<-nc_open("~/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/ispei_04_8-15E_51-54N_n.nc")

tmx.loc.1<-ncvar_get(tmx.loc,"tmx")
tmn.loc.1<-ncvar_get(tmn.loc,"tmn")
pre.loc.1<-ncvar_get(pre.loc,"pre")
vap.loc.1<-ncvar_get(vap.loc,"vap")
spei.loc.1<-ncvar_get(spei.loc,"spei")

tmx.reg.1<-ncvar_get(tmx.reg,"tmx")
tmn.reg.1<-ncvar_get(tmn.reg,"tmn")
pre.reg.1<-ncvar_get(pre.reg,"pre")
vap.reg.1<-ncvar_get(vap.reg,"vap")
spei.reg.1<-ncvar_get(spei.reg,"spei")

dates<-seq(ymd('1901-01-01'),ymd('2020-12-01'),by='month')
dates.spei<-seq(ymd('1901-01-01'),ymd('2018-12-01'),by='month')

clim.data.1<-cbind.data.frame(year=year(dates),month=month(dates),tmx.loc.1,tmn.loc.1,pre.loc.1,vap.loc.1,
                 tmx.reg.1,tmn.reg.1,pre.reg.1,vap.reg.1)
clim.data.2<-cbind.data.frame(year=year(dates.spei),month=month(dates.spei),spei.loc.1,spei.reg.1)
clim.data.3<-left_join(clim.data.1,clim.data.2)
clim.data.3<-round(clim.data.3,2)
clim.data.3<-filter(clim.data.3, year >= 1901 & year <= 2015)


d18O_crn<-read.crn("~/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/d180.txt")
d18O_crn$year<-rownames(d18O_crn)
d18O_crn.mod<-filter(d18O_crn, year >= 1901 & year <= 2015)
d18O_crn.mod<-d18O_crn.mod[,c(1),drop=FALSE]

dcc.loc<-dcc(d18O_crn.mod,clim.data.3[,c(1:6)],
           method = "correlation",
           dynamic="static",
           selection=-1:10)

dcc.reg<-dcc(d18O_crn.mod,clim.data.3[,c(1,2,7:10)],
             method = "correlation",
             dynamic="static",
             selection=-1:10)


clim.data.spei<-clim.data.3[-c(1:12),c(1,2,11,12)]
dcc.spei<-dcc(d18O_crn.mod,clim.data.spei,
    method = "correlation",
    selection = -1:10,
    boot = "stationary")

coef.out<-rbind(dcc.loc$coef,dcc.reg$coef,dcc.spei$coef)
coef.out$index<-1:22

ggplot()+
  geom_hline(yintercept = 0)+
  geom_point(data=coef.out,aes(x=index,y=coef,colour=varname,fill=significant))+
  coord_cartesian(ylim=c(-0.6,0.6))
  
# correlation test with SPI
# require(precintcon)
# data(monthly)
# ppt.loc<-as.data.frame(clim.data.3[,c(1,2,5)])
# ppt.loc<-as.precintcon.monthly(ppt.loc)
# spi.loc<-as.data.frame(spi(ppt.loc, period = 2))
# 
# ppt.reg<-as.data.frame(clim.data.3[,c(1,2,9)])
# ppt.reg<-as.precintcon.monthly(ppt.reg)
# spi.reg<-as.data.frame(spi(ppt.reg, period = 2))
# 
# spi.clim<-data.frame(spi.loc,spi.loc=spi.reg$spi)
# spi.clim.1<-spi.clim[-c(1:11),]
# dcc.spi<-dcc(d18O_crn.mod,spi.clim.1,
#               method = "correlation",
#               selection = -1:10,
#               boot = "stationary")
# dcc.spi


d18O.series<-read.tucson("/Users/danielbalanzategui/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/modern_isotopes_d18O_tucson_november_2021.txt")

#check dating
write.csv(d18O.series,"/Users/danielbalanzategui/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/modern_isotopes_d18O_cols_november_2021.csv",row.names=FALSE)

#redated series
d18O.series<-read.csv("/Users/danielbalanzategui/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/modern_isotopes_d18O_cols_november_2021_redated.csv",header=TRUE) 
d18O.series$year<-as.numeric(d18O.series$year)
d18O.series.melt<-reshape2::melt(d18O.series,id.vars="year")
d18O.series.melt$value<-ifelse(d18O.series.melt$value == 0,NA,d18O.series.melt$value)

d18O.series.melt<-d18O.series.melt %>%
  group_by(variable) %>%
  mutate(year=as.numeric(year)) %>%
  dplyr::filter(year >= 1901 & year <= 2015) %>%
  mutate(value.sc=scale(value)[,1])

#make stabilised chronology
d18O.series.spread<-as.data.frame(spread(d18O.series.melt[,c(1,2,4)],key="variable",value="value.sc"))
rownames(d18O.series.spread)<-d18O.series.spread$year
d18O.series.spread<-d18O.series.spread[,-1]
redated.chron<-chron.stabilized(d18O.series.spread,winLength = 51,biweight = TRUE, running.rbar = TRUE)


#run climate response
dcc.loc<-dcc(redated.chron,clim.data.3[,c(1:6)],
             method = "correlation",
             dynamic="static",
             selection=-1:10)

dcc.reg<-dcc(redated.chron,clim.data.3[,c(1,2,7:10)],
             method = "correlation",
             dynamic="static",
             selection=-1:10)


clim.data.spei<-clim.data.3[-c(1:12),c(1,2,11,12)]
dcc.spei<-dcc(redated.chron,clim.data.spei,
              method = "correlation",
              selection = -1:10,
              boot = "stationary")

coef.out<-rbind(dcc.loc$coef,dcc.reg$coef,dcc.spei$coef)
coef.out$index<-1:22

ggplot()+
  geom_hline(yintercept = 0)+
  geom_point(data=coef.out,aes(x=index,y=coef,colour=varname,fill=significant))+
  coord_cartesian(ylim=c(-0.62,0.62))

#extract climate data of interest ie regional SPEI-4 July
clim.data.3.7<-dplyr::filter(clim.data.3,month == 7)

ggplot()+
  # geom_line(data=d18O.series.melt,
  #           aes(x=year,
  #               y=value.sc,
  #               colour=variable))+
  geom_line(data=redated.chron,aes(x=1901:2015,y=adj.crn),colour="black")+
  geom_line(data=clim.data.3.7,aes(x=year,y=spei.reg.1*-1),colour="red")+
  #geom_point(data=clim.data.3.7,aes(x=year,y=spei.reg.1*-1),colour="black")+
  coord_cartesian(xlim=c(1901,2016))

