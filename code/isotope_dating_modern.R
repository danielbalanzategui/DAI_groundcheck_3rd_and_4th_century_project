chronology.quality.packages<-c("dplR","tidyverse")
lapply(chronology.quality.packages, require, character.only = TRUE)

#chronology quality
d18O.redated<-read.csv("/Users/danielbalanzategui/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/modern_isotopes_d18O_cols_november_2021_redated.csv",header=TRUE)
d18O.redated.no.na<-d18O.redated
for(i in 1:ncol(d18O.redated.no.na)){
  d18O.redated.no.na[is.na(d18O.redated.no.na[,i]), i] <- mean(d18O.redated.no.na[,i], na.rm = TRUE)
}

rownames(d18O.redated.no.na)<-d18O.redated.no.na$year
d18O.redated.no.na.RWI<-d18O.redated.no.na[,-1]

d18O.redated.no.na.dt<-detrend(d18O.redated.no.na.RWI,method="Mean")
d18O.redated.no.na.dt$g1e3b<-ifelse(d18O.redated.no.na.dt$g1e3b == 1.0000000, NA,d18O.redated.no.na.dt$g1e3b)
d18O.redated.no.na.dt$g2e3b<-ifelse(d18O.redated.no.na.dt$g2e3b == 1.0000000, NA,d18O.redated.no.na.dt$g2e3b)
d18O.redated.no.na.dt$g2e6d<-ifelse(d18O.redated.no.na.dt$g2e6d == 1.0000000, NA,d18O.redated.no.na.dt$g2e6d)
d18O.redated.no.na.dt$g3e1c<-ifelse(d18O.redated.no.na.dt$g3e1c == 1.0000000, NA,d18O.redated.no.na.dt$g3e1c)
d18O.redated.no.na.dt$gms03b<-ifelse(d18O.redated.no.na.dt$gms03b == 1.0000000, NA,d18O.redated.no.na.dt$gms03b)
d18O.redated.no.na.dt$gms06b<-ifelse(d18O.redated.no.na.dt$gms06b == 1.0000000, NA,d18O.redated.no.na.dt$gms06b)

d18O.redated.no.na.dt.IDs<-read.ids(d18O.redated.no.na.dt, stc = c(1, 1, 1))
d18O.redated.no.na.dt.IDs$tree[3]<-5
d18O.redated.no.na.dt.IDs$tree[5]<-6
rwi.stats.running(d18O.redated.no.na.dt.IDs, gp.ids)
d18O.redated.no.na.dt.stats <- rwi.stats.running(d18O.redated.no.na.dt,d18O.redated.no.na.dt.IDs,
                                                 prewhiten = TRUE,
                                                 running.window = TRUE,
                                                 window.length = 31,
                                                 window.overlap = 11,
                                                 min.corr.overlap = 1,
                                                 zero.is.missing = TRUE,
                                                 method = "spearman")
d18O.redated.no.na.dt.stats
d18O.redated.no.na.dt.sss <- sss(d18O.redated.no.na.dt,d18O.redated.no.na.dt.IDs)

# write.tucson(d18O.redated.no.na.dt,"/Users/danielbalanzategui/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/d18O_for_EPS_RBAR.txt",prec = 0.001)
# write.tucson(d18O.redated.no.na.dt,"/Volumes/THE_UNNAMED/dai/october_2022/d18O_for_EPS_RBAR.txt",prec = 0.001)

d18O.redated.no.na.dt<-data.frame(year=d18O.redated$year,
                                  detrend(d18O.redated.no.na.RWI,
                                          method="Mean"))

clim.data.3.july<-dplyr::filter(clim.data.3,month==7)

d18O.redated.no.na.dt.melt<-reshape2::melt(d18O.redated.no.na.dt,id.vars="year")
a<-ggplot(d18O.redated.no.na.dt.melt)+
  geom_line(aes(x=as.numeric(year),y=value,colour=variable))+
  #geom_line(data=clim.data.3.july,aes(x=as.numeric(year),y=scale(spei.reg.1)/10*-1+1),colour="black")+
  coord_cartesian(xlim=c(1901,2015))

b<-ggplot(d18O.redated.no.na.dt.melt)+
  geom_line(aes(x=year,y=value,colour=variable))+
  #geom_line(data=clim.data.3.july,aes(x=as.numeric(year),y=scale(spei.reg.1)/20*-1+1),colour="black")+
  coord_cartesian(xlim=c(1901,2015))

a/b
read.rwl("/Users/danielbalanzategui/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/historic_isotopes_d18O_tucson.txt")

d18O_redated.old<-read.rwl("/Volumes/Elements/usb_backups/giraffe/dai/d18O_redated.txt")  
d18O_redated.old$year<-as.numeric(rownames(d18O_redated.old))
d18O_redated.old.melt<-reshape2::melt(d18O_redated.old,id.vars="year")
d18O_redated.old.melt$value<-ifelse(d18O_redated.old.melt$value<1,NA,d18O_redated.old.melt$value)
d18O_redated.old.melt<-dplyr::filter(d18O_redated.old.melt,year >= 1901 & year <= 2015)

d18O_redated.old.spread<-spread(d18O_redated.old.melt,key=variable,value=value)

rownames(d18O_redated.old.spread)<-d18O_redated.old.spread$year
d18O_redated.old.spread.1<-d18O_redated.old.spread[,-1]

d18O.redated.old.dt<-data.frame(year=d18O_redated.old.spread$year,detrend(d18O_redated.old.spread.1,method="Mean"))
colnames(d18O_redated.old.spread.1)<-c("g1e3bo","g2e3bo","g2e6do","g3e1co","gms03bo","gms06bo")
left_join(d18O_redated.old.spread,d18O.redated)

write.tucson(d18O.redated.dt,"/Users/danielbalanzategui/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/modern_isotopes_d18O_cols_november_2022_redated_tucson.txt")
write.tucson(d18O.redated.dt,"/Volumes/THE_UNNAMED/dai/october_2022/modern_isotopes_d18O_cols_november_2022_redated_tucson.txt")                            

d18O_redated.old.melt<-reshape2::melt(d18O_redated.old,id.vars="year")
ggplot(d18O_redated.old.melt)+
  geom_line(aes(x=year,y=value,colour=variable))


#################
d18O.hist<-read.tucson("/Users/danielbalanzategui/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/historic_isotopes_d18O_tucson.txt")
d18O.hist.OG<-data.frame(year=as.numeric(200:350),d18O.hist)
d18O.hist.OG$X1w<-ifelse(d18O.hist.OG$X1w <1,NA,d18O.hist.OG$X1w)
d18O.hist.OG$X2b<-ifelse(d18O.hist.OG$X2b <1,NA,d18O.hist.OG$X2b)
d18O.hist.OG$X2b2<-ifelse(d18O.hist.OG$X2b2 <1,NA,d18O.hist.OG$X2b2)
d18O.hist.OG$X8b<-ifelse(d18O.hist.OG$X8b <1,NA,d18O.hist.OG$X8b)
d18O.hist.OG$X9b<-ifelse(d18O.hist.OG$X9b <1,NA,d18O.hist.OG$X9b)

d18O.hist.gdrive.august<-read.csv("/Users/danielbalanzategui/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/historic_TRSI_IRMS_databytree_august_2021.csv",header=TRUE)
d18O.hist.gdrive.august<-d18O.hist.gdrive.august[,-c(1,4)]
d18O.hist.gdrive.august.spread<-spread(d18O.hist.gdrive.august,key=ID,value=d18O.norm.)
colnames(d18O.hist.gdrive.august.spread)<-c("year","iX2b","iX2b2","iX8b","iX9b")

historic_TRSI_IRMS_1<-read.csv("/Users/danielbalanzategui/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/historic_TRSI_IRMS_1.csv",header=TRUE)
historic_TRSI_IRMS_1<-historic_TRSI_IRMS_1[,c(1,9)]
colnames(historic_TRSI_IRMS_1)<-c("ID","d.18O.normalized")
historic_TRSI_IRMS_1<-historic_TRSI_IRMS_1 %>% 
  filter(!grepl("IAEA CH_6|MeCel|Lactose", ID)) %>%
  separate(ID, c("ID", "year"), "_")
historic_TRSI_IRMS_1<-historic_TRSI_IRMS_1[c(1:77),]
historic_TRSI_IRMS_1.spread<-spread(historic_TRSI_IRMS_1,key=ID,value=d.18O.normalized)
historic_TRSI_IRMS_1.spread$year<-as.numeric(historic_TRSI_IRMS_1.spread$year)

d18O.hist.OG.X2b<-data.frame(year=d18O.hist.OG$year,X2b=d18O.hist.OG$X2b)
d18O.hist.gdrive.august.spread.X2b<-data.frame(year=d18O.hist.gdrive.august.spread$year,iX2b=d18O.hist.gdrive.august.spread$iX2b)
X2b<-left_join(d18O.hist.OG.X2b,d18O.hist.gdrive.august.spread.X2b)
X2b<-X2b %>% 
  mutate(nX2b = coalesce(X2b,iX2b))

d18O.hist.OG.X2b2<-data.frame(year=d18O.hist.OG$year,X2b2=d18O.hist.OG$X2b2)
d18O.hist.gdrive.august.spread.X2b2<-data.frame(year=d18O.hist.gdrive.august.spread$year,iX2b2=d18O.hist.gdrive.august.spread$iX2b2)
X2b2<-left_join(d18O.hist.OG.X2b2,d18O.hist.gdrive.august.spread.X2b2)
X2b2<-X2b2 %>% 
  mutate(nX2b2 = coalesce(X2b2,iX2b2))

d18O.hist.OG.X8b<-data.frame(year=d18O.hist.OG$year,X8b=d18O.hist.OG$X8b)
d18O.hist.gdrive.august.spread.X8b<-data.frame(year=d18O.hist.gdrive.august.spread$year,iX8b=d18O.hist.gdrive.august.spread$iX8b)
X8b<-left_join(d18O.hist.OG.X8b,d18O.hist.gdrive.august.spread.X8b)
X8b<-X8b %>% 
  mutate(nX8b = coalesce(X8b,iX8b))

d18O.hist.OG.X9b<-data.frame(year=d18O.hist.OG$year,X9b=d18O.hist.OG$X9b)
d18O.hist.gdrive.august.spread.X9b<-data.frame(year=d18O.hist.gdrive.august.spread$year,iX9b=d18O.hist.gdrive.august.spread$iX9b)
X9b<-left_join(d18O.hist.OG.X9b,d18O.hist.gdrive.august.spread.X9b)
X9b<-X9b %>% 
  mutate(nX9b = coalesce(X9b,iX9b))

d18O.hist.all.to.check.dates<-data.frame(year=d18O.hist.OG$year,
                          X1w=d18O.hist.OG$X1w,
                          X2b=X2b$nX2b,
                          X2b2=X2b2$nX2b2,
                          X8b=X8b$nX8b,
                          X9b=X9b$nX9b)

write.csv(d18O.hist.all.to.check.dates,"/Users/danielbalanzategui/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/dump/d18O.hist.all.to.check.dates.csv",row.names=FALSE)

d18O.hist.all.sc<-data.frame(year=d18O.hist.OG$year,scale(d18O.hist.all[,c(2:6)])+10)
rownames(d18O.hist.all.sc)<-d18O.hist.all.sc$year
d18O.hist.all.sc<-d18O.hist.all.sc[,-1]
write.tucson(d18O.hist.all.sc,"/Volumes/THE_UNNAMED/dai/october_2022/hist_for_redating.txt")


a<-ggplot()+
  geom_line(data=d18O.hist.OG,aes(x=year,y=scale(X1w)),colour="orange")+
  geom_line(data=X2b,aes(x=year,y=scale(nX2b)),colour="red")+
  geom_line(data=X2b2,aes(x=year,y=scale(nX2b2)),colour="blue")+
  geom_line(data=X8b,aes(x=year,y=scale(nX8b)),colour="seagreen",size=1)+
  geom_line(data=X9b,aes(x=year,y=scale(nX9b)),colour="brown")+
  geom_point(data=d18O.hist.OG,aes(x=year,y=scale(X1w)),colour="orange")+
  geom_point(data=X2b,aes(x=year,y=scale(nX2b)),colour="red")+
  geom_point(data=X2b2,aes(x=year,y=scale(nX2b2)),colour="blue")+
  geom_point(data=X8b,aes(x=year,y=scale(nX8b)),colour="seagreen")+
  geom_point(data=X9b,aes(x=year,y=scale(nX9b)),colour="brown")+
  geom_text(data=X8b,aes(x=year,y=scale(nX8b)+0.4,label=year),colour="seagreen")+
  coord_cartesian(xlim=c(200,350))+
  geom_vline(xintercept = 220)
a
#green
X8b.to_correct<-X8b$nX8b
X8b.to_corrected.dates<-data.frame(year=200:350,
                                   nX8b.corrected=c(X8b.to_correct[1:5],
                                                    X8b.to_correct[8],
                                                    X8b.to_correct[6],
                                                    X8b.to_correct[7],
                                                    X8b.to_correct[9:23],
                                                    X8b.to_correct[25],
                                                    X8b.to_correct[24],
                                                    X8b.to_correct[26:36],
                                                    X8b.to_correct[38],
                                                    X8b.to_correct[37],
                                                    X8b.to_correct[39:65],
                                                    X8b.to_correct[67],
                                                    X8b.to_correct[66],
                                                    X8b.to_correct[68:72],
                                                    X8b.to_correct[74:82],
                                                    NA,
                                                    X8b.to_correct[83:92],
                                                    NA,
                                                    X8b.to_correct[93:110],
                                                    rev(X8b.to_correct[112:length(X8b.to_correct)])))

#brown
X9b.to_correct<-X9b$nX9b
X9b.to_corrected.dates<-data.frame(year=200:350,
                                   nX9b.corrected=c(X9b.to_correct[1:7],
                                                    X9b.to_correct[9],
                                                    X9b.to_correct[8],
                                                    X9b.to_correct[10:38],
                                                    X9b.to_correct[40],
                                                    X9b.to_correct[39],
                                                    X9b.to_correct[41:43],
                                                    X9b.to_correct[45],
                                                    X9b.to_correct[44],
                                                    X9b.to_correct[46:59],
                                                    X9b.to_correct[61],
                                                    X9b.to_correct[60],
                                                    X9b.to_correct[62:length(X9b.to_correct)]))


#red
X2b.to_correct<-X2b$nX2b
X2b.to_corrected.dates<-data.frame(year=200:350,
                                   nX2b.corrected=c(X2b.to_correct[1:9],
                                                    X2b.to_correct[11],
                                                    X2b.to_correct[10],
                                                    X2b.to_correct[12:18],
                                                    X2b.to_correct[20],
                                                    X2b.to_correct[21],
                                                    X2b.to_correct[19],
                                                    X2b.to_correct[22:length(X2b.to_correct)]))

#yellow
X1w.to_correct<-d18O.hist.OG$X1w
X1w.to_corrected.dates<-data.frame(year=200:350,
                                   nX1w.corrected=c(X1w.to_correct[1:38],
                                                    X1w.to_correct[40],
                                                    X1w.to_correct[39],
                                                    X1w.to_correct[41:56],
                                                    X1w.to_correct[58],
                                                    X1w.to_correct[57],
                                                    X1w.to_correct[59:length(X1w.to_correct)]))

#blue
X2b2.to_correct<-X2b2$nX2b2
X2b2.to_corrected.dates<-data.frame(year=200:350,
                                    nX2b2.corrected=c(X2b2.to_correct[1:50],
                                                      X2b2.to_correct[52],
                                                      X2b2.to_correct[51],
                                                      X2b2.to_correct[53:length(X2b2.to_correct)]))
                                                    

b<-ggplot()+
  geom_line(data=X1w.to_corrected.dates,aes(x=year,y=scale(nX1w.corrected)),colour="orange")+
  geom_line(data=X2b.to_corrected.dates,aes(x=year,y=scale(nX2b.corrected)),colour="red")+
  geom_line(data=X2b2.to_corrected.dates,aes(x=year,y=scale(nX2b2.corrected)),colour="blue")+
  geom_line(data=X8b.to_corrected.dates,aes(x=year,y=scale(nX8b.corrected)),colour="seagreen")+
  geom_line(data=X9b.to_corrected.dates,aes(x=year,y=scale(nX9b.corrected)),colour="brown")+
  geom_point(data=X1w.to_corrected.dates,aes(x=year,y=scale(nX1w.corrected)),colour="orange")+
  geom_point(data=X2b.to_corrected.dates,aes(x=year,y=scale(nX2b.corrected)),colour="red")+
  geom_point(data=X2b2.to_corrected.dates,aes(x=year,y=scale(nX2b2.corrected)),colour="blue")+
  geom_point(data=X8b.to_corrected.dates,aes(x=year,y=scale(nX8b.corrected)),colour="seagreen")+
  geom_point(data=X9b.to_corrected.dates,aes(x=year,y=scale(nX9b.corrected)),colour="brown")+
  geom_text(data=X9b.to_corrected.dates,aes(x=year,y=scale(nX9b.corrected),label=as.character(1:151)),colour="brown")+
  coord_cartesian(xlim=c(200,350))+
  geom_vline(xintercept = c(260))

a/b

  #write.csv("/Users/danielbalanzategui/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/d180_historic_original_november_2022",row.names = FALSE)

d<-left_join(X1w.to_corrected.dates,X2b.to_corrected.dates)
dd<-left_join(d,X2b2.to_corrected.dates)
ddd<-left_join(dd,X8b.to_corrected.dates)
dddd<-left_join(ddd,X9b.to_corrected.dates)
row.names(dddd)<-dddd$year
dddd<-dddd[,-1]
dddd.IDs<-read.ids(dddd, stc = c(1, 1, 1))
dddd.hist.dt.stats <- rwi.stats.running(dddd,
                                        prewhiten = TRUE,
                                        running.window = TRUE,
                                        window.length = 31,
                                        window.overlap = 11,
                                        min.corr.overlap = 1,
                                        zero.is.missing = TRUE,
                                        method = "spearman")
dddd.hist.dt.stats

ggplot()+
  geom_line(data=d18O.hist.OG,aes(x=year,y=X2b))+
  geom_point(data=d18O.hist.gdrive.august.spread,aes(x=year,y=iX2b))+
  geom_point(data=historic_TRSI_IRMS_1.spread,aes(x=year,y=X2b+0.1),colour="brown")

ggplot()+
  geom_line(data=d18O.hist.OG,aes(x=year,y=X2b2))+
  geom_point(data=d18O.hist.gdrive.august.spread,aes(x=year,y=iX2b2))+
  geom_point(data=historic_TRSI_IRMS_1.spread,aes(x=year,y=X2b2+0.1),colour="brown")

ggplot()+
  geom_line(data=d18O.hist.OG,aes(x=year,y=X1w))+
  geom_point(data=d18O.hist.gdrive.august.spread,aes(x=year,y=X1w))+
  geom_point(data=historic_TRSI_IRMS_1.spread,aes(x=year,y=X1w+0.1),colour="brown")

ggplot()+
  geom_line(data=d18O.hist.OG,aes(x=year,y=X8b))+
  geom_point(data=d18O.hist.gdrive.august.spread,aes(x=year,y=iX8b))+
  geom_point(data=historic_TRSI_IRMS_1.spread,aes(x=year,y=X8b+0.1),colour="brown")

ggplot()+
  geom_line(data=d18O.hist.OG,aes(x=year,y=X9b))+
  geom_point(data=d18O.hist.gdrive.august.spread,aes(x=year,y=iX9b))+
  geom_point(data=historic_TRSI_IRMS_1.spread,aes(x=year,y=X9b+0.1),colour="brown")


d18O.hist.dt<-detrend(d18O.hist,method="Mean")


d18O.hist.ids<-read.ids(d18O.hist.dt, stc = c(1, 1, 1))
d18O.hist.ids$tree[1]<-1
d18O.hist.ids$tree[2]<-2
d18O.hist.ids$tree[3]<-3
d18O.hist.ids$tree[4]<-4
d18O.hist.ids$tree[5]<-5
rwi.stats.running(d18O.hist.dt, d18O.hist.ids)
d18O.hist.dt.stats <- rwi.stats.running(d18O.hist.dt, d18O.hist.ids,
                                        prewhiten = TRUE,
                                        running.window = TRUE,
                                        window.length = 31,
                                        window.overlap = 11,
                                        min.corr.overlap = 1,
                                        zero.is.missing = TRUE,
                                        method = "spearman")





sss.custom<-function (rwi, ids = NULL) 
{
  rwiVars <- rwi.stats(d18O.redated.no.na.dt, ids = d18O.redated.no.na.dt.IDs,
                       prewhiten = TRUE,
                       zero.is.missing = TRUE,
                       method = "spearman")
  rbar <- rwiVars$rbar.eff
  N <- rwiVars$n.trees
  if (is.null(d18O.redated.no.na.dt.IDs)) {
    n <- rowSums(!is.na(d18O.redated.no.na.dt))
  }
  else {
    colnames.rwi <- colnames(d18O.redated.no.na.dt)
    n <- rep(NA_integer_, nrow(d18O.redated.no.na.dt))
    for (i in seq_len(nrow(d18O.redated.no.na.dt))) {
      cols.with.data <- c(!is.na(d18O.redated.no.na.dt[i, ]))
      trees.this.year <- d18O.redated.no.na.dt.IDs$tree[rownames(d18O.redated.no.na.dt.IDs) %in% colnames.rwi[cols.with.data]]
      n[i] <- length(unique(trees.this.year))
    }
  }
  res <- (n * (1 + (N - 1) * rbar))/(N * (1 + (n - 1) * rbar))
  res
}

d18O.redated.no.na.dt.sss <- sss(d18O.redated.no.na.dt)

d18O.redated.no.na.dt.sss <- sss.custom(d18O.redated.no.na.dt,d18O.redated.no.na.dt.IDs)
d18O.redated.no.na.dt.sss

d18O.hist.dt.sss <- sss(d18O.hist.dt, d18O.hist.ids)
d18O.hist.dt.sss  
