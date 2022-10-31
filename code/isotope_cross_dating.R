d18O.raw<-read.csv("/Users/danielbalanzategui/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/all_historic_TRSI_measurements_from_gdrive.csv",header=TRUE)
colnames(d18O.raw)<-c("ID","d13C","d18O","file_x")
d18O.raw<-d18O.raw %>% 
  separate(ID, c("ID", "year"), "_")
d18O.raw$year<-as.numeric(d18O.raw$year)
d18O.raw$file_x<-as.factor(d18O.raw$file_x)

d18O.raw.w1<-d18O.raw %>% 
  filter(grepl("1w|w1", ID))
d18O.raw.w1$ID<-"w1"
d18O.raw.w1.spread<-d18O.raw.w1 |> 
  pivot_wider(names_from = file_x, 
              values_from = d18O) |>
  arrange(year)
d18O.raw.w1.spread

#2b and x2b
d18O.raw.2b<-d18O.raw %>% 
  dplyr::filter(ID == "2b")
d18O.raw.x2b<-d18O.raw %>% 
  dplyr::filter(ID == "x2b")

d18O.raw.2b<-rbind(d18O.raw.2b,d18O.raw.x2b)
d18O.raw.2b$ID<-"x2b"

d18O.raw.2b.spread<-d18O.raw.2b |> 
  pivot_wider(names_from = file_x, 
              values_from = d18O) |>
  arrange(year)
d18O.raw.2b.spread

#2b2 and x2b2
d18O.raw.2b2<-d18O.raw %>% 
  dplyr::filter(ID == "2b2")

d18O.raw.x2b2<-d18O.raw %>% 
  dplyr::filter(ID == "x2b2")

d18O.raw.2b2<-rbind(d18O.raw.2b2,d18O.raw.x2b2)
d18O.raw.2b2$ID<-"x2b2"
d18O.raw.2b2.spread<-d18O.raw.2b2 |> 
  pivot_wider(names_from = file_x, 
              values_from = d18O) |>
  arrange(year)
d18O.raw.2b2.spread

#8b
d18O.raw.8b<-d18O.raw %>% 
  filter(grepl("8b|x8b", ID))
d18O.raw.8b$ID<-"x8b"
d18O.raw.8b.spread<-d18O.raw.8b |> 
  pivot_wider(names_from = file_x, 
              values_from = d18O) |>
  arrange(year)
d18O.raw.8b.spread

#9b
d18O.raw.9b<-d18O.raw %>% 
  filter(grepl("9b|x9b", ID))
d18O.raw.9b$ID<-"x9b"
d18O.raw.9b.spread<-d18O.raw.9b |> 
  pivot_wider(names_from = file_x, 
              values_from = d18O) |>
  arrange(year)
d18O.raw.9b.spread


##################
d18O.consol<-read.csv("/Users/danielbalanzategui/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/all_historic_TRSI_measurements_from_gdrive_cleaning.csv",header=TRUE)
colnames(d18O.consol)<-c("ID","year","d18O")

d18O.consol<-dplyr::filter(d18O.consol,year >= 200 & year <= 350)

a<-ggplot()+
  geom_line(data=d18O.hist.OG,aes(x=year,y=scale(X1w)),colour="orange",alpha=0.5)+
  geom_line(data=X2b,aes(x=year,y=scale(nX2b)),colour="red",alpha=0.5)+
  geom_line(data=X2b2,aes(x=year,y=scale(nX2b2)),colour="blue",alpha=0.5)+
  geom_line(data=X8b,aes(x=year,y=scale(nX8b)),colour="seagreen",alpha=0.5)+
  geom_line(data=X9b,aes(x=year,y=scale(nX9b)),colour="brown",alpha=0.5)+
  # geom_point(data=d18O.hist.OG,aes(x=year,y=scale(X1w)),colour="orange")+
  # geom_point(data=X2b,aes(x=year,y=scale(nX2b)),colour="red")+
  # geom_point(data=X2b2,aes(x=year,y=scale(nX2b2)),colour="blue")+
  # geom_point(data=X8b,aes(x=year,y=scale(nX8b)),colour="seagreen")+
  # geom_point(data=X9b,aes(x=year,y=scale(nX9b)),colour="brown")+
  # geom_text(data=X8b,aes(x=year,y=scale(nX8b)+0.4,label=year),colour="seagreen")+
  coord_cartesian(xlim=c(200,350))


c<-ggplot(d18O.consol)+
  #geom_vline(xintercept = c(200:350),size=0.1)+
  geom_line(aes(x=year,y=scale(d18O),colour=ID,group=ID),alpha=0.5)+
  # stat_summary(data=d18O.consol,fun.y = mean, color = "red", geom = "line",aes(x=year,y=scale(d18O))) +
  # stat_summary(data=d18O.consol,fun.y = mean, color = "red", geom = "point",aes(x=year,y=scale(d18O))) +
  scale_colour_manual(values=c("orange","red","blue","seagreen","brown"))+
  coord_cartesian(xlim=c(200,350))
a/c
##################
d18O.consol.spread<-as.data.frame(d18O.consol |> 
  pivot_wider(names_from = ID, 
              values_from = d18O) |>
  arrange(year))
rownames(d18O.consol.spread)<-d18O.consol.spread$year
d18O.consol.spread.RWI<-d18O.consol.spread[,-1]

d18O.consol.spread.RWI.IDs<-read.ids(d18O.consol.spread.RWI)
d18O.consol.spread.RWI.IDs$tree<-c(1,2,3,4,5)
d18O.consol.spread.RWI.IDs$site<-c(1,1,1,1,1)

d18O.consol.spread.RWI.stats <- rwi.stats.running(rwi=d18O.consol.spread.RWI,
                                                  ids=d18O.consol.spread.RWI.IDs,
                                        prewhiten = TRUE,
                                        running.window = TRUE,
                                        window.length = 31,
                                        window.overlap = 11,
                                        min.corr.overlap = 1,
                                        zero.is.missing = TRUE,
                                        method = "spearman")
d18O.consol.spread.RWI.stats


d18O.raw.OG<-data.frame(year=d18O.hist.OG$year,
           X1w=d18O.hist.OG$X1w,
           X2b$nX2b,
           X2b2$nX2b2,
           X8b$nX8b,
           X9b$nX9b)
rownames(d18O.raw.OG)<-d18O.raw.OG$year
d18O.raw.OG.RWI<-d18O.raw.OG[,-1]
d18O.raw.OG.RWI.IDs<-read.ids(d18O.raw.OG.RWI)
d18O.raw.OG.RWI.IDs$tree<-c(1,2,3,4,5)
d18O.raw.OG.RWI.IDs$site<-c(1,1,1,1,1)

d18O.raw.OG.RWI.stats <- rwi.stats.running(rwi=d18O.raw.OG.RWI,
                                                  ids=d18O.raw.OG.RWI.IDs,
                                                  prewhiten = TRUE,
                                                  running.window = TRUE,
                                                  window.length = 31,
                                                  window.overlap = 11,
                                                  min.corr.overlap = 1,
                                                  zero.is.missing = TRUE,
                                                  method = "spearman")
d18O.raw.OG.RWI.stats

plot(d18O.raw.OG.RWI.stats$mid.year,d18O.raw.OG.RWI.stats$eps,type="l",ylim=c(0,1))
lines(d18O.consol.spread.RWI.stats$mid.year,d18O.consol.spread.RWI.stats$eps,col="red")
