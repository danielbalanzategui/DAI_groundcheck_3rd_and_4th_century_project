spatial.analysis.packages<-c("sf","ggplot2","ggplot2","dplyr","tidyverse","raster",
                             "lubridate","reshape2","colorspace","rnaturalearth","rnaturalearthdata",
                             "patchwork")
lapply(spatial.analysis.packages, require, character.only = TRUE)

#spatial analysis
#load data
tmn_climatology<-raster::raster("/Users/danielbalanzategui/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/climatology_cru_ts4.04.1981.2010.tmn.dat.nc")
tmn_climatology<-crop(tmn_climatology, extent(0,21,48,57))
tmn_climatologyXYZ<-rasterToPoints(tmn_climatology)
colnames(tmn_climatologyXYZ)<-c("x","y","tmn")
tmn_climatologyXYZ<-as.data.frame(tmn_climatologyXYZ)

#exploratory plot of raw data
# plot(tmn_climatology)

tmx_climatology<-raster::raster("/Users/danielbalanzategui/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/climatology_cru_ts4.04.1981.2010.tmx.dat.nc")
tmx_climatology<-crop(tmx_climatology, extent(0,21,48,57))
tmx_climatologyXYZ<-rasterToPoints(tmx_climatology)
colnames(tmx_climatologyXYZ)<-c("x","y","tmx")
tmx_climatologyXYZ<-as.data.frame(tmx_climatologyXYZ)
# plot(tmx_climatology)

tmmean_climatologyXYZ<-data.frame(x=tmx_climatologyXYZ$x,y=tmx_climatologyXYZ$y,tmean=(tmx_climatologyXYZ$tmx+tmn_climatologyXYZ$tmn)/2)

pre_climatology<-raster::raster("/Users/danielbalanzategui/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/climatology_cru_ts4.04.1981.2010.pre.dat.nc")
pre_climatology<-crop(pre_climatology, extent(0,21,48,57))
pre_climatologyXYZ<-rasterToPoints(pre_climatology)
colnames(pre_climatologyXYZ)<-c("x","y","pre")
pre_climatologyXYZ<-as.data.frame(pre_climatologyXYZ)
# plot(pre_climatology)

spei_climatology<-raster::raster("/Users/danielbalanzategui/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/climatology_october_spei08.nc.1981.2010.nc")
spei_climatology<-crop(spei_climatology, extent(0,21,48,57))
spei_climatologyXYZ<-rasterToPoints(spei_climatology)
colnames(spei_climatologyXYZ)<-c("x","y","spei")
spei_climatologyXYZ<-as.data.frame(spei_climatologyXYZ)
# plot(spei_climatology)

#get shapefile for study area
world_coastlines <- ne_coastline(scale = 10, returnclass = 'sf')
bbox_europe <- st_bbox(c(xmin=0,xmax=21,ymin=48,ymax=57), crs = st_crs(world_coastlines))
european_union_map_cropped <- st_crop(world_coastlines, bbox_europe)

pre.gg<-ggplot() +
  geom_raster(data=pre_climatologyXYZ,aes(x=x,y=y,fill=pre),alpha=0.8)+
  geom_sf(data=european_union_map_cropped,fill="black",size=0.25)+
  geom_rect(aes(xmin=8,xmax=15,ymin=51,ymax=54),size=0.4,colour="grey20",fill=NA,alpha=0.4)+
  scale_fill_binned_sequential(name="PPT",palette = "Plasma",n.breaks=10) +
  xlab("")+
  ylab("")+
  theme(plot.title.position = "plot",
        legend.position = "bottom",
        legend.key.width=unit(2,"cm"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        axis.text = element_text(size=16))+
  coord_sf(xlim=c(0,21),ylim=c(48,57))

tmean.gg<-ggplot() +
  geom_raster(data=tmmean_climatologyXYZ,aes(x=x,y=y,fill=tmean),alpha=0.8)+
  geom_sf(data=european_union_map_cropped,fill="black",size=0.25)+
  geom_rect(aes(xmin=8,xmax=15,ymin=51,ymax=54),size=0.4,colour="grey20",fill=NA,alpha=0.4)+
  scale_fill_binned_sequential(name="Tmean",palette = "Plasma",rev = FALSE,n.breaks=10) +
  xlab("")+
  ylab("")+
  theme(plot.title.position = "plot",
        legend.position = "bottom",
        legend.key.width=unit(2,"cm"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        axis.text = element_text(size=16))+
  coord_sf(xlim=c(0,21),ylim=c(48,57))

spei.gg<-ggplot() +
  geom_raster(data=spei_climatologyXYZ,aes(x=x,y=y,fill=spei),alpha=0.8)+
  geom_sf(data=european_union_map_cropped,fill="black",size=0.25)+
  geom_rect(aes(xmin=8,xmax=15,ymin=51,ymax=54),size=0.4,colour="grey20",fill=NA,alpha=0.4)+
  scale_fill_binned_sequential(name="SPEI",palette = "Plasma",rev = TRUE,n.breaks=10) +
  xlab("")+
  ylab("")+
  theme(plot.title.position = "plot",
        legend.position = "bottom",
        legend.key.width=unit(2,"cm"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        axis.text = element_text(size=16))+
  coord_sf(xlim=c(0,21),ylim=c(48,57))

png("/Users/danielbalanzategui/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/figures/climatology_1981_2010.png",width = 21.3, height = 6, units = "in", res=300)
tmean.gg+pre.gg+spei.gg +
  plot_layout(design=gg.layout.climatology)
dev.off()

# stars_object <- raster::raster("your_data_set.nc") %>% st_as_stars()
# sf_object <- sf::st_read("second_data_set.shp")
# 
# #Make sure they have the same CRS
# sf::st_crs(stars_object) <- sf::st_crs(sf_object)
# 
# #Crop to bounding box (insert coordinates in ...)
# bb <- sf::st_bbox(xmin = ..., xmax = ..., ymin = ..., ymax = ...)
# stars_object <- stars_object[bb]
# 
# #plot
# ggplot()+
#   geom_stars(data = stars_object) +
#   geom_sf(data = sf_object)

oak_metadata<-fread("/Users/danielbalanzategui/Documents/R/DAI_groundcheck_3rd_and_4th_century_project/raw_data/alles_koordi_merged_orginal_10_2022_corrections.csv",header=TRUE, encoding = 'UTF-8')
oak_metadata<-oak_metadata[grepl(c("Eiche","eiche"),oak_metadata),]
as.numeric(oak_metadata$Breitengrad)

oak_metadata<-dplyr::filter(oak_metadata, longitude >= 8 & longitude <= 15)
oak_metadata<-dplyr::filter(oak_metadata, latitude >= 51 & latitude <= 54)

ff$start<-as.numeric(ff$start)
ff$end<-as.numeric(ff$end)
ff$felling_date<-as.numeric(ff$felling_date)
ff$felling_date.2<-ifelse(ff$felling_date == 0,ff$end,ff$felling_date)

ff.3<-dplyr::filter(ff.2, felling_date.2 >= -100 & felling_date.2 <= 699)


hist(as.numeric(ff.3$felling_date),breaks=10)

ggplot(ff)+

ggplot(ff)+
  geom_point(aes(x=longitude,y=latitude),alpha=0.2)+
  geom_point(data=ff.2,aes(x=longitude,y=latitude),alpha=0.2,colour="orange")+
  coord_sf(xlim=c(0,21),ylim=c(48,57))

    