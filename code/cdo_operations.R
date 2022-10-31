#cdo operations

#select variable
#cdo select,name=tmn cru_ts4.04.1901.2019.tmn.dat.nc tmn.cru_ts4.04.1901.2019.tmn.dat.nc
#cdo select,name=tmx cru_ts4.04.1901.2019.tmx.dat.nc tmx.cru_ts4.04.1901.2019.tmx.dat.nc
#cdo select,name=pre cru_ts4.04.1901.2019.pre.dat.nc pte.cru_ts4.04.1901.2019.pre.dat.nc

#select years
# cdo -selyear,1981/2010 tmn.cru_ts4.04.1901.2019.tmn.dat.nc cru_ts4.04.1981.2010.tmn.dat.nc
# cdo -selyear,1981/2010 tmx.cru_ts4.04.1901.2019.tmx.dat.nc cru_ts4.04.1981.2010.tmx.dat.nc
# cdo -selyear,1981/2010 pre.cru_ts4.04.1901.2019.pre.dat.nc cru_ts4.04.1981.2010.pre.dat.nc
# cdo -selyear,1981/2010 spei04.nc spei04.nc.1981.2010.nc
# cdo -selyear,1981/2010 spei08.nc spei08.nc.1981.2010.nc

#for SPEI select July/October
#cdo -selmon,4 spei04.nc.1981.2010.nc july_spei04.nc.1981.2010.nc
#cdo -selmon,10 spei08.nc.1981.2010.nc october_spei08.nc.1981.2010.nc


#annual mean 
#cdo -yearmean cru_ts4.04.1981.2010.tmn.dat.nc yearlyMean_cru_ts4.04.1981.2010.tmn.dat.nc
#cdo -yearmean cru_ts4.04.1981.2010.tmx.dat.nc yearlyMean_cru_ts4.04.1981.2010.tmx.dat.nc
#cdo -yearsum cru_ts4.04.1981.2010.pre.dat.nc yearlySum_cru_ts4.04.1981.2010.pre.dat.nc

#climatology for 1981 to 2010 
# cdo -timmean yearlyMean_cru_ts4.04.1981.2010.tmn.dat.nc climatology_cru_ts4.04.1981.2010.tmn.dat.nc
# cdo -timmean yearlyMean_cru_ts4.04.1981.2010.tmx.dat.nc climatology_cru_ts4.04.1981.2010.tmx.dat.nc
# cdo -timmean yearlySum_cru_ts4.04.1981.2010.pre.dat.nc climatology_cru_ts4.04.1981.2010.pre.dat.nc
# cdo -timmean july_spei04.nc.1981.2010.nc climatology_july_spei04.nc.1981.2010.nc
# cdo -timmean october_spei08.nc.1981.2010.nc climatology_october_spei08.nc.1981.2010.nc
