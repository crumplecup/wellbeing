
# set library path
.libPaths( c( 'P:/lib', .libPaths()) )

# set working & temp directory, load packages
setwd('E:/Wells')
library(data.table)
library(sp)
library(rgdal)
library(magrittr)
library(staTools)
library(raster)
library(wellbeing)
write("TMP = 'E:/Riparian/temp'", file=file.path(Sys.getenv('R_USER'), '.Renviron'))

wells <- readOGR('S:/maps/UTILITIES/WELLS/Wells.shp')

bad_wells <- 0
for(i in 1:nrow(wells)) {
  p <- try(plot(wells[i,]))
  if(class(p) == 'try-error') bad_wells <- c(bad_wells, i)
}
bad_wells <- bad_wells[-1]

plot(wells[-bad_wells,])

pump_dates <- as.Date(wells$PUMP_TEST_)
pump_dates[pump_dates == '1899-12-30'] <- NA


pump_month_nos <- month_no(months(pump_dates))
plot(pump_dates, pump_month_nos)
plot(wells$YIELD_SUS[!is.na(pump_dates)], pump_dates[!is.na(pump_dates)])
nrow(wells[wells$YIELD_SUS > 0,])
nrow(wells[wells$RECOVER_T > 0,])
nrow(wells[wells$WZ_FROM > 0,])

hist(pump_month_nos[pump_month_nos>0])

plot(wells$WZ_TO[wells$WZ_FROM > 0] - wells$WZ_FROM[wells$WZ_FROM > 0])
wells$WZ_1_TO[wells$WZ_1_FROM > 0] - wells$WZ_1_FROM[wells$WZ_1_FROM > 0]
wells$WZ_2_TO[wells$WZ_2_FROM > 0] - wells$WZ_2_FROM[wells$WZ_2_FROM > 0]

df <- data.frame(well_id = wells$IDENT[wells$WZ_FROM > 0],
                 wz_depth = wells$WZ_TO[wells$WZ_FROM > 0] -
                   wells$WZ_FROM[wells$WZ_FROM > 0])
df
plot(wells$WZ_FROM[wells$WZ_FROM > 0], wells$DEPTH[wells$WZ_FROM > 0])

wells$wz_dif <- 0
wells$wz_dif[wells$WZ_FROM > 0] <- wells$WZ_TO[wells$WZ_FROM > 0] -
  wells$WZ_FROM[wells$WZ_FROM > 0]
head(wells[wells$wz_dif < 0,])

recs <- wells[wells$RECOVER_T > 0, ]
recs$wlr <- recs$RECOVERY / (recs$RECOVER_T * 60)
recs$wlr
head(recs[recs$wlr == max(recs$wlr),])

wells$WATER_QUAL




