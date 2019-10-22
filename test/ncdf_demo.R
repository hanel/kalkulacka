require(raster)
require(rgdal)
require(data.table)

setwd('/home/mha/GIT/kalkulacka/data')


b = brick('pr_step_day_2016.nc')

pov = readOGR('PLO40hranice_S-JTSK_Krovak_East_North.shp')
wpov = spTransform(pov, projection(b))

e = extract(b, wpov[1,], fun = mean, weights = TRUE, na.rm=TRUE)

dta = data.table(DTM = seq(as.Date('1961-01-01'), length = ncol(e), by = 'day'), PR = e[1,])
