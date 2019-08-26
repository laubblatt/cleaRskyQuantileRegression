#' Small example to test the cleaRskyQuantileRegression package
#'

(LIN2006 =  dt60[SiteCode == "LIN" & year(Date) == 2006, .(Date,Time,IncomingShortwave)])

save(LIN2006, file = "data/LIN2006.rdata")

LIN2006

# apply regression per month and with different windows
(rqmw = LIN2006[ , calc_ClearSky_QuantileRegression_MonthlyTimeWindow(Date,Time,IncomingShortwave, tau = 0.85, lat = 52.21, lon = 14.122, hourshift = 0.5,timeZone = 0)])

plot(IncomingShortwavePotential ~ month, data = rqmw, type = "l", col = 4, ylab = "Shortwave Radiation (W/m2)", ylim = c(0,500))
lines(IncomingShortwaveClearSky ~ month, data = rqmw, type = "l", col =2)
lines(IncomingShortwave ~ month, data = rqmw, type = "b")
legend("topright", c("Potential", "Clear Sky flux", "Observed at surface"), col = c(4,2,1), lty = 1)

# calculate potential surface solar radiation

LIN2006[ , IncomingShortwavePotential := calc_PotRadiation_CosineResponsePower(doy = yday(Date),hour = Time/3600 + 0.5, latDeg = 52.21, longDeg = 14.122, timeZone = 0) ]
LIN2006
#plot(IncomingShortwave ~ IncomingShortwavePotential, data = LIN2006[month(Date) == 6 &  mday(Date) == 7, ], type = "l")

plot(IncomingShortwave ~ IncomingShortwavePotential, data = LIN2006[month(Date) == 6, ], type = "p")



