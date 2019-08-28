#' Small example to test the cleaRskyQuantileRegression package
#'

# (LIN2006 =  dt60[SiteCode == "LIN" & year(Date) == 2006, .(Date,Time,IncomingShortwave)])
#
# save(LIN2006, file = "data/LIN2006.rdata")

#(LIN2003 =  dt30[SiteCode == "LIN" & year(Date) == 2003, .(Date,Time,IncomingShortwave)])
#
#save(LIN2003, file = "data/LIN2003.rdata")


# data(LIN2006)
#
# # apply regression per month and with different windows
# (rqmw = LIN2006[ , calc_ClearSky_QuantileRegression_MonthlyTimeWindow(Date,Time,IncomingShortwave, tau = 0.85, lat = 52.21, lon = 14.122, hourshift = 0.5,timeZone = 0)])
#
# plot(IncomingShortwavePotential ~ month, data = rqmw, type = "l", col = 4, ylab = "Shortwave Radiation (W/m2)", ylim = c(0,500))
# lines(IncomingShortwaveClearSky ~ month, data = rqmw, type = "l", col =2)
# lines(IncomingShortwave ~ month, data = rqmw, type = "b")
# legend("topright", c("Potential", "Clear Sky flux", "Observed at surface"), col = c(4,2,1), lty = 1)
#
# # calculate potential surface solar radiation
#
# LIN2006[ , IncomingShortwavePotential := calc_PotRadiation_CosineResponsePower(doy = yday(Date),hour = Time/3600 + 0.5, latDeg = 52.21, longDeg = 14.122, timeZone = 0) ]
# LIN2006
# #plot(IncomingShortwave ~ IncomingShortwavePotential, data = LIN2006[month(Date) == 6 &  mday(Date) == 7, ], type = "l")
#
# plot(IncomingShortwave ~ IncomingShortwavePotential, data = LIN2006[month(Date) == 6, ], type = "p")
#
#

### check if the error can be reproduced
# dt60
#
# system.time(
#   qryrmon <- dt30[ , calc_ClearSky_QuantileRegression_MonthlyTimeWindow(Date = Date,
#                                                                         Time = Time, IncomingShortwave = IncomingShortwave,
#                                                                         IncomingShortwavePotential = Rsdpot_12,
#                                                                         mc.cores = 10,tau = 0.85, pdev = 0.25), by = SiteCode]
# )


data(LIN2003)
(dat = LIN2003[year(Date) == 2003 & month(Date) == 8, ])

dat[ , Rsdpot_12 := calc_PotRadiation_CosineResponsePower(doy = yday(Date), hour = Time/3600 + 0.25,
                                                          latDeg = 52.21 ,
                                                          longDeg = 14.122,
                                                          timeZone = 0, isCorrectSolartime = TRUE,
                                                          cosineResponsePower = 1.2 )]

plot(IncomingShortwave ~  Rsdpot_12, data = dat,
     xlab = expression(bold("Potential Shortwave Radiation ")(W*m^-2)), ylab = expression(bold("Observed Shortwave Radiation ")(W*m^-2)) )


library(lattice)
lab_Rsdpot12_name = expression(bold("Potential Shortwave Radiation ")(W*m^-2))
lab_Rsdpot12 = expression(bold( R['sd,pot']==S[0]*cdot*cos(SZA)^1.2*" " )(W*m^-2))
lab_Rsd      = expression(bold("Observed Shortwave Radiation ")(W*m^-2))
xyplot(IncomingShortwave ~  Rsdpot_12, data = dat,
       xlab = list(lab_Rsdpot12_name,cex = 1.3), ylab = list(label=lab_Rsd, cex=1.3),
       type = c("p","g"), pch = ".", cex = 3,
       # main = paste0(dtyrmon[SiteCode == sico, unique(SiteName)]," ,Quantile Regression, tau = 0.90" ),
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.abline(rq(y~x, tau = 0.85), col=1, lwd = 2)
         # panel.abline(0,1, col=1, lty = 2, lwd = 2)
         panel.ablineq(0,1, col=1, lty = 2, lwd = 2, at = 0.89, label = "1:1", rotate = TRUE, fontfamily = "sans", cex = 1.5,pos = 3)
         panel.ablineq(rq(y~x, tau = 0.85), col=1, at = 0.7, rotate =TRUE,
                       pos = 3, label = "Slope of 85% Quantile", cex = 1.5, font = "Helvetica", fontface = 2 )
         panel.text(700,-20, "half hourly data of one month", fontfamily = "Helvetica")
       },
       grid = TRUE)


data(LIN2003)
(rqmw = LIN2003[ , calc_ClearSky_QuantileRegression_MonthlyTimeWindow(Date,Time,IncomingShortwave, tau = 0.85, lat = 52.21, lon = 14.122, hourshift = 0.5,timeZone = 0)])
plot(IncomingShortwavePotential ~ month, data = rqmw, type = "l", col = 4, ylab = "Monthly mean Shortwave Radiation (W/m2)", ylim = c(0,500))
lines(IncomingShortwaveClearSky ~ month, data = rqmw, type = "l", col =2)
lines(IncomingShortwave ~ month, data = rqmw, type = "b")
legend("topright", c("Potential", "Clear Sky flux", "Observed at surface"), col = c(4,2,1), lty = 1)
