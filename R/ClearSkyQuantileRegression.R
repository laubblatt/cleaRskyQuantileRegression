
#' @filename ClearSkyQuantileRegression.R
#' @host gitbgc/clearskyqunatileregression/R/
#' @author Maik Renner, mrenner [at] bgc-jena.mpg.de

# require(quantreg)
# library(data.table)
# library(parallel)  # mclapply
# library(lubridate) # floor_date
# require(phaselag) # mlm.output.statlong.call.rq


#' @export calc_ClearSky_QuantileRegression_MonthlyTimeWindow
#' @export calc_ClearSky_QuantileRegression

#' @import quantreg
#' @import data.table
#' @import parallel
#' @import lubridate
#'
#' @version 0.20 2019-08-20 extract core functions from M44_BSRN_hourly_quantreg_85.r script
#' @version 0.21 2019-08-21 complete core function and documentation
#' @version 0.22 2019-08-22 update function and variable names
#' @version 0.22 2019-08-22 prepare documentation for package
#'
# https://stackoverflow.com/questions/15932585/multiple-functions-in-one-rd-file

#' @examples
#' data(LIN2006)
#' # apply regression per month and with different windows
#' (rqmw = LIN2006[ , calc_ClearSky_QuantileRegression_MonthlyTimeWindow(Date,Time,IncomingShortwave, tau = 0.85, lat = 52.21, lon = 14.122, hourshift = 0.5,timeZone = 0)])
#'
#' plot(IncomingShortwavePotential ~ month, data = rqmw, type = "l", col = 4, ylab = "Shortwave Radiation (W/m2)", ylim = c(0,500))
#' lines(IncomingShortwaveClearSky ~ month, data = rqmw, type = "l", col =2)
#' lines(IncomingShortwave ~ month, data = rqmw, type = "b")
#' legend("topright", c("Potential", "Clear Sky flux", "Observed at surface"), col = c(4,2,1), lty = 1)
#'
#' # calculate potential surface solar radiation
#'
#' LIN2006[ , IncomingShortwavePotential := calc_PotRadiation_CosineResponsePower(doy = yday(Date),hour = Time/3600 + 0.5, latDeg = 52.21, longDeg = 14.122, timeZone = 0) ]
#' LIN2006
#' #plot(IncomingShortwave ~ IncomingShortwavePotential, data = LIN2006[month(Date) == 6 &  mday(Date) == 7, ], type = "l")
#'
#' plot(IncomingShortwave ~ IncomingShortwavePotential, data = LIN2006[month(Date) == 6, ], type = "p")
#'

NULL

##### core function ---------------------------
calc_ClearSky_QuantileRegression = function(IncomingShortwave, IncomingShortwavePotential, tau = 0.85) {
  #' Calculate fractional absorption of solar radiation by Quantile regression
  #' of the observed incoming shortwave radiation against potential radiation
  #'
  #' The slope at a given quantile (here 0.85) is assumed to represent the fractional absorption of solar radiation under cloud-free conditions.
  #' This can be used to estimate the clear sky solar radiation of a period of one month when global radiation records are available.
  #' The observations should not be biased, since this directly effects the estimate.
  #' Persistent cloud-cover will reduce the estimate, but often results in poorer goodness of fit.
  #' The estimate is not useful for nighttime conditions.
  #' Details are described in Renner et al., 2019 ESS
  #'
  #' @param IncomingShortwave Vector of observed incoming total Shortwave radiation in units W m-2, also refferred to as global radiation or the sum of diffuse and direct radiation if available
  #' @param IncomingShortwavePotential Vector pf potential surface shortwave radiation at the surface without clouds.
  #' @seealso \code{[calc_PotRadiation_CosineResponsePower()]} for calculating  potential surface shortwave radiation as function of date, time and position
  #' @param tau Numeric constant, quatile for which the regression is being performed.
  #'   Defaults to 0.85 based on many BSRN stations, but this may vary (cloud type, frequency and aerosols).
  #' @return A data.table with columns tau, statistic and value.
  #' Key statistics are R1 (measure of goodness of fit), slope1 (linear slope assumed to represent the fractional absorption of solar radiation under cloud-free conditions),
  #' slope_sd (the standard deviation of the slope estimate), intercept (which should be close to 0, otherwise there is a problem in the correct timing of the observed and potential radiation), and
  #' n the sample size
  #' @author Maik Renner, mrenner [at] bgc-jena.mpg.de
  #' @references  Renner, M., M. Wild, M. Schwarz, and A. Kleidon.
  #'   "Estimating Shortwave Clear-Sky Fluxes from Hourly Global
  #'     Radiation Records by Quantile Regression."
  #'       Earth and Space Science, 2019.
  #'       \url{https://doi.org/10.1029/2019EA000686}
  #'
  dt = data.table(IncomingShortwave = IncomingShortwave, IncomingShortwavePotential = IncomingShortwavePotential)
  dt[ , mlm.output.statlong.call.rq("IncomingShortwave ~ IncomingShortwavePotential", data = .SD, tau = tau) ]
}

# dt30[ SiteCode == "ASP" & year(Date) == 2012 & month(Date) == 7 , mlm.output.statlong.call.rq("IncomingShortwave ~ Rsdpot_12",
#                                                                                               data = .SD, tau = seq(0.7,0.99,0.1)), by = list(SiteCode)]

### main function
calc_ClearSky_QuantileRegression_MonthlyTimeWindow = function(Date, Time, IncomingShortwave,
                                                       IncomingShortwavePotential = NULL, tau = 0.85, lat = NULL, lon = NULL, hourshift = 0.25, timeZone = 0, isCorrectSolartime = TRUE, cosineResponsePower = 1.2,
                                                       pdev = 0.25,
                                                       mc.cores = 1,
                                                       briefoutput = TRUE )
  #' Main function to estimate the clear-sky solar radiation based on (half-)hourly global radiation data for monthly periods.
  #'
  #'   This function will estimate the fractional solar transmission per year and month and for moving averages with window sizes of 3, 5, and 7 months.
  #'   Based on the deviation of the monthly estimates from the site average and the goodness of fit the appropriate window is selected.
  #'   For this window the resulting fractional solar transmission is used to calculate the ClearSky Shortwave radiation.
  #'   The methodology is described in Renner et al., (2019) ESS.
  #'   The function may take some time since different window length are computed.
  #'
  #'
  #' @param Date a vector of Dates in the format of data.table::as.IDate
  #' @param Time a vector of Times in the format of data.table::as.ITime
  #' @param IncomingShortwave a vector of subdaily (hourly or half-hourly) observations of solar radiation
  #' @param IncomingShortwavePotential a vector of potential solar radiation (if not supplied it will be computed)
  #' @param tau numeric the quatile for which the regression is being performed
  #'             Defaults to 0.85 based on many BSRN stations, but this may vary (cloud type, frequency and aerosols)
  #' @param lat Latitude of the site in degrees
  #' @param lon Longitude of the site in degrees
  #' @param hourshift numeric A shift in hours to align the potential solar radiation with the observed radiation. Here 0.25 is used as default for half-hourly aggregated data
  #'        (a shift will induce hyseresis and an intercept term in quantile regression)
  #' @param timeZone numeric, if local time is used then 0 otherwise the time must be adapted
  #' @param cosineResponsePower numeric defaults to 1.2, may be lower at high latitude sites, see Long and Ackerman (2000) JGR
  #' @param pdev numeric allowable fractional deviation of the monthly estimate from the site average estimate (here 0.25)
  #' @param mc.cores Numeric setting the number of parallel child processes to calculate the regressions for each month, \code{@seealso[detectCores()]}
  #' @param briefoutput logical defaults to TRUE, otherwise all statistics are reported
  #' @return a data.table with average fluxes for year and months, the fractional transmission estimate (ftau), its corresponding Window length and quantile and the monthly mean ClearSky incoming solar radiation
  #' @details   This function may also compute the potential solar radiation when this is not given, this requires lat, long, time with timezone and possible shift.
  #' @details To speed up the calculations the parallel package is used. This will not work under Windows, unless mc.cores = 1.
  #'
  #' @references  Renner, M., M. Wild, M. Schwarz, and A. Kleidon.
  #'   "Estimating Shortwave Clear-Sky Fluxes from Hourly Global
  #'     Radiation Records by Quantile Regression."
  #'       Earth and Space Science, 2019.
  #'       \url{https://doi.org/10.1029/2019EA000686}
  #'
  #' @author Maik Renner, mrenner [at] bgc-jena.mpg.de
  #' @examples
  #' data(LIN2006)
  #' # apply regression per month and with different windows
  #' # Check available cores to parallize the task with setting mc.cores larger than 1
  #' detectCores()
  #' (rqmw = LIN2006[ , calc_ClearSky_QuantileRegression_MonthlyTimeWindow(Date,Time,IncomingShortwave, tau = 0.85, lat = 52.21, lon = 14.122, hourshift = 0.5,timeZone = 0, mc.cores = 1)])
  #'
  #' plot(IncomingShortwavePotential ~ month, data = rqmw, type = "l", col = 4, ylab = "Shortwave Radiation (W/m2)", ylim = c(0,500))
  #' lines(IncomingShortwaveClearSky ~ month, data = rqmw, type = "l", col =2)
  #' lines(IncomingShortwave ~ month, data = rqmw, type = "b")
  #' legend("topright", c("Potential", "Clear Sky flux", "Observed at surface"), col = c(4,2,1), lty = 1)
  #'
  #'
{

  if( is.null(IncomingShortwavePotential) | length(IncomingShortwavePotential) != length(IncomingShortwave)) {

    IncomingShortwavePotential = calc_PotRadiation_CosineResponsePower(doy = yday(Date), hour = Time/3600 + hourshift,
                                                                  latDeg = lat ,
                                                                  longDeg = lon,
                                                                  timeZone = timeZone,  isCorrectSolartime = isCorrectSolartime, cosineResponsePower = cosineResponsePower )

  }

  ## full series of the site
  dth = data.table(Date,Time,IncomingShortwave, IncomingShortwavePotential)

  rqall = dth[ , calc_ClearSky_QuantileRegression(IncomingShortwave,IncomingShortwavePotential,tau)][ , Window := "all"]

  ## for each month
  rqmon = dth[ , calc_ClearSky_QuantileRegression(IncomingShortwave,IncomingShortwavePotential,tau), by = list(year(Date), month(Date))][ , Window := "1mon"]


  # dth = dt30[SiteCode == "LIN" & year(Date) == 1997 , .(Date,Time,IncomingShortwave, IncomingShortwavePotential = PotRadiation_CosineResponsePower(doy = yday(Date),
  #                                                                                                                                                  hour = Time/3600 + 0.25,
  #                                                                                                                                                  latDeg = 51 ,
  #                                                                                                                                                  longDeg = 11,
  #                                                                                                                                                  timeZone = 0,
  #                                                                                                                                                  isCorrectSolartime = TRUE, CosineResponsePower = 1.2 )) ]
  #
  dth[ , yrmon := paste(year(Date), month(Date), sep = "_") ]
  setkey(dth,yrmon)
#  dth
  # x = "1997_1"
    system.time(
    results3mon <- mclapply( unique(dth[[ "yrmon"]]),
                             function(x) {
                               d1 = dth[.(x), floor_date( first(Date), "month") - months(1) ]
                               d2 = dth[.(x), floor_date(first(Date), "month") + months(2)]
                               year = dth[.(x), unique(year(Date)) ]
                               month = dth[.(x), unique(month(Date)) ]
                               if (is.na(d1) | is.na(d2)) {
                                 out = data.table()#SiteCode = sid, tau = NA, statistic = NA, value = NA, year,month)
                               } else {
                                 out =  dth[ Date %in% d1:d2, ][
                                   sum(!is.na(IncomingShortwave) & IncomingShortwavePotential > 10) > 300,
                                   calc_ClearSky_QuantileRegression(IncomingShortwave,IncomingShortwavePotential, tau)][ , year := year][, month := month]
                               }
                               if (nrow(out)< 2) {
                                 out = data.table()#SiteCode = sid, statistic = NA, value = NA, year,month)
                               }
                               return(out)
                             }, mc.cores = mc.cores)
  )
  rq3mon = rbindlist(results3mon,fill = TRUE)[ , Window := "3mon"]

  results5mon <- mclapply( unique(dth[[ "yrmon"]]),
                           function(x) {
                             d1 = dth[.(x), floor_date( first(Date), "month") - months(2) ]
                             d2 = dth[.(x), floor_date(first(Date), "month") + months(3)]
                             year = dth[.(x), unique(year(Date)) ]
                             month = dth[.(x), unique(month(Date)) ]
                             if (is.na(d1) | is.na(d2)) {
                               out = data.table()#SiteCode = sid, tau = NA, statistic = NA, value = NA, year,month)
                             } else {
                               out =  dth[ Date %in% d1:d2, ][
                                 sum(!is.na(IncomingShortwave) & IncomingShortwavePotential > 10) > 300,
                                 calc_ClearSky_QuantileRegression(IncomingShortwave,IncomingShortwavePotential, tau)][ , year := year][, month := month]
                             }
                             if (nrow(out)< 2) {
                               out = data.table()#SiteCode = sid, statistic = NA, value = NA, year,month)
                             }
                             return(out)
                           }, mc.cores = mc.cores)
  rq5mon = rbindlist(results5mon,fill = TRUE)[ , Window := "5mon"]

results7mon <- mclapply( unique(dth[[ "yrmon"]]),
                         function(x) {
                           d1 = dth[.(x), floor_date( first(Date), "month") - months(3) ]
                           d2 = dth[.(x), floor_date(first(Date), "month") + months(4)]
                           year = dth[.(x), unique(year(Date)) ]
                           month = dth[.(x), unique(month(Date)) ]
                           if (is.na(d1) | is.na(d2)) {
                             out = data.table()#SiteCode = sid, tau = NA, statistic = NA, value = NA, year,month)
                           } else {
                             out =  dth[ Date %in% d1:d2, ][
                               sum(!is.na(IncomingShortwave) & IncomingShortwavePotential > 10) > 300,
                               calc_ClearSky_QuantileRegression(IncomingShortwave,IncomingShortwavePotential, tau)][ , year := year][, month := month]
                           }
                           if (nrow(out)< 2) {
                             out = data.table()#SiteCode = sid, statistic = NA, value = NA, year,month)
                           }
                           return(out)
                         }, mc.cores = mc.cores)
rq7mon = rbindlist(results7mon,fill = TRUE)[ , Window := "7mon"]

  rqout = rbind(rqall,rqmon,rq3mon,rq5mon, rq7mon, fill = TRUE)
  rqmons = rbind(rqmon,rq3mon,rq5mon, rq7mon, fill = TRUE)

  ### now identify the window based on the stats
  dtyrmon = aggregate2yrmon(dth)
  # return( dcast(rqmons, ... ~ statistic + Window) )
  dtyrmon =  merge( dtyrmon, dcast(rqmons, ... ~ statistic + Window), by = c("year", "month"))
  # return( dcast(rqmons, ... ~ statistic + Window) )
  dtyrmon = cbind(dtyrmon, dcast(rqall, ... ~ statistic + Window) )
  # dtyrmon

  dtyrmon[ , ftau := NA_real_]
  dtyrmon[R1_1mon > 0.75 &
            slope1_1mon < (slope1_all *( 1 + pdev)) &
            slope1_1mon > (slope1_all *( 1 - pdev) ), ftau := slope1_1mon]
  dtyrmon[!is.na(ftau) , Window := "1mon"]

  dtyrmon[is.na(ftau) &  R1_3mon > 0.75 & slope1_3mon < (slope1_all *(1 + pdev)) &
            slope1_3mon > (slope1_all *(1 - pdev)) , c("ftau","Window") := list(slope1_3mon, "3mon")]

  dtyrmon[is.na(ftau) &  R1_5mon > 0.75 & slope1_5mon < (slope1_all *(1 + pdev)) &
            slope1_5mon > (slope1_all *(1 - pdev)) , c("ftau","Window") := list(slope1_5mon, "5mon")]

  dtyrmon[is.na(ftau) &  R1_7mon > 0.75 & slope1_7mon < (slope1_all *(1 + pdev)) &
            slope1_7mon > (slope1_all *(1 - pdev)) , c("ftau","Window") := list(slope1_7mon, "7mon")]

  dtyrmon[ , IncomingShortwaveClearSky :=   ftau * IncomingShortwavePotential][]

  if (briefoutput == TRUE) {
    dtyrmon = dtyrmon[!is.an(Window) , .(year,month,IncomingShortwave,IncomingShortwavePotential,ftau,IncomingShortwaveClearSky,Window,tau)]
  }
  return(dtyrmon)
}

# (rqmw = dt30[SiteCode == "LIN" & year(Date) == 1997 , ClearSkyQuantileRegressionMonthlyTimeWindow(Date,Time,IncomingShortwave, lat = 50, lon = 11 )])
# (rqmw = dt30[SiteCode == "LIN" & year(Date) == 1997 , ClearSkyQuantileRegressionMonthlyTimeWindow(Date,Time,IncomingShortwave, lat = 50, lon = 11, briefoutput = F )])


### perform yrmon averging
#### AGGREGATE TO MONTHLYdiurnal cylce with 26 days required and then average the monthly mean diurnal cycle ####
aggregate2yrmon = function(dth = NULL, Date = NULL, Time = NULL, IncomingShortwave = NULL, ..., nminmon = 26) {
  #' Aggregate sub-daily data first to a monthly mean diurnal cylce and then to monthly average
  #'
  #' The aggregation tries to avoid sampling biases (e.g. missing data at night time)
  #' with 26 days required and then average the monthly mean diurnal cycle
  #'
  #' @param dth a data.table with column names Date, Time; if not supplied it will be constructed from vectors
  #' @param Date a vector of Dates in the format of data.table::as.IDate
  #' @param Time a vector of Times in the format of data.table::as.ITime
  #' @param IncomingShortwave a vector of subdaily (hourly or half-hourly) observations of solar radiation
  #' @param nminmon numeric, sets the required number of existing days for averaging
  #' @param ... further vectors which shall be aggregated
  #' @return data.table with aggregated data per year and month
  #' @details Provide either a data table dth or vectors of Date, Time, IncomingShortwave
  #' @author Maik Renner, mrenner [at] bgc-jena.mpg.de
  #'
  if (is.null(dth)) dth = data.table(Date,Time,IncomingShortwave, ...)
  dtyrmondiurnal = dth[ , lapply(.SD, meann, nmin = 26, na.rm = TRUE),  by = list(year(Date), month(Date), Time)]
  dtyrmon = dtyrmondiurnal[ , lapply(.SD, mean, na.rm = FALSE),  by = list(year, month)][ , Time := NULL][]
  return(dtyrmon)
}

# dt30[SiteCode == "LIN" & year(Date) == 1997 , Aggregate2yrmon(dth = NULL, Date,Time,IncomingShortwave,IncomingLongwave)]


calc_ClearSkyQuantileRegressionMonthly = function(Date, Time, IncomingShortwave, IncomingShortwavePotential = NULL, tau = tau,
                                             lat = NULL, lon = NULL, hourshift = 0.25, TimeZone = 0, isCorrectSolartime = TRUE, cosineResponsePower = 1.2)
{

  if( is.null(IncomingShortwavePotential) | length(IncomingShortwavePotential) != length(IncomingShortwave)) {

    IncomingShortwavePotential = calc_PotRadiation_CosineResponsePower(doy = yday(Date), hour = Time/3600 + hourshift,
                                                                       latDeg = lat ,
                                                                       longDeg = lon,
                                                                       timeZone = TimeZone,  isCorrectSolartime = isCorrectSolartime, cosineResponsePower = cosineResponsePower )

  }

  rqout = calc_ClearSky_QuantileRegression(IncomingShortwave,IncomingShortwavePotential,tau = tau)
  return(rqout)
}

# dt30[SiteCode == "LIN" & year(Date) == 1997 , ClearSkyQuantileRegressionMonthly(Date,Time,IncomingShortwave, lat = 50, lon = 11 )]

