#' Extend the potential solar radiation at the surface with power law
#'
#' Attenuation of the solar beam in a curved, refractive atmosphere modelled with a simple power
#' law with an exponent to the cosine of the solar zenit angle
#' being equivalent to the sine of the solar elevation angle
#' The core set of functions is copied from the R packages
#' REddyProc::fCalcPotRadiation and solartime.
#' Since their arguments where changing I copied part of these packages
#'

#'
#' @filename PotentialRadiation.R
#' @host ~/bgc/gitbgc/cleaRskyQuantileRegression
#' @author Maik Renner, mrenner [at] bgc-jena.mpg.de
#' @version 0.1 2018-11-23
#' @version 0.2 2019-08-20 update arguments and remove dependency of REddyProc
#' @export calc_PotRadiation_CosineResponsePower
#
NULL

calc_PotRadiation_CosineResponsePower <- function(doy,
                                                  hour, latDeg, longDeg, timeZone, isCorrectSolartime = TRUE, cosineResponsePower = 1.2) {
  #' Model potential solar radiation at the surface with power law to remove non-linearities with Incoming solar
  #'
  #' Attenuation of the solar beam in a curved, refractive atmosphere modelled with a simple power
  #' law with an exponent to the cosine of the solar zenit angle
  #' The function calles other functions to calculate the solar zenith angle, the extra terrestrial
  #' radiation copied from  the R packages
  #' REddyProc::fCalcPotRadiation and solartime.
  #' Since their arguments where changing I copied part of these packages.
  #'
  #' @references Long and Ackerman 2000 JGR
  #' Renner et al., 2019 ESS
  #' @author Maik Renner, mrenner [at] bgc-jena.mpg.de
  #' @param doy value or vector of day of year
  #' @param hour value or vector of hour of day also with decimal fraction
  #' @param latDeg latitude in degrees
  #' @param latDeg longitude in degrees
  #' @param timeZone numeric, if local time is used then 0 otherwise the time must be adapted
  #' @param cosineResponsePower numeric defaults to 1.2, may be lower at high latitude sites, see Long and Ackerman 2000
  #' @return vector of potential solar radiation at the surface
  #' @examples
  #' data(LIN2003)
  #' # calculate potential surface solar radiation
  #' LIN2003[ , IncomingShortwavePotential := calc_PotRadiation_CosineResponsePower(doy = yday(Date),hour = Time/3600 + 0.25, latDeg = 52.21, longDeg = 14.122, timeZone = 0) ]
  #' LIN2003
  #' plot(IncomingShortwave ~ IncomingShortwavePotential, data = LIN2003[month(Date) == 6, ], type = "p")


    # SolElev_rad.V.n <- fCalcSunPosition(DoY.V.n, Hour.V.n, Lat_deg.n,
  #                                     Long_deg.n, TimeZone_h.n, useSolartime.b = useSolartime.b)$SolElev
  solElevrad <- SolElev_rad(doy, hour, latDeg, longDeg, timeZone, isCorrectSolartime)

  extRadiation <- fCalcExtRadiation(doy)
  # adding the power law here
  potRadiation <- ifelse(solElevrad <= 0, 0, extRadiation *
                               sin(solElevrad)^cosineResponsePower)
  attr(potRadiation, "varnames") <- "Potential Radiation"
  attr(potRadiation, "units") <- attr(extRadiation,
                                          "units")
  return(potRadiation)
}

#### functions below are copied from packages REddyProc and solartime

#' @export
fCalcExtRadiation <- function(
  ##description<<
  ## Calculate the extraterrestrial solar radiation with the
  ## eccentricity correction
  doy ##<< Data vector with day of year (DoY)
  ##author<<
  ## AMM
) {

    # Calculate extraterrestrial solar radiation after Lanini, 2010
  # (Master thesis, Bern University)
  # Fractional year in radians
  FracYearRad <- 2 * pi * (doy - 1) / 365.24

  # Total solar irradiance
  SolarIrr_Wm2.c <- 1366.1 #W / m-2

  #Eccentricity correction
  ExtRadiation.V.n <- SolarIrr_Wm2.c * (
    1.00011 + 0.034221 * cos(FracYearRad) + 0.00128 * sin(FracYearRad)
    + 0.000719 * cos(2 * FracYearRad) + 0.000077 * sin(2 * FracYearRad)
  )
  attr(ExtRadiation.V.n, 'varnames') <- 'ExtRad'
  attr(ExtRadiation.V.n, 'units') <- 'W_m-2'
  ExtRadiation.V.n
  ##value<<
  ## Data vector of extraterrestrial radiation (ExtRad, W_m-2)
}


SolElev_rad <- function(
  ### Compute the position of the sun (solar angle)
  doy	                   ##<< integer vector with day of year
  ## [DoY, 1..366], same length as Hour or length 1
  , hour		                   ##<< numeric vector with local winter time
  ## as decimal hour [0..24)
  , latDeg		                 ##<< Latitude in (decimal) degrees
  , longDeg=NA	               ##<< Longitude in (decimal) degrees
  , timeZone=NA                ##<< Time zone (in hours) ahead of UTC
  ## (Central Europe is +1)
  , isCorrectSolartime = TRUE	 ##<< by default corrects hour
  ## (given in local winter time) for latitude to solar time
  ## (where noon is exactly at 12:00). Set this to FALSE if times are
  ## specified already as solar times.
){
  # adapted from REddyProc, credits to Antje Moffat
  # and Alessandro Cescatti's C++ code
  #
  ##details<<
  ## This code assumes that Hour is given in local winter time zone.
  ## By default, it corrects by longitude to solar time (where noon
  ## is exactly at 12:00).
  ## Set argument \code{isCorrectSolartime} to FALSE to use the given
  ## local winter time instead.
  #
  ## this can throw an error when the length of longDeg and timZone differs
  # if (isCorrectSolartime & any(!is.finite(c(longDeg, timeZone)))) stop(
  #   "if isCorrectSolartime, one needs to provide finite longDeg and timeZone")
  # if (isCorrectSolartime & (any(!is.finite(longDeg)) | any(!is.finite(timeZone))) ) stop(
  #   "if isCorrectSolartime, one needs to provide finite longDeg and timeZone")

      # Fractional year in radians
  fracYearInRad <- 2 * pi * (doy - 1) / 365.24
  # Solar time, corrected for local time and equation of time
  solarTimeHour <- if (!isCorrectSolartime ) hour else {
    hour + computeSolarToLocalTimeDifference(
      longDeg, timeZone, fracYearInRad = fracYearInRad)
  }
  # Conversion to radians
  # with correction for solar time < -pi to positive, important
  # for SolAzim_rad.V.n below
  SolTimeRad <- {
    SolTimeRad0 <- (solarTimeHour - 12) * pi / 12.0
    ifelse(SolTimeRad0 < -pi, SolTimeRad0 + 2*pi, SolTimeRad0)
  }
  #Solar declination in radians, accounting for the earth axis tilt
  SolDeclRad <- ((0.33281 - 22.984*cos(fracYearInRad)
                  - 0.34990*cos(2*fracYearInRad) - 0.13980*cos(3*fracYearInRad)
                  + 3.7872*sin(fracYearInRad) + 0.03205*sin(2*fracYearInRad)
                  + 0.07187*sin(3*fracYearInRad))/180*pi )
  # Solar elevation (vertical, zenithal angle) in radians with zero for horizon
  SolElevRad <-  asin( sin(SolDeclRad) * sin(latDeg/180*pi)
                       + cos(SolDeclRad) * cos(latDeg/180*pi) * cos(SolTimeRad))

  return(SolElevRad)
}



#' @export
computeSolarToLocalTimeDifference <- function(
  ### computes the time difference in hours between (apparent) solar time and local time
  longDeg		                  ##<< Longitude in (decimal) degrees
  , timeZone	              ##<< Time zone (in hours) ahead of UTC (Berlin is +1)
  , doy = integer(0)	##<< integer vector with day of year [DoY, 1..366],
  ## Specify NA get mean solar time across the year instead of apparent solar
  ## time (i.e. with differences throughout the year due to eccentricity
  ## of earth orbit)
  , fracYearInRad = 2 * pi * (doy - 1)/365.24 ##<< may specify instead
  ## of doy for efficiency.
){
  # convert solar time to local winter time
  # Equation of time in hours, accounting for changes in the time of solar noon
  # to local time zone
  eqTimeHour <- if (!length(fracYearInRad) || is.na((fracYearInRad))) 0 else
    (0.0072*cos(fracYearInRad) - 0.0528*cos(2*fracYearInRad)
     - 0.0012*cos(3*fracYearInRad) - 0.1229*sin(fracYearInRad)
     - 0.1565*sin(2*fracYearInRad) - 0.0041*sin(3*fracYearInRad))
  # Local time in hours
  localTimeHour <- (longDeg/15 - timeZone)
  ##value<< time difference in hours to be added to local winter time
  ## to get solar time
  localTimeHour + eqTimeHour
}
attr(computeSolarToLocalTimeDifference,"ex") <- function(){
  # Jena: 50.927222, 11.586111
  longDeg <- 11.586
  doi <- 1:366
  # due to longitude: west of timezone meridian: sun culminates later,
  # solar time is less than local time
  (localDiff <- computeSolarToLocalTimeDifference(longDeg, 1L)*60)
  # taking into account shift during the year due to earth orbit eccentricity
  plot( computeSolarToLocalTimeDifference(longDeg, 1L, doi)*60 ~ doi )
  abline(h = localDiff)
}

