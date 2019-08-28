#' Half-Hourly Global Radiation Record of the BSRN site Lindenberg, Germany (LIN) for 2003
#'
#' Obtained from the BSRN archive https://bsrn.awi.de
#'
#' @docType data
#'
#' @usage data(cleaRskyQuantileRegression)
#'
#' @keywords datasets
#'
#' @source \href{https://bsrn.awi.de}
#'
#' @examples
#' data(LIN2003)
# (dat = LIN2003[year(Date) == 2003 & month(Date) == 8, ])
# # Calc Potential Radiation
# dat[ , Rsdpot_12 := calc_PotRadiation_CosineResponsePower(doy = yday(Date), hour = Time/3600 + 0.25,
#                                                           latDeg = 52.21 ,
#                                                           longDeg = 14.122,
#                                                           timeZone = 0, isCorrectSolartime = TRUE,
#                                                           cosineResponsePower = 1.2 )]
# # Plot for one month
# plot(IncomingShortwave ~  Rsdpot_12, data = dat,
#      xlab = expression(bold("Potential Shortwave Radiation ")(W*m^-2)), ylab = expression(bold("Observed Shortwave Radiation ")(W*m^-2)) )

