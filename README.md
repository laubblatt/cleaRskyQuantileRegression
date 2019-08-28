# R package cleaRskyQuantileRegression

**_By Maik Renner, Max-Planck Institute for Biogeochemistry, Jena, Germany_**
> 2019-08-26

This package provides the functions to calculate 
    the fractional clear-sky shortwave transmission and
    shortwave clear-sky fluxes at the surface
    using (half-)hourly global radiation records. 
    The method is validated and described in 
    
    Renner, M., M. Wild, M. Schwarz, and A. Kleidon.
    **_Estimating Shortwave Clear-Sky Fluxes from Hourly Global
    Radiation Records by Quantile Regression._**
    Earth and Space Science, 2019.
    https://doi.org/10.1029/2019EA000686. 
    
Main functionality is to perform a Quantile Regression on data of observed global radiation (total incoming shortwave radiation) records with potential solar radiation as a function of location and date and time. The package also provides a function to perfrom the regression for different time windows, which can be required when there is persistent cloud cover. 
The derived fractional transmission of clear-sky solar radiation can be used to calculate the corresponding clear sky fluxes and the shortwave cloud radiative effect. 
The method is intended for observational sites where no data on diffuse and direct radiation is available. The method only requires global radiation in hourly or half-hourly resolution. 

The package also provide functions to calculate the potential shortwave radiation at the surface, i.e without any clouds and aerosols. Therefore Location (lat,lon) and Date and Time are required. The function is based on an code of the oR packages REddyProc and solartime maintained by Thomas Wutzler (MPI-BGC)


## Instructions    
### To install the R package use:
```R
library(devtools)
install_github("laubblatt/cleaRskyQuantileRegression")
 ```

## Working Example and illustration of the method
Load a dataset of half-hourly solar radiation from the site Lindenberg, Germany for the year 2003

```R
library(cleaRskyQuantileRegression)
data(LIN2003)
 ```

Extract data for August 2003
```R
(dat = LIN2003[year(Date) == 2003 & month(Date) == 8, ])
```

### Calculate potential surface solar radiation

```R
dat[ , Rsdpot_12 := calc_PotRadiation_CosineResponsePower(doy = yday(Date), hour = Time/3600 + 0.25,
                                                          latDeg = 52.21 ,
                                                          longDeg = 14.122,
                                                          timeZone = 0, isCorrectSolartime = TRUE,
                                                          cosineResponsePower = 1.2 )]

```

### Apply Qunatile regression and plot for one month 
```R
library(lattice)
lab_Rsdpot12_name = expression(bold("Potential Shortwave Radiation ")(W*m^-2))
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
```R




## Perform regression for all month 
```R
(rqmw = LIN2003[ , calc_ClearSky_QuantileRegression_MonthlyTimeWindow(Date,Time,IncomingShortwave, tau = 0.85, lat = 52.21, lon = 14.122, hourshift = 0.5,timeZone = 0)])
 ```

The result is a monthly data table of fractional shortwave transmission (ftau), the resulting mean flux of shortwave radiation without clouds (IncomingShortwaveClearSky) and the length of the sampling period (Window) which was derived internally by the goodness of fit (R1) and deviations of the monthly ftau from the site mean ftau. 
Resulting fluxes can be plotted:
```R
plot(IncomingShortwavePotential ~ month, data = rqmw, type = "l", col = 4, ylab = "Monthly mean Shortwave Radiation (W/m2)", ylim = c(0,500))
lines(IncomingShortwaveClearSky ~ month, data = rqmw, type = "l", col =2)
lines(IncomingShortwave ~ month, data = rqmw, type = "b")
legend("topright", c("Potential", "Clear Sky flux", "Observed at surface"), col = c(4,2,1), lty = 1)
 ```


