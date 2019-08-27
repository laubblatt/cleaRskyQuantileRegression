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

## Working Example for clear sky fluxes
Load a dataset of hourly solar radiation from the site Lindenberg, Germany for the year 2006

```R
library(cleaRskyQuantileRegression)
data(LIN2006)
 ```

### Now perform regression per month 
```R
(rqmw = LIN2006[ , calc_ClearSky_QuantileRegression_MonthlyTimeWindow(Date,Time,IncomingShortwave, tau = 0.85, lat = 52.21, lon = 14.122, hourshift = 0.5,timeZone = 0)])
 ```

The result is a monthly data table of fractional shortwave transmission (ftau), the resulting mean flux of shortwave radiation without clouds (IncomingShortwaveClearSky) and the length of the sampling period (Window) which was derived internally by the goodness of fit (R1) and deviations of the monthly ftau from the site mean ftau. 
Resulting fluxes can be plotted:
```R
plot(IncomingShortwavePotential ~ month, data = rqmw, type = "l", col = 4, ylab = "Shortwave Radiation (W/m2)", ylim = c(0,500))
lines(IncomingShortwaveClearSky ~ month, data = rqmw, type = "l", col =2)
lines(IncomingShortwave ~ month, data = rqmw, type = "b")
legend("topright", c("Potential", "Clear Sky flux", "Observed at surface"), col = c(4,2,1), lty = 1)
 ```


### Working example to calculate potential surface solar radiation

```R
LIN2006[ , IncomingShortwavePotential := calc_PotRadiation_CosineResponsePower(doy = yday(Date),hour = Time/3600 + 0.5, latDeg = 52.21, longDeg = 14.122, timeZone = 0) ]
LIN2006
#plot(IncomingShortwave ~ IncomingShortwavePotential, data = LIN2006[month(Date) == 6 &  mday(Date) == 7, ], type = "l")
plot(IncomingShortwave ~ IncomingShortwavePotential, data = LIN2006[month(Date) == 6, ], type = "p")
```

