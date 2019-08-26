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

