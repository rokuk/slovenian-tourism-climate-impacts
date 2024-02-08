ARSO-CIT-HCI
================

This code calculates CIT and HCI from weather station data from ARSO and
plots it.

## Libraries and functions

``` r
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(purrr)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
options(dplyr.summarise.inform=F)
```

Define constants:

``` r
periods <- c("1971-2000", "1981-2010", "1991-2020", "1986-2005")
month_names <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
station_names <- c("Rateče", "Bilje", "Portorož", "Ljubljana - Bežigrad", "Kočevje", "Novo mesto", "Celje", "Šmartno pri Slovenj Gradcu", "Maribor", "Murska Sobota - Rakičan", "Cerklje ob Krki")
cit_types <- c("3S", "football", "motor boating", "cycling", "golf", "sailing", "cultural & hiking")
h = 0.008 # thickness of clothing [cm]
M = 25 # metabolic rate [cal/s]
A = 0.45 # albedo
```

ASHRAE scale from skin temperature. See
[CDS](https://cds.climate.copernicus.eu/cdsapp#!/dataset/sis-tourism-climate-suitability-indicators?tab=doc).

``` r
ashrae_scale <- function(skintemp, ...) { # scores have been shifted to go from 0 to 9
    if (skintemp > 35.5) {
        return (9)
    } else if (skintemp > 34.5 & skintemp <= 35.5) {
        return (8)
    } else if (skintemp > 33.5 & skintemp <= 34.5) {
        return (7)
    } else if (skintemp > 32.5 & skintemp <= 33.5) {
        return (6)
    } else if (skintemp > 31.0 & skintemp <= 32.5) {
        return (5)
    } else if (skintemp > 29.0 & skintemp <= 31.0) {
        return (4)
    } else if (skintemp > 26.0 & skintemp <= 29.0) {
        return (3)
    } else if (skintemp > 21.0 & skintemp <= 26.0) {
        return (2)
    } else if (skintemp <= 21.0) {
        return (1)
    } else {
        return (9999)
    }
}
```

CIT 3S lookup table. See
[CDS](https://cds.climate.copernicus.eu/cdsapp#!/dataset/sis-tourism-climate-suitability-indicators?tab=doc)
and (De Freitas et al., 2008).

``` r
# list of cit values for each limiting condition (cloud cover, rain, wind) and ashrae value, see CDS docs
cloudlesslist_sss <- c(0, 0, 0, 4, 5, 6, 7, 6, 4)
cloudmorelist_sss <- c(0, 0, 0, 3, 3, 4, 5, 5, 3)
rainlist_sss <- c(0, 0, 0, 1, 1, 1, 2, 2, 2)
windlist_sss <- c(0, 0, 0, 2, 2, 4, 4, 4, 3)

cittable_sss <- function(ashrae, cloudcover, precip, wind) {
    if (precip > 3) {
        return (rainlist_sss[ashrae]) # rain overrides all other facets
    }
    
    if (wind >= 6) {
        return (windlist_sss[ashrae]) # wind overrides cloud cover
    }
    
    if (cloudcover <= 50) {
        return (cloudlesslist_sss[ashrae])
    } else {
        return (cloudmorelist_sss[ashrae])
    }
}
```

Other CIT lookup tables. For cloud cover a border of 50 % is used. See
(Bafaluy et al., 2013).

``` r
# list of cit values for each limiting condition (cloud cover, rain, wind) and ashrae value, see CDS docs
cloud_football <- c(3, 4, 5, 6, 7, 7, 6, 4, 3)
rainlist_football <- c(1, 1, 2, 3, 4, 5, 4, 3, 2)
windlist_football <- c(1, 1, 1, 2, 3, 4, 3, 2, 2)

cittable_football <- function(ashrae, cloudcover, precip, wind) {
    if (precip > 10) {
        return (rainlist_football[ashrae]) # rain overrides all other facets
    }
    
    if (wind >= 8) {
        return (windlist_football[ashrae]) # wind overrides cloud cover
    }
    
    return (cloud_football[ashrae])
}

cloudlesslist_motorboat <- c(2, 3, 4, 5, 6, 7, 7, 6, 5)
cloudmorelist_motorboat <- c(1, 2, 3, 4, 5, 6, 6, 5, 4)
rainlist_motorboat <- c(1, 1, 2, 2, 4, 4, 4, 3, 3)
windlist_motorboat <- c(1, 1, 1, 2, 2, 3, 2, 2, 1)

cittable_motorboat <- function(ashrae, cloudcover, precip, wind) {
    if (precip > 20) {
        return (rainlist_motorboat[ashrae]) # rain overrides all other facets
    }
    
    if (wind >= 15) {
        return (windlist_motorboat[ashrae]) # wind overrides cloud cover
    }
    
    if (cloudcover <= 50) {
        return (cloudlesslist_motorboat[ashrae])
    } else {
        return (cloudmorelist_motorboat[ashrae])
    }
}

cloudlesslist_cycle <- c(3, 4, 5, 6, 7, 7, 6, 4, 3)
cloudmorelist_cycle <- c(2, 3, 4, 5, 6, 7, 5, 3, 2)
rainlist_cycle <- c(1, 2, 3, 3, 4, 4, 4, 3, 3)
windlist_cycle <- c(1, 1, 1, 2, 2, 3, 2, 2, 2)

cittable_cycle <- function(ashrae, cloudcover, precip, wind) {
    if (precip > 10) {
        return (rainlist_cycle[ashrae]) # rain overrides all other facets
    }
    
    if (wind >= 8) {
        return (windlist_cycle[ashrae]) # wind overrides cloud cover
    }
    
    if (cloudcover <= 50) {
        return (cloudlesslist_cycle[ashrae])
    } else {
        return (cloudmorelist_cycle[ashrae])
    }
}

cloudlesslist_golf <- c(3, 4, 5, 6, 7, 7, 6, 5, 3)
cloudmorelist_golf <- c(2, 3, 4, 5, 6, 6, 5, 4, 2)
rainlist_golf <- c(1, 2, 3, 3, 4, 4, 4, 3, 3)
windlist_golf <- c(1, 1, 2, 2, 3, 2, 2, 1, 1)

cittable_golf <- function(ashrae, cloudcover, precip, wind) {
    if (precip > 10) {
        return (rainlist_golf[ashrae]) # rain overrides all other facets
    }
    
    if (wind >= 4) {
        return (windlist_golf[ashrae]) # wind overrides cloud cover
    }
    
    if (cloudcover <= 50) {
        return (cloudlesslist_golf[ashrae])
    } else {
        return (cloudmorelist_golf[ashrae])
    }
}

cloudlesslist_sail <- c(2, 3, 4, 5, 6, 7, 7, 6, 5)
cloudmorelist_sail <- c(1, 2, 3, 4, 5, 6, 6, 5, 4)
rainlist_sail <- c(1, 1, 2, 2, 4, 4, 4, 3, 3)
windlist_sail <- c(1, 1, 1, 2, 2, 3, 2, 2, 1)

cittable_sail <- function(ashrae, cloudcover, precip, wind) {
    if (precip > 20) {
        return (rainlist_sail[ashrae]) # rain overrides all other facets
    }
    
    if (wind >= 15 | wind <= 1.5) {
        return (windlist_sail[ashrae]) # wind overrides cloud cover
    }
    
    if (cloudcover <= 50) {
        return (cloudlesslist_sail[ashrae])
    } else {
        return (cloudmorelist_sail[ashrae])
    }
}

cloudlesslist_culhike <- c(3, 4, 5, 6, 7, 7, 6, 4, 3)
cloudmorelist_culhike <- c(2, 4, 4, 5, 6, 6, 5, 3, 3)
rainlist_culhike <- c(1, 2, 3, 4, 4, 4, 4, 3, 2)
windlist_culhike <- c(1, 1, 3, 4, 4, 4, 4, 3, 2)

cittable_culhike <- function(ashrae, cloudcover, precip, wind) {
    if (precip > 5) {
        return (rainlist_culhike[ashrae]) # rain overrides all other facets
    }
    
    if (wind >= 10) {
        return (windlist_culhike[ashrae]) # wind overrides cloud cover
    }
    
    if (cloudcover <= 50) {
        return (cloudlesslist_culhike[ashrae])
    } else {
        return (cloudmorelist_culhike[ashrae])
    }
}

cittable <- function(ashrae, cloudcover, precip, wind, cit_type) {
    if (cit_type == "3S") {
        return (cittable_sss(ashrae, cloudcover, precip, wind))
    } else if (cit_type == "football") {
        return (cittable_football(ashrae, cloudcover, precip, wind))
    } else if (cit_type == "motor boating") {
        return (cittable_motorboat(ashrae, cloudcover, precip, wind))
    } else if (cit_type == "cycling") {
        return (cittable_cycle(ashrae, cloudcover, precip, wind))
    } else if (cit_type == "golf") {
        return (cittable_golf(ashrae, cloudcover, precip, wind))
    } else if (cit_type == "sailing") {
        return (cittable_sail(ashrae, cloudcover, precip, wind))
    } else if (cit_type == "cultural & hiking") {
        return (cittable_culhike(ashrae, cloudcover, precip, wind))
    }
}
```

HCI effective temperature rating. See Table 3 in (Scott et al., 2016).

``` r
urban_efftemp_rating <- function(efftemp) {
    if (efftemp >= 39) {
        return (0)
    } else if (efftemp >= 37 & efftemp < 39) {
        return (2)
    } else if (efftemp >= 35 & efftemp < 37) {
        return (4)
    } else if (efftemp >= 33 & efftemp < 35) {
        return (5)
    } else if (efftemp >= 31 & efftemp < 33) {
        return (6)
    } else if (efftemp >= 29 & efftemp < 31) {
        return (7)
    } else if (efftemp >= 27 & efftemp < 29) {
        return (8)
    } else if (efftemp >= 26 & efftemp < 27) {
        return (9)
    } else if (efftemp >= 23 & efftemp < 26) {
        return (10)
    } else if (efftemp >= 20 & efftemp < 23) {
        return (9)
    } else if (efftemp >= 18 & efftemp < 20) {
        return (7)
    } else if (efftemp >= 15 & efftemp < 18) {
        return (6)
    } else if (efftemp >= 11 & efftemp < 15) {
        return (5)
    } else if (efftemp >= 7 & efftemp < 11) {
        return (4)
    } else if (efftemp >= 0 & efftemp < 7) {
        return (3)
    } else if (efftemp > -6 & efftemp < -0) {
        return (2)
    } else if (efftemp <= -6) {
        return (1)
    } else {
        return (9999)
    }
}

beach_efftemp_rating <- function(efftemp) {
        if (efftemp >= 39) {
        return (0)
    } else if (efftemp >= 38 & efftemp < 39) {
        return (2)
    } else if (efftemp >= 37 & efftemp < 38) {
        return (4)
    } else if (efftemp >= 36 & efftemp < 37) {
        return (5)
    } else if (efftemp >= 35 & efftemp < 36) {
        return (6)
    } else if (efftemp >= 34 & efftemp < 35) {
        return (7)
    } else if (efftemp >= 33 & efftemp < 34) {
        return (8)
    } else if (efftemp >= 31 & efftemp < 33) {
        return (9)
    } else if (efftemp >= 28 & efftemp < 31) {
        return (10)
    } else if (efftemp >= 26 & efftemp < 28) {
        return (9)
    } else if (efftemp >= 23 & efftemp < 26) {
        return (7)
    } else if (efftemp >= 22 & efftemp < 23) {
        return (6)
    } else if (efftemp >= 21 & efftemp < 22) {
        return (5)
    } else if (efftemp >= 20 & efftemp < 21) {
        return (4)
    } else if (efftemp >= 19 & efftemp < 20) {
        return (3)
    } else if (efftemp >= 18 & efftemp < 19) {
        return (2)
    } else if (efftemp >= 17 & efftemp < 18) {
        return (1)
    } else if (efftemp >= 15 & efftemp < 17) {
        return (0)
    } else if (efftemp >= 10 & efftemp < 15) {
        return (-5)
    } else if (efftemp < 10) {
        return (-10)
    } else {
        return (9999)
    }
}
```

HCI cloud rating. See Table 4 in (Scott et al., 2016).

``` r
urban_cloud_rating <- function(cloudcover) {
    if (cloudcover >= 11 & cloudcover <= 20) {
        return (10)
    } else if ((cloudcover >= 1 & cloudcover <= 10) | (cloudcover >= 21 & cloudcover <= 30)) {
        return (9)
    } else if ((cloudcover == 0) | (cloudcover >= 31 & cloudcover <= 40)) {
        return (8)
    } else if (cloudcover >= 41 & cloudcover <= 50) {
        return (7)
    } else if (cloudcover >= 51 & cloudcover <= 60) {
        return (6)
    } else if (cloudcover >= 61 & cloudcover <= 70) {
        return (5)
    } else if (cloudcover >= 71 & cloudcover <= 80) {
        return (4)
    } else if (cloudcover >= 81 & cloudcover <= 90) {
        return (3)
    } else if (cloudcover >= 91 & cloudcover <= 99) {
        return (2)
    } else if (cloudcover == 100) {
        return (1)
    } else {
        return (9999)
    }
}

beach_cloud_rating <- function(cloudcover) {
    if (cloudcover >= 15 & cloudcover <= 25) {
        return (10)
    } else if ((cloudcover >= 1 & cloudcover <= 14) | (cloudcover >= 26 & cloudcover <= 35)) {
        return (9)
    } else if ((cloudcover == 0) | (cloudcover >= 36 & cloudcover <= 46)) {
        return (8)
    } else if (cloudcover >= 46 & cloudcover <= 55) {
        return (7)
    } else if (cloudcover >= 56 & cloudcover <= 65) {
        return (6)
    } else if (cloudcover >= 66 & cloudcover <= 75) {
        return (5)
    } else if (cloudcover >= 76 & cloudcover <= 85) {
        return (4)
    } else if (cloudcover >= 86 & cloudcover <= 95) {
        return (3)
    } else if (cloudcover >= 96 & cloudcover <= 100) {
        return (2)
    } else {
        return (9999)
    }
}
```

HCI precipitation rating. See Table 5 in (Scott et al., 2016).

``` r
urban_precip_rating <- function(precip) {
    if (precip == 0) {
         return (10)
    } else if (precip > 0 & precip < 3) {
        return (9)
    } else if (precip >= 3 & precip < 6) {
        return (8)
    } else if (precip >= 6 & precip < 9) {
        return (5)
    } else if (precip >= 9 & precip < 12) {
        return (2)
    } else if (precip >= 12 & precip <= 25) {
        return (0)
    } else if (precip > 25) {
        return (-1)
    } else {
        return (9999)
    }
}

beach_precip_rating <- function(precip) {
    if (precip == 0) {
         return (10)
    } else if (precip > 0 & precip < 3) {
        return (9)
    } else if (precip >= 3 & precip < 6) {
        return (8)
    } else if (precip >= 6 & precip < 9) {
        return (6)
    } else if (precip >= 9 & precip < 12) {
        return (4)
    } else if (precip >= 12 & precip <= 25) {
        return (0)
    } else if (precip > 25) {
        return (-1)
    } else {
        return (9999)
    }
}
```

HCI wind rating. See Table 6 in (Scott et al., 2016).

``` r
urban_wind_rating <- function(wind) {
    wind <- wind * 3.6 # convert wind from m/s to km/h
    
    if (wind > 0 & wind < 10) {
        return (10)
    } else if (wind >= 10 & wind < 20) {
        return (9)
    } else if ((wind == 0.0) | (wind >= 20 & wind < 30)) {
        return (8)
    } else if (wind >= 30 & wind < 40) {
        return (6)
    } else if (wind >= 40 & wind < 50) {
        return (3)
    } else if (wind >= 50 & wind < 70) {
        return (0)
    } else if (wind >= 70) {
        return (-10)
    } else {
        return (9999)
    }
}

beach_wind_rating <- function(wind) {
    wind <- wind * 3.6 # convert wind from m/s to km/h
    
    if (wind > 0.5 & wind <= 10) {
        return (10)
    } else if (wind >= 10 & wind < 20) {
        return (9)
    } else if ((wind <= 0.5) | (wind >= 20 & wind < 30)) {
        return (8)
    } else if (wind >= 30 & wind < 40) {
        return (6)
    } else if (wind >= 40 & wind < 50) {
        return (3)
    } else if (wind >= 50 & wind < 70) {
        return (0)
    } else if (wind >= 70) {
        return (-10)
    } else {
        return (9999)
    }
} 
```

## Importing data

Wind is in m/s, Tmax in degC, cloud cover in %, precipitation in mm and
relative humidity in %.

``` r
import_daily_data <- function(station_name) {
    data <- read_csv(paste0("../data/arso-cit-hci/daily/", gsub(" ", "_", station_name), ".txt"), col_names=c("station_id", "station_name", "date", "Tmax", "avgwind", "cloudcover", "precip"), skip=1, col_types = c("c", "c", "D", "d", "d", "i", "d")) %>% drop_na()
    return (data)
}
```

``` r
import_RH_data <- function(station_name) {
    data <- read_csv(paste0("../data/arso-cit-hci/RH/RH-", gsub(" ", "_", station_name), ".txt"), col_names=c("station_id", "station_name", "datetime", "RH"), skip=1, col_types = c("c", "c", "D", "i")) %>% drop_na()
}
```

## Process data, calculate indexes

Prepare daily data:

``` r
prepare_daily_data <- function(rawdata) {
    prepared_data <- rawdata %>%
        mutate(precip = lead(precip)) %>% # move precip values one row higher
        mutate(month = month(ymd(date)), year = year(ymd(date))) %>% # extract month and year
        mutate(avgwind = if_else(avgwind >= 0, avgwind, -avgwind)) # replace negative wind values with positive values

    return (prepared_data[1:nrow(prepared_data)-1,]) # drop last row (with NA precip)
}
```

Prepare relative humidity data at 14.00:

``` r
prepare_RH_data <- function(rawRHdata) {
    prepared_RH_data <- rawRHdata %>%
        mutate(hour = hour(ymd_hms(datetime)),
               date = date(ymd_hms(datetime))) %>%
        filter(hour == 14) %>%
        select(-hour, -station_id, -station_name, -datetime)
    
    return (prepared_RH_data)
}
```

Calculate skin temperature, ashrae score and CIT:

``` r
calculate_cit <- function(index_data, cit_type) {
    index_data <- index_data %>%
        mutate(skintemp = Tmax + h * M / 7 + (M - 15 + 120 * (1 - cloudcover / 100) * (1 - A)) / (2 + 9 * sqrt(0.1 + avgwind))) %>% # calculate skin temperature
        rowwise() %>%
        mutate(ashrae = ashrae_scale(skintemp),
               cit = cittable(ashrae, cloudcover, precip, avgwind, cit_type)) # calculate ashrae score from skin temp, then calculate cit
    
    return(index_data)
}
```

Join relative humidity and daily datasets, calculate effective
temperature and HCI from facet scores.

``` r
calculate_hci_urban <- function(RH_data, daily_data) {
    index_data <- inner_join(RH_data, daily_data, by="date") # join daily and RH dataframes, drop any rows with NA
    
    index_data <- index_data %>%
        mutate(efftemp = Tmax - 0.4 * (Tmax - 10) * (1 - RH / 100)) %>%
        rowwise() %>%
        mutate(hci = 4 * urban_efftemp_rating(efftemp) + 2 * urban_cloud_rating(cloudcover) + 3 * urban_precip_rating(precip) + urban_wind_rating(avgwind)) # calculate effective temperature, then calculate hci
    
    return (index_data)
}

calculate_hci_beach <- function(RH_data, daily_data) {
    index_data <- inner_join(RH_data, daily_data, by="date") # join daily and RH dataframes, drop any rows with NA
    
    index_data <- index_data %>%
        mutate(efftemp = Tmax - 0.4 * (Tmax - 10) * (1 - RH / 100)) %>%
        rowwise() %>%
        mutate(hci = 2 * beach_efftemp_rating(efftemp) + 4 * beach_cloud_rating(cloudcover) + 3 * beach_precip_rating(precip) + beach_wind_rating(avgwind)) # calculate effective temperature, then calculate hci
    
    return (index_data)
}
```

## Aggregating data

Split data into periods and join them with relevant columns, add period
column:

``` r
assemble_cit_data <- function(index_data) {
    period_data <- data.frame(matrix(nrow=0, ncol=4))

    period1 <- filter(index_data, year >= 1971 & year <= 2000)
    period2 <- filter(index_data, year >= 1981 & year <= 2010)
    period3 <- filter(index_data, year >= 1991 & year <= 2020)
    
    periodlist <- list(period1, period2, period3)

    for (i in 1:length(periodlist)) {
        period_data <- rbind(period_data, data.frame(
            station_name = periodlist[[i]]$station_name,
            period = rep(periods[i], nrow(periodlist[[i]])),
            year = periodlist[[i]]$year,
            month = month_names[periodlist[[i]]$month],
            cit = periodlist[[i]]$cit
        ))
    }
    
    return (period_data)
}
```

``` r
assemble_hci_data <- function(index_data) {
    period_data <- data.frame(matrix(nrow=0, ncol=4))

    period1 <- filter(index_data, year >= 1971 & year <= 2000)
    period2 <- filter(index_data, year >= 1981 & year <= 2010)
    period3 <- filter(index_data, year >= 1991 & year <= 2020)
    
    periodlist <- list(period1, period2, period3)

    for (i in 1:length(periodlist)) {
        period_data <- rbind(period_data, data.frame(
            station_name = periodlist[[i]]$station_name,
            period = rep(periods[i], nrow(periodlist[[i]])),
            year = periodlist[[i]]$year,
            month = month_names[periodlist[[i]]$month],
            hci = periodlist[[i]]$hci,
            efftemp = periodlist[[i]]$efftemp
        ))
    }
    
    return (period_data)
}
```

Calculate number of days in each CIT category, set period, category and
month names:

``` r
aggregate_cit_data <- function(period_data) {
    agg_data <- period_data %>%
        mutate(
            cit_category = case_when(
                cit >= 5 ~ "ideal",
                cit == 4 ~ "marginal",
                cit <= 3 ~ "very poor"
            )
        ) %>%
        group_by(period, month, cit_category) %>%
        summarise(count = n()) %>%
        ungroup() %>%
        group_by(period, month) %>%
        mutate(percentage = count / sum(count))

    agg_data$period <- factor(agg_data$period, levels=periods)
    agg_data$cit_category <- factor(agg_data$cit_category, levels=c("very poor", "marginal", "ideal"))
    agg_data$month <- factor(agg_data$month, levels=month_names)
    
    return(agg_data)
}
```

Calculate number of days in each HCI category, set period, category and
month names:

``` r
aggregate_hci_data <- function(period_data) {
    agg_data <- period_data %>%
        mutate(
            hci_category = case_when(
                hci >= 70 ~ "ideal",
                hci >= 50 & hci < 70 ~ "marginal",
                hci < 50 ~ "very poor"
            )
        ) %>%
        group_by(period, month, hci_category) %>%
        summarise(count = n()) %>%
        ungroup() %>%
        group_by(period, month) %>%
        mutate(percentage = count / sum(count))

    agg_data$period <- factor(agg_data$period, levels=periods)
    agg_data$hci_category <- factor(agg_data$hci_category, levels=c("very poor", "marginal", "ideal"))
    agg_data$month <- factor(agg_data$month, levels=month_names)
    
    return(agg_data)
}
```

Calculate mean effective temperature for each period and month, set
period and month names:

``` r
aggregate_efftemp_data <- function(period_data) {
    agg_data <- period_data %>%
        group_by(period, month) %>%
        summarise(avgefftemp = mean(efftemp))
    
    agg_data$period <- factor(agg_data$period, levels=periods)
    agg_data$month <- factor(agg_data$month, levels=month_names)
    
    return (agg_data)
}
```

## Missing data

Report how much data is missing:

``` r
check_data <- function() {
    TmaxNAs <- c()
    avgwindNAs <- c()
    cloudcoverNAs <- c()
    precipNAs <- c()
    RHNAs <- c()
    nrows <- c()
    
    for (station_name in station_names) {
        daily_data <- read_csv(paste0("../data/arso-cit-hci/daily/", gsub(" ", "_", station_name), ".txt"), col_names=c("station_id", "station_name", "date", "Tmax", "avgwind", "cloudcover", "precip"), skip=1, col_types = c("c", "c", "D", "d", "d", "i", "d"))
        TmaxNAs <- rbind(TmaxNAs, sum(is.na(daily_data$Tmax)))
        avgwindNAs <- rbind(avgwindNAs, sum(is.na(daily_data$avgwind)))
        cloudcoverNAs <- rbind(cloudcoverNAs, sum(is.na(daily_data$cloudcover)))
        precipNAs <- rbind(precipNAs, sum(is.na(daily_data$precip)))
        
        RH_data <- read_csv(paste0("../data/arso-cit-hci/RH/RH-", gsub(" ", "_", station_name), ".txt"), col_names=c("station_id", "station_name", "datetime", "RH"), skip=1, col_types = c("c", "c", "D", "i"))
        
        RHNAs <- rbind(RHNAs, sum(is.na(RH_data$RH)))
        nrows <- rbind(nrows, nrow(RH_data))
    }
    
    NAs <- data.frame(station_names, nrows, TmaxNAs, avgwindNAs, cloudcoverNAs, precipNAs, RHNAs)
    print(NAs)
}
check_data()
```

    ##                 station_names nrows TmaxNAs avgwindNAs cloudcoverNAs precipNAs
    ## 1                      Rateče 54792       0          3          1281         0
    ## 2                       Bilje 54795       4         41            33         0
    ## 3                    Portorož 55248       0          0             0         0
    ## 4        Ljubljana - Bežigrad 55887       0          0             0         0
    ## 5                     Kočevje 54592      19         35            35         0
    ## 6                  Novo mesto 54792       5         23            13         0
    ## 7                       Celje 54960       0          5          1367         0
    ## 8  Šmartno pri Slovenj Gradcu 54792       0         26            43         1
    ## 9                     Maribor 54792      17         17             5         0
    ## 10    Murska Sobota - Rakičan 54790       1         71            46         1
    ## 11            Cerklje ob Krki 42282       0          0           335         0
    ##    RHNAs
    ## 1      0
    ## 2     42
    ## 3      1
    ## 4      0
    ## 5     82
    ## 6      6
    ## 7      0
    ## 8      9
    ## 9     59
    ## 10    25
    ## 11     0

## Plot functions

A function to plot aggregated CIT data for one station:

``` r
plot_cit_data <- function(agg_data, station_name, cit_type) {
    p <- ggplot(data=agg_data,
                mapping=aes(x=period, y=percentage, fill=cit_category)) +
        geom_col() +
        facet_grid(~ month) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.02)), 
                           labels = scales::percent_format(accuracy = 1),
                           breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
        guides(x = guide_axis(angle = 90)) +
        labs(title=station_name, subtitle = paste("ARSO - CIT:", cit_type), fill="CIT category") +
        xlab("period") +
        ylab("percentage of days per month") +
        scale_fill_manual(values = c("#C04330", "#EAAA00", "#009E53")) +
        theme(axis.text.x = element_text(size = 10), 
              axis.text.y = element_text(size = 10),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    
    return (p)
}
```

A function to plot aggregated HCI data for one station:

``` r
plot_hci_urban <- function(agg_data, station_name) {
    p <- ggplot(data=agg_data,
                mapping=aes(x=period, y=percentage, fill=hci_category)) +
        geom_col() +
        facet_grid(~ month) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.02)), 
                           labels = scales::percent_format(accuracy = 1),
                           breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
        guides(x = guide_axis(angle = 90)) +
        labs(title=station_name, subtitle = "ARSO - HCI:Urban", fill="HCI category") +
        xlab("period") +
        ylab("percentage of days per month") + 
        scale_fill_manual(values = c("#C04330", "#EAAA00", "#009E53")) +
        theme(axis.text.x = element_text(size = 10), 
              axis.text.y = element_text(size = 10),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    
    return (p)
}
```

``` r
plot_hci_beach <- function(agg_data, station_name) {
    p <- ggplot(data=agg_data,
                mapping=aes(x=period, y=percentage, fill=hci_category)) +
        geom_col() +
        facet_grid(~ month) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.02)), 
                           labels = scales::percent_format(accuracy = 1),
                           breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
        guides(x = guide_axis(angle = 90)) +
        labs(title=station_name, subtitle = "ARSO - HCI:Beach", fill="HCI category") +
        xlab("period") +
        ylab("percentage of days per month") + 
        scale_fill_manual(values = c("#C04330", "#EAAA00", "#009E53")) +
        theme(axis.text.x = element_text(size = 10), 
              axis.text.y = element_text(size = 10),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    
    return (p)
}
```

A function to plot aggregated effective temperature data for one
station:

``` r
plot_efftemp_data <- function(agg_data, station_name) {
    p <- ggplot(data=agg_data,
                mapping=aes(x=period, y=avgefftemp, fill=period)) +
        geom_col() +
        facet_grid(~ month) +
        scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#D55E00")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.02)),
                           limits = c(0, 26)) +
        labs(title=station_name) +
        ylab("average effective temperature [°C]") + 
        theme(axis.text.y = element_text(size = 10), 
              panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank(),
              axis.title.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.x=element_blank())
    
    return (p)
}
```

## Plots

Plot CIT data for all stations:

``` r
for (station_name in station_names) {
    prepared_data <- import_daily_data(station_name) %>%
        prepare_daily_data()
    
    for (cit_type in cit_types) {
        
        if ((station_name != "Portorož" & (cit_type != "sailing" & cit_type != "motor boating")) | station_name == "Portorož") {
            
            data <- calculate_cit(prepared_data, cit_type) %>%
                assemble_cit_data() %>%
                aggregate_cit_data()
    
            p <- plot_cit_data(data, station_name, cit_type)
    
            print(p)
        }
    }
}
```

![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-1.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-2.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-3.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-4.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-5.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-6.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-7.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-8.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-9.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-10.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-11.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-12.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-13.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-14.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-15.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-16.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-17.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-18.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-19.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-20.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-21.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-22.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-23.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-24.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-25.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-26.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-27.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-28.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-29.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-30.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-31.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-32.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-33.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-34.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-35.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-36.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-37.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-38.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-39.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-40.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-41.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-42.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-43.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-44.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-45.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-46.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-47.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-48.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-49.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-50.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-51.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-52.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-53.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-54.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-55.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-56.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-26-57.svg)<!-- -->

Plot HCI data for all stations:

``` r
for (station_name in station_names) {
    data <- calculate_hci_urban(
        prepare_RH_data(
            import_RH_data(station_name)), 
        prepare_daily_data(
            import_daily_data(station_name))) %>%
        assemble_hci_data() %>%
        aggregate_hci_data()
    
    p <- plot_hci_urban(data, station_name)
    
    print(p)
}
```

![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-27-1.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-27-2.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-27-3.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-27-4.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-27-5.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-27-6.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-27-7.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-27-8.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-27-9.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-27-10.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-27-11.svg)<!-- -->

``` r
for (station_name in station_names) {
    data <- calculate_hci_beach(
        prepare_RH_data(
            import_RH_data(station_name)), 
        prepare_daily_data(
            import_daily_data(station_name))) %>%
        assemble_hci_data() %>%
        aggregate_hci_data()
    
    p <- plot_hci_beach(data, station_name)
    
    print(p)
}
```

![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-28-1.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-28-2.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-28-3.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-28-4.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-28-5.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-28-6.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-28-7.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-28-8.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-28-9.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-28-10.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-28-11.svg)<!-- -->

Plot effective temperature data for all stations:

``` r
for (station_name in station_names) {
    data <- calculate_hci_urban(
        prepare_RH_data(
            import_RH_data(station_name)), 
        prepare_daily_data(
            import_daily_data(station_name))) %>%
        assemble_hci_data() %>%
        aggregate_efftemp_data()
    
    p <- plot_efftemp_data(data, station_name)
    
    print(p)
}
```

![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-29-1.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-29-2.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-29-3.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-29-4.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-29-5.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-29-6.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-29-7.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-29-8.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-29-9.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-29-10.svg)<!-- -->![](ARSO-CIT-HCI_files/figure-gfm/unnamed-chunk-29-11.svg)<!-- -->

Save all the plots:

``` r
for (station_name in (station_names)) {
    prepared_data <- import_daily_data(station_name) %>%
        prepare_daily_data()
    
    for (cit_type in cit_types) {
        
        if ((station_name != "Portorož" & (cit_type != "sailing" & cit_type != "motor boating")) | station_name == "Portorož") {
            
            print(paste(station_name, cit_type))
            
            data <- calculate_cit(prepared_data, cit_type) %>%
                assemble_cit_data() %>%
                aggregate_cit_data()
    
            p <- plot_cit_data(data, station_name, cit_type)
    
            print(p)
   
            ggsave(paste("arso-CIT_", gsub(" ", "_", station_name), "_", gsub(" ", "_", cit_type),  ".pdf", sep=""), p, width=9, height=4, units="in", path="../output/pdf/arso-CIT", device=cairo_pdf)
            ggsave(paste("arso-CIT_", gsub(" ", "_", station_name), "_", gsub(" ", "_", cit_type), ".eps", sep=""), p, width=9, height=4, units="in", path="../output/eps/arso-CIT", device=cairo_ps)
            ggsave(paste("arso-CIT_", gsub(" ", "_", station_name), "_", gsub(" ", "_", cit_type), ".svg", sep=""), p, width=9, height=4, units="in", path="../output/svg/arso-CIT")
            ggsave(paste("arso-CIT_", gsub(" ", "_", station_name), "_", gsub(" ", "_", cit_type), ".png", sep=""), p, width=9, height=4, units="in", path="../output/png/arso-CIT", dpi=500)
        }
    }
}
```

``` r
for (station_name in (station_names)) {
    print(station_name)
    
    data <- calculate_hci_urban(
        prepare_RH_data(
            import_RH_data(station_name)), 
        prepare_daily_data(
            import_daily_data(station_name))) %>%
        assemble_hci_data() %>%
        aggregate_hci_data()
    
    p <- plot_hci_urban(data, station_name)
    
    ggsave(paste("arso-HCI-urban_", gsub(" ", "_", station_name), ".pdf", sep=""), p, width=9, height=4, units="in", path="../output/pdf/arso-HCI-urban", device=cairo_pdf)
    ggsave(paste("arso-HCI-urban_", gsub(" ", "_", station_name), ".eps", sep=""), p, width=9, height=4, units="in", path="../output/eps/arso-HCI-urban", device=cairo_ps)
    ggsave(paste("arso-HCI-urban_", gsub(" ", "_", station_name), ".svg", sep=""), p, width=9, height=4, units="in", path="../output/svg/arso-HCI-urban")
    ggsave(paste("arso-HCI-urban_", gsub(" ", "_", station_name), ".png", sep=""), p, width=9, height=4, units="in", path="../output/png/arso-HCI-urban", dpi=500)
}
```

``` r
for (station_name in (station_names)) {
    print(station_name)
    
    data <- calculate_hci_beach(
        prepare_RH_data(
            import_RH_data(station_name)), 
        prepare_daily_data(
            import_daily_data(station_name))) %>%
        assemble_hci_data() %>%
        aggregate_hci_data()
    
    p <- plot_hci_beach(data, station_name)
    
    ggsave(paste("arso-HCI-beach_", gsub(" ", "_", station_name), ".pdf", sep=""), p, width=9, height=4, units="in", path="../output/pdf/arso-HCI-beach", device=cairo_pdf)
    ggsave(paste("arso-HCI-beach_", gsub(" ", "_", station_name), ".eps", sep=""), p, width=9, height=4, units="in", path="../output/eps/arso-HCI-beach", device=cairo_ps)
    ggsave(paste("arso-HCI-beach_", gsub(" ", "_", station_name), ".svg", sep=""), p, width=9, height=4, units="in", path="../output/svg/arso-HCI-beach")
    ggsave(paste("arso-HCI-beach_", gsub(" ", "_", station_name), ".png", sep=""), p, width=9, height=4, units="in", path="../output/png/arso-HCI-beach", dpi=500)
}
```

``` r
for (station_name in (station_names)) {
    print(station_name)
    
    data <- calculate_hci_urban(
        prepare_RH_data(
            import_RH_data(station_name)), 
        prepare_daily_data(
            import_daily_data(station_name))) %>%
        assemble_hci_data() %>%
        aggregate_efftemp_data()
    
    p <- plot_efftemp_data(data, station_name)
    
    ggsave(paste("arso-ET_", gsub(" ", "_", station_name), ".pdf", sep=""), p, width=9, height=4, units="in", path="../output/pdf/arso-ET", device=cairo_pdf)
    ggsave(paste("arso-ET_", gsub(" ", "_", station_name), ".eps", sep=""), p, width=9, height=4, units="in", path="../output/eps/arso-ET", device=cairo_ps)
    ggsave(paste("arso-ET_", gsub(" ", "_", station_name), ".svg", sep=""), p, width=9, height=4, units="in", path="../output/svg/arso-ET")
    ggsave(paste("arso-ET_", gsub(" ", "_", station_name), ".png", sep=""), p, width=9, height=4, units="in", path="../output/png/arso-ET", dpi=500)
}
```
