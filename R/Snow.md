Snow
================

This code reads data from netcdf files (downloaded from
[CDS](https://cds.climate.copernicus.eu/cdsapp#!/dataset/sis-tourism-snow-indicators?tab=overview))
and makes plots for selected ski resorts.

## Importing libraries and data

``` r
library(ncdf4)
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
library(readr)
library(forcats)
library(ggplot2)
library(sf)
```

    ## Linking to GEOS 3.9.1, GDAL 3.4.0, PROJ 8.1.1; sf_use_s2() is TRUE

``` r
library(rnaturalearth)
library(ggrepel)
library(ggspatial)
```

The dataset provides snow indicators at the scale of NUTS-3 regions.
Read data from NUTS-3 geopackage (borders between regions). The file was
downloaded from auxillary files at
[CDS](https://cds.climate.copernicus.eu/cdsapp#!/dataset/sis-tourism-snow-indicators?tab=doc).

``` r
nuts3_mapdata <- st_read("geodata/NUTS3_ID.gpkg")
```

    ## Reading layer `NUTS3_ID' from data source 
    ##   `/Users/rokuk/Documents/Work/BFUL/Turizem-projekcije/R/geodata/NUTS3_ID.gpkg' 
    ##   using driver `GPKG'
    ## Simple feature collection with 1524 features and 8 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -7030022 ymin: -2438080 xmax: 6215534 ymax: 11462690
    ## Projected CRS: WGS 84 / Pseudo-Mercator

Select Slovenian NUTS-3 regions and get ther ids:

``` r
slovenia_nuts3_mapdata <- filter(nuts3_mapdata, cntr_code=="SI")
slovenia_nuts3_ids <- pull(slovenia_nuts3_mapdata, fid)
slovenia_nuts3_names <- pull(slovenia_nuts3_mapdata, nuts_name)
print(slovenia_nuts3_ids)
```

    ##  [1] "SI038" "SI034" "SI035" "SI033" "SI036" "SI037" "SI041" "SI042" "SI043"
    ## [10] "SI044" "SI032" "SI031"

We define a function to read data from netcdf files. The function
returns a data frame with longitudes, latitudes, heights, NUTS-3 region
id and value of `datavarname` variable at each point.

``` r
readcdf <- function(filepath, datavarname) {
    nc_data <- nc_open(filepath)

    lon <- ncvar_get(nc_data, "LON")
    lat <- ncvar_get(nc_data, "LAT")
    datapoint <- ncvar_get(nc_data, datavarname)
    height <- ncvar_get(nc_data, "ZS")
    nuts3id <- ncvar_get(nc_data, "NUTS3_ID")
    
    nc_close(nc_data)
    
    df <- data.frame(
        lon = lon, 
        lat = lat, 
        height = height, 
        nuts3id = nuts3id, 
        datapoint=datapoint
    )
    
    return (df)
}
```

Read data from one netcdf file and select datapoints in Slovenian
regions:

``` r
dataset <- readcdf("../data/snow/historical/sd-days-5-NS_HISTORICAL_Mean_1986-2005_v1.nc", "sd_days") %>% 
    filter(nuts3id %in% slovenia_nuts3_ids)
regions <- distinct(dataset, nuts3id, .keep_all=T) %>% 
    select(lon, lat, nuts3id) %>% 
    mutate(name=gsub("Å¡","š", slovenia_nuts3_names[match(nuts3id, slovenia_nuts3_ids)]))
print(regions)
```

    ##         lon      lat nuts3id                  name
    ## 1  14.30632 45.68736   SI038  Primorsko-notranjska
    ## 2  15.02106 45.70808   SI037 Jugovzhodna Slovenija
    ## 3  13.88285 45.62179   SI044         Obalno-kraška
    ## 4  13.77725 46.09002   SI043               Goriška
    ## 5  15.43760 45.96600   SI036              Posavska
    ## 6  14.56583 46.02554   SI041     Osrednjeslovenska
    ## 7  14.13987 46.30333   SI042             Gorenjska
    ## 8  14.97124 46.09972   SI035              Zasavska
    ## 9  15.19567 46.27018   SI034             Savinjska
    ## 10 15.75228 46.47155   SI032             Podravska
    ## 11 15.08801 46.53133   SI033               Koroška
    ## 12 16.18284 46.66385   SI031              Pomurska

Read ski resort data (heights rounded to 100 m):

``` r
skidata <- read_csv("../data/skiresorts.csv", show_col_types = F) %>%
    mutate(region_name=gsub("Å¡","š", slovenia_nuts3_names[match(nuts3id, slovenia_nuts3_ids)]))
print.data.frame(skidata)
```

    ##      ski_resort      lat      lon lowest highest nuts3id region_name
    ## 1        Cerkno 46.16251 14.05852    800    1300   SI042   Gorenjska
    ## 2         Kanin 46.35882 13.47624   1100    2300   SI043     Goriška
    ## 3 Kranjska Gora 46.48576 13.77673    800    1300   SI042   Gorenjska
    ## 4       Krvavec 46.30159 14.52695   1500    2000   SI042   Gorenjska
    ## 5       Pohorje 46.53328 15.60030    300    1300   SI032   Podravska
    ## 6         Rogla 46.45059 15.34987   1000    1500   SI032   Podravska
    ## 7     Stari Vrh 46.18306 14.19184    500    1200   SI042   Gorenjska
    ## 8         Vogel 46.26346 13.84031    500    1800   SI042   Gorenjska

Plot Slovenian NUTS3 regions (black) and ski resort locations (red):

``` r
p <- ggplot() +
    geom_sf(data=slovenia_nuts3_mapdata) +
    geom_point(data=skidata, mapping=aes(lon, lat), color="red") +
    geom_label_repel(data=skidata, mapping=aes(lon, lat, label=ski_resort), min.segment.length = 0.2, box.padding = 0.3, color="brown", nudge_y = 46.66-skidata$lat, force=2) +
    geom_point(data=regions, mapping=aes(lon, lat)) +
    geom_label(data=regions, mapping=aes(lon, lat, label=name), nudge_y = -0.055, size = 2.5) +
    coord_sf(crs = st_crs(4326)) +
    xlab("longitude") +
    ylab("latitude") +
    ggtitle("NUTS-3 regions and selected ski resorts") +
    annotation_scale(location="br", pad_y = unit(0.5, "cm")) +
    theme_light()
print(p)
```

![](Snow_files/figure-gfm/unnamed-chunk-7-1.svg)<!-- -->

``` r
ggsave("snow.pdf", p, width=6, height=4.5, units="in", path="../output/pdf/maps", device=cairo_pdf)
ggsave("snow.eps", p, width=6, height=4.5, units="in", path="../output/eps/maps", device=cairo_ps)
ggsave("snow.svg", p, width=6, height=4.5, units="in", path="../output/svg/maps")
ggsave("snow.png", p, width=6, height=4.5, units="in", path="../output/png/maps", dpi=500)
```

## Assemble data

Define variable values:

``` r
time_periods <- c("2021-2040", "2041-2060", "2081-2100")
scenarios <- c("RCP26", "RCP45", "RCP85")
metrics <- c("Q10", "Q50", "Q90")
variables <- c("sd-days-5-NS", "sd-days-30-NS", "snowfall-amount-winter", "wbt-2-hrs", "wbt-5-hrs")
# variable name used in netcdf file for each of the above variables:
datavarnames <- c("sd_days", "sd_days", "snowfall_amount", "wbt_hrs", "wbt_hrs")
# name of variables for plotting:
plotvarnames <- c("Days with at least 5 cm of natural snow from August to July", "Days with at least 30 cm of natural snow from August to July", "Snowfall precipitation from November to April", "Potential snowmaking hours in November and December (WBT <= -2\u00B0C)", "Potential snowmaking hours in November and December (WBT <= -5\u00B0C)")
# labels for y axis for each variable:
yaxislabels <- c("number of days", "number of days", "cumulative snowfall [mm]", "number of hours", "number of hours")
```

Read data from netcdf files:

``` r
alldata <- data.frame(matrix(ncol = 7, nrow = 0)) # create empty dataframe

# read and extract historical data
for (variable in variables) {
    for (metric in metrics) {
        print(paste(variable, "historical", "1986-2005", metric))
            
            # construct path of netcdf file
        filepath <- paste("../data/snow/historical/", variable, "_HISTORICAL_", metric, "_1986-2005_v1.nc", sep = "")
        datavarname <- datavarnames[match(variable, variables)]
        
        dataset <- readcdf(filepath, datavarname) %>%
            filter(nuts3id %in% slovenia_nuts3_ids)
        
        alldata <- rbind(alldata, data.frame(
            variable=variable,
            scenario="historical", 
            time_period="1986-2005", 
            metric=metric,
            nuts3id=dataset$nuts3id,
            height=dataset$height,
            datapoint=dataset$datapoint))
    }
}
```

    ## [1] "sd-days-5-NS historical 1986-2005 Q10"
    ## [1] "sd-days-5-NS historical 1986-2005 Q50"
    ## [1] "sd-days-5-NS historical 1986-2005 Q90"
    ## [1] "sd-days-30-NS historical 1986-2005 Q10"
    ## [1] "sd-days-30-NS historical 1986-2005 Q50"
    ## [1] "sd-days-30-NS historical 1986-2005 Q90"
    ## [1] "snowfall-amount-winter historical 1986-2005 Q10"
    ## [1] "snowfall-amount-winter historical 1986-2005 Q50"
    ## [1] "snowfall-amount-winter historical 1986-2005 Q90"
    ## [1] "wbt-2-hrs historical 1986-2005 Q10"
    ## [1] "wbt-2-hrs historical 1986-2005 Q50"
    ## [1] "wbt-2-hrs historical 1986-2005 Q90"
    ## [1] "wbt-5-hrs historical 1986-2005 Q10"
    ## [1] "wbt-5-hrs historical 1986-2005 Q50"
    ## [1] "wbt-5-hrs historical 1986-2005 Q90"

``` r
# read and extract RCP2.6, RCP4.5 and RCP8.5 data
for (variable in variables) {
    for (scenario in scenarios) {
        for (time_period in time_periods) {
            for (metric in metrics) {
                print(paste(variable, scenario, time_period, metric))
            
                # construct path of netcdf file
                filepath <- paste("../data/snow/", scenario, "/", variable, "_", scenario, "_", metric, "_", time_period, "_v1.nc", sep = "")
                datavarname <- datavarnames[match(variable, variables)]
                
                # read data and select only datapoints for our regions
                dataset <- readcdf(filepath, datavarname) %>%
                    filter(nuts3id %in% slovenia_nuts3_ids)
                
                alldata <- rbind(alldata, data.frame(
                    variable=variable,
                    scenario=scenario, 
                    time_period=time_period, 
                    metric=metric,
                    nuts3id=dataset$nuts3id,
                    height=dataset$height,
                    datapoint=dataset$datapoint))
            }
        }
    }
}
```

    ## [1] "sd-days-5-NS RCP26 2021-2040 Q10"
    ## [1] "sd-days-5-NS RCP26 2021-2040 Q50"
    ## [1] "sd-days-5-NS RCP26 2021-2040 Q90"
    ## [1] "sd-days-5-NS RCP26 2041-2060 Q10"
    ## [1] "sd-days-5-NS RCP26 2041-2060 Q50"
    ## [1] "sd-days-5-NS RCP26 2041-2060 Q90"
    ## [1] "sd-days-5-NS RCP26 2081-2100 Q10"
    ## [1] "sd-days-5-NS RCP26 2081-2100 Q50"
    ## [1] "sd-days-5-NS RCP26 2081-2100 Q90"
    ## [1] "sd-days-5-NS RCP45 2021-2040 Q10"
    ## [1] "sd-days-5-NS RCP45 2021-2040 Q50"
    ## [1] "sd-days-5-NS RCP45 2021-2040 Q90"
    ## [1] "sd-days-5-NS RCP45 2041-2060 Q10"
    ## [1] "sd-days-5-NS RCP45 2041-2060 Q50"
    ## [1] "sd-days-5-NS RCP45 2041-2060 Q90"
    ## [1] "sd-days-5-NS RCP45 2081-2100 Q10"
    ## [1] "sd-days-5-NS RCP45 2081-2100 Q50"
    ## [1] "sd-days-5-NS RCP45 2081-2100 Q90"
    ## [1] "sd-days-5-NS RCP85 2021-2040 Q10"
    ## [1] "sd-days-5-NS RCP85 2021-2040 Q50"
    ## [1] "sd-days-5-NS RCP85 2021-2040 Q90"
    ## [1] "sd-days-5-NS RCP85 2041-2060 Q10"
    ## [1] "sd-days-5-NS RCP85 2041-2060 Q50"
    ## [1] "sd-days-5-NS RCP85 2041-2060 Q90"
    ## [1] "sd-days-5-NS RCP85 2081-2100 Q10"
    ## [1] "sd-days-5-NS RCP85 2081-2100 Q50"
    ## [1] "sd-days-5-NS RCP85 2081-2100 Q90"
    ## [1] "sd-days-30-NS RCP26 2021-2040 Q10"
    ## [1] "sd-days-30-NS RCP26 2021-2040 Q50"
    ## [1] "sd-days-30-NS RCP26 2021-2040 Q90"
    ## [1] "sd-days-30-NS RCP26 2041-2060 Q10"
    ## [1] "sd-days-30-NS RCP26 2041-2060 Q50"
    ## [1] "sd-days-30-NS RCP26 2041-2060 Q90"
    ## [1] "sd-days-30-NS RCP26 2081-2100 Q10"
    ## [1] "sd-days-30-NS RCP26 2081-2100 Q50"
    ## [1] "sd-days-30-NS RCP26 2081-2100 Q90"
    ## [1] "sd-days-30-NS RCP45 2021-2040 Q10"
    ## [1] "sd-days-30-NS RCP45 2021-2040 Q50"
    ## [1] "sd-days-30-NS RCP45 2021-2040 Q90"
    ## [1] "sd-days-30-NS RCP45 2041-2060 Q10"
    ## [1] "sd-days-30-NS RCP45 2041-2060 Q50"
    ## [1] "sd-days-30-NS RCP45 2041-2060 Q90"
    ## [1] "sd-days-30-NS RCP45 2081-2100 Q10"
    ## [1] "sd-days-30-NS RCP45 2081-2100 Q50"
    ## [1] "sd-days-30-NS RCP45 2081-2100 Q90"
    ## [1] "sd-days-30-NS RCP85 2021-2040 Q10"
    ## [1] "sd-days-30-NS RCP85 2021-2040 Q50"
    ## [1] "sd-days-30-NS RCP85 2021-2040 Q90"
    ## [1] "sd-days-30-NS RCP85 2041-2060 Q10"
    ## [1] "sd-days-30-NS RCP85 2041-2060 Q50"
    ## [1] "sd-days-30-NS RCP85 2041-2060 Q90"
    ## [1] "sd-days-30-NS RCP85 2081-2100 Q10"
    ## [1] "sd-days-30-NS RCP85 2081-2100 Q50"
    ## [1] "sd-days-30-NS RCP85 2081-2100 Q90"
    ## [1] "snowfall-amount-winter RCP26 2021-2040 Q10"
    ## [1] "snowfall-amount-winter RCP26 2021-2040 Q50"
    ## [1] "snowfall-amount-winter RCP26 2021-2040 Q90"
    ## [1] "snowfall-amount-winter RCP26 2041-2060 Q10"
    ## [1] "snowfall-amount-winter RCP26 2041-2060 Q50"
    ## [1] "snowfall-amount-winter RCP26 2041-2060 Q90"
    ## [1] "snowfall-amount-winter RCP26 2081-2100 Q10"
    ## [1] "snowfall-amount-winter RCP26 2081-2100 Q50"
    ## [1] "snowfall-amount-winter RCP26 2081-2100 Q90"
    ## [1] "snowfall-amount-winter RCP45 2021-2040 Q10"
    ## [1] "snowfall-amount-winter RCP45 2021-2040 Q50"
    ## [1] "snowfall-amount-winter RCP45 2021-2040 Q90"
    ## [1] "snowfall-amount-winter RCP45 2041-2060 Q10"
    ## [1] "snowfall-amount-winter RCP45 2041-2060 Q50"
    ## [1] "snowfall-amount-winter RCP45 2041-2060 Q90"
    ## [1] "snowfall-amount-winter RCP45 2081-2100 Q10"
    ## [1] "snowfall-amount-winter RCP45 2081-2100 Q50"
    ## [1] "snowfall-amount-winter RCP45 2081-2100 Q90"
    ## [1] "snowfall-amount-winter RCP85 2021-2040 Q10"
    ## [1] "snowfall-amount-winter RCP85 2021-2040 Q50"
    ## [1] "snowfall-amount-winter RCP85 2021-2040 Q90"
    ## [1] "snowfall-amount-winter RCP85 2041-2060 Q10"
    ## [1] "snowfall-amount-winter RCP85 2041-2060 Q50"
    ## [1] "snowfall-amount-winter RCP85 2041-2060 Q90"
    ## [1] "snowfall-amount-winter RCP85 2081-2100 Q10"
    ## [1] "snowfall-amount-winter RCP85 2081-2100 Q50"
    ## [1] "snowfall-amount-winter RCP85 2081-2100 Q90"
    ## [1] "wbt-2-hrs RCP26 2021-2040 Q10"
    ## [1] "wbt-2-hrs RCP26 2021-2040 Q50"
    ## [1] "wbt-2-hrs RCP26 2021-2040 Q90"
    ## [1] "wbt-2-hrs RCP26 2041-2060 Q10"
    ## [1] "wbt-2-hrs RCP26 2041-2060 Q50"
    ## [1] "wbt-2-hrs RCP26 2041-2060 Q90"
    ## [1] "wbt-2-hrs RCP26 2081-2100 Q10"
    ## [1] "wbt-2-hrs RCP26 2081-2100 Q50"
    ## [1] "wbt-2-hrs RCP26 2081-2100 Q90"
    ## [1] "wbt-2-hrs RCP45 2021-2040 Q10"
    ## [1] "wbt-2-hrs RCP45 2021-2040 Q50"
    ## [1] "wbt-2-hrs RCP45 2021-2040 Q90"
    ## [1] "wbt-2-hrs RCP45 2041-2060 Q10"
    ## [1] "wbt-2-hrs RCP45 2041-2060 Q50"
    ## [1] "wbt-2-hrs RCP45 2041-2060 Q90"
    ## [1] "wbt-2-hrs RCP45 2081-2100 Q10"
    ## [1] "wbt-2-hrs RCP45 2081-2100 Q50"
    ## [1] "wbt-2-hrs RCP45 2081-2100 Q90"
    ## [1] "wbt-2-hrs RCP85 2021-2040 Q10"
    ## [1] "wbt-2-hrs RCP85 2021-2040 Q50"
    ## [1] "wbt-2-hrs RCP85 2021-2040 Q90"
    ## [1] "wbt-2-hrs RCP85 2041-2060 Q10"
    ## [1] "wbt-2-hrs RCP85 2041-2060 Q50"
    ## [1] "wbt-2-hrs RCP85 2041-2060 Q90"
    ## [1] "wbt-2-hrs RCP85 2081-2100 Q10"
    ## [1] "wbt-2-hrs RCP85 2081-2100 Q50"
    ## [1] "wbt-2-hrs RCP85 2081-2100 Q90"
    ## [1] "wbt-5-hrs RCP26 2021-2040 Q10"
    ## [1] "wbt-5-hrs RCP26 2021-2040 Q50"
    ## [1] "wbt-5-hrs RCP26 2021-2040 Q90"
    ## [1] "wbt-5-hrs RCP26 2041-2060 Q10"
    ## [1] "wbt-5-hrs RCP26 2041-2060 Q50"
    ## [1] "wbt-5-hrs RCP26 2041-2060 Q90"
    ## [1] "wbt-5-hrs RCP26 2081-2100 Q10"
    ## [1] "wbt-5-hrs RCP26 2081-2100 Q50"
    ## [1] "wbt-5-hrs RCP26 2081-2100 Q90"
    ## [1] "wbt-5-hrs RCP45 2021-2040 Q10"
    ## [1] "wbt-5-hrs RCP45 2021-2040 Q50"
    ## [1] "wbt-5-hrs RCP45 2021-2040 Q90"
    ## [1] "wbt-5-hrs RCP45 2041-2060 Q10"
    ## [1] "wbt-5-hrs RCP45 2041-2060 Q50"
    ## [1] "wbt-5-hrs RCP45 2041-2060 Q90"
    ## [1] "wbt-5-hrs RCP45 2081-2100 Q10"
    ## [1] "wbt-5-hrs RCP45 2081-2100 Q50"
    ## [1] "wbt-5-hrs RCP45 2081-2100 Q90"
    ## [1] "wbt-5-hrs RCP85 2021-2040 Q10"
    ## [1] "wbt-5-hrs RCP85 2021-2040 Q50"
    ## [1] "wbt-5-hrs RCP85 2021-2040 Q90"
    ## [1] "wbt-5-hrs RCP85 2041-2060 Q10"
    ## [1] "wbt-5-hrs RCP85 2041-2060 Q50"
    ## [1] "wbt-5-hrs RCP85 2041-2060 Q90"
    ## [1] "wbt-5-hrs RCP85 2081-2100 Q10"
    ## [1] "wbt-5-hrs RCP85 2081-2100 Q50"
    ## [1] "wbt-5-hrs RCP85 2081-2100 Q90"

Extract data at heights corresponding to ski resorts and compute
errorbar locations:

``` r
data_at_skiresorts <- data.frame(matrix(ncol=7, nrow=0))
heights <- distinct(alldata, height) %>% pull(height)
heightlevels <- heights[order(heights)]

for (i in 1:nrow(skidata)) {
    selection <- filter(alldata, nuts3id==pull(skidata, "nuts3id")[i] & height >= pull(skidata, "lowest")[i] & height <= pull(skidata, "highest")[i])
    
    data_at_skiresorts <- rbind(data_at_skiresorts, data.frame(
        name=skidata$ski_resort[i],
        variable=selection[selection$metric=="Q50", "variable"],
        scenario=factor(selection[selection$metric=="Q50", "scenario"], levels=c("historical", "RCP26", "RCP45", "RCP85")),
        time_period=selection[selection$metric=="Q50", "time_period"],
        height=factor(selection[selection$metric=="Q50", "height"], levels=heightlevels),
        prct50=selection[selection$metric=="Q50", "datapoint"],
        lower=selection[selection$metric=="Q10", "datapoint"],
        upper=selection[selection$metric=="Q90", "datapoint"]
    ))
}
```

## Plots

``` r
plotdata <- function(scen, var, data_at_skiresorts) {
    data_plot <- data.frame(matrix(nrow=0, ncol=8))
    
    for (resortname in skidata %>% pull(ski_resort)) {
        lowest <- filter(skidata, ski_resort==resortname)$lowest
        highest <- filter(skidata, ski_resort==resortname)$highest
        
        if (highest > 1700) { # if highest point is higher than 1700 (max height in dataset), just use 1700 m
            highest <- 1700
        }
        if (resortname == "Rogla" | resortname == "Pohorje") {
            highest <- 1200
        }
        
        temp_part <- data_at_skiresorts %>% filter(name==resortname & variable==var & (scenario==scen | scenario=="historical") & (height==lowest | height==highest))
        data_plot <- rbind(data_plot, temp_part)
    }
    
    if (scen == "RCP26") {
        scen <- "RCP2.6"
    } else if (scen == "RCP45") {
        scen <- "RCP4.5"
    } else if (scen == "RCP85") {
        scen <- "RCP8.5"
    }
    
    data_plot$height <- fct_relabel(data_plot$height, ~ paste(.x, "m"))

    p <- ggplot(data_plot, aes(x = height, y = prct50, fill = time_period)) +
        geom_col(position = position_dodge2(preserve = "single"), width=0.9) +
        facet_grid(~name, scales="free") +
        scale_fill_manual(values = c("#009E73", "#E69F00", "#56B4E9", "#D55E00")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
        labs(title = plotvarnames[match(var, variables)], subtitle = scen, fill="period") +
        xlab("height") + 
        ylab(yaxislabels[match(var, variables)]) +
        guides(x = guide_axis(angle = 90)) +
        geom_errorbar(mapping = aes(ymax=upper, ymin=lower), stat="identity", size=0.3, width=0.9, position = position_dodge2(preserve = "single")) + 
        theme(panel.grid.major.x = element_blank())
        
    return (p)
}
```

``` r
for (var in variables) {
    for (scen in scenarios) {
        p <- plotdata(scen, var, data_at_skiresorts)
        print(p)
    }
}
```

![](Snow_files/figure-gfm/unnamed-chunk-13-1.svg)<!-- -->![](Snow_files/figure-gfm/unnamed-chunk-13-2.svg)<!-- -->![](Snow_files/figure-gfm/unnamed-chunk-13-3.svg)<!-- -->![](Snow_files/figure-gfm/unnamed-chunk-13-4.svg)<!-- -->![](Snow_files/figure-gfm/unnamed-chunk-13-5.svg)<!-- -->![](Snow_files/figure-gfm/unnamed-chunk-13-6.svg)<!-- -->![](Snow_files/figure-gfm/unnamed-chunk-13-7.svg)<!-- -->![](Snow_files/figure-gfm/unnamed-chunk-13-8.svg)<!-- -->![](Snow_files/figure-gfm/unnamed-chunk-13-9.svg)<!-- -->![](Snow_files/figure-gfm/unnamed-chunk-13-10.svg)<!-- -->![](Snow_files/figure-gfm/unnamed-chunk-13-11.svg)<!-- -->![](Snow_files/figure-gfm/unnamed-chunk-13-12.svg)<!-- -->![](Snow_files/figure-gfm/unnamed-chunk-13-13.svg)<!-- -->![](Snow_files/figure-gfm/unnamed-chunk-13-14.svg)<!-- -->![](Snow_files/figure-gfm/unnamed-chunk-13-15.svg)<!-- -->

Save all the plots:

``` r
for (scen in scenarios) {
    for (var in variables) {
        print(paste(scen, var))

        p <- plotdata(scen, var, data_at_skiresorts)
        
        ggsave(paste("snow_", scen, "_", var, ".pdf", sep=""), p, width=9, height=4, units="in", path="../output/pdf/snow", device=cairo_pdf)
        ggsave(paste("snow_", scen, "_", var, ".eps", sep=""), p, width=9, height=4, units="in", path="../output/eps/snow", device=cairo_ps)
        ggsave(paste("snow_", scen, "_", var, ".svg", sep=""), p, width=9, height=4, units="in", path="../output/svg/snow")
        ggsave(paste("snow_", scen, "_", var, ".png", sep=""), p, width=9, height=4, units="in", path="../output/png/snow", dpi=500)
    }
}
```
