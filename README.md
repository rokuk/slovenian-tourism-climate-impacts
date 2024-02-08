# slovenian-tourism-climate-imapcts

Code and figures for an analysis of climate indexes and conditions relevant for tourism in Slovenia.

Code is in the [R](/R) folder:

- Analysis of CIT data from Copernicus is in [CIT.md](/R/CIT.md)
- Analysis of HCI data from Copernicus is in [HCI.md](/R/HCI.md)
- Choice of gridpoints from the Copernicus dataset (for CIT and HCI) is described in [CIT-HCI-gridpoints.md](/R/CIT-HCI-gridpoints.md)
- Analysis of snow indicators from Copernicus is in [Snow.md](/R/Snow.md)
- Analysis for a few chosen ARSO stations is in [WeatherStations.md](/R/WeatherStations.md)
- Calculation of CIT and HCI from ARSO data is in [ARSO-CIT-HCI.md](/R/ARSO-CIT-HCI.md)

Figures are in the [output](/output) folder.

The data used is not included in the repository. Copernicus CIT, HCI and snow datasets can be downloaded from [DOI: 10.24381/cds.126d9ce7](https://doi.org/10.24381/cds.126d9ce7) and [DOI: 10.24381/cds.2fe6a082](https://doi.org/10.24381/cds.2fe6a082). All other data was provided by [ARSO](https://www.arso.gov.si) (Slovenian Environment Agency). Historical station measurements were retreived from the ARSO archive, available at http://meteo.arso.gov.si/met/sl/archive/ (accessed on March 4th 2022). Climate projections for the number of warm/hot days, tropical nights, number of days with snow cover and days with at least 1 mm/20 mm of precipitation were also provided by ARSO and are available upon reasonable request (see paper for contact information).
