# Open Source Spatio-Temporal Toolkit for Crime Analysis
### Contributors: Jamie Spaulding and Keith Morris

#### Abstract

Traditionally, third-party mapping applications are used for analysis of crime data. Such applications are often expensive and lack flexibility, transparency, or efficiency in uncovering associations and relationships in crime. An open source (R) solution is proposed to remedy these problems and expand current techniques with a spatio-temporal methodology. R has demonstrated great versatility for the mapping of incidents, statistical analysis, and prediction modeling process. Crime incident data gathered from the city of Chicago was used to develop and test several tools for crime analysis. The developed scripts (code) for the crime analysis are provided to facilitate further optimization of techniques in an open source platform.

#### Keywords

Crime Analysis, Geographic Information Systems (GIS), Spatio-Temporal, Near Repeat Analysis, Open Source, Technical Note

#### Instructions

When using the provided Chicago database; ensure that the R script entitled "FIRST_STEP_Database_Combination" is run first to combine the database parts. This databse must be stored locally as it exceeds the GitHub limit of 100MB. This can then be used in the various scripts provided. Documentation can be found in each script as to the functions that are being applied.

#### Notes

The authors have modified the Chicago crime incident data. Throughout the incidents in the dataset acquired from the Chicago Data Portal (see https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2) there were several incidents missing a latitude and longitude. The addresses were geocoded to the center of the city block provided for incorporation into the dataset.

Also note that the database only contains the reported crime incidents from 1/1/2001-12/31/2017. 

#### Chicago Data Portal Terms of Use Statement

Per the Chicago Data Portal terms of use; this project provides applications using data that has been modified for use from its original source, www.cityofchicago.org, the official website of the City of Chicago. The City of Chicago makes no claims as to the content, accuracy, timeliness, or completeness of any of the data provided with this project. The data provided at this site is subject to change at any time. It is understood that the data provided at this site is being used at one’s own risk. Please see https://www.cityofchicago.org/city/en/narr/foia/data_disclaimer.html for more details.

