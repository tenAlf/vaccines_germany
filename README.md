Vaccines by county in Germany
================

# Introduction

This project is mainly considered as a exercise in the usage of git (and
GitHub), Rstudio and possibly developing a shiny App. Based on the
vaccination data published by the RKI this project will focus on
creating a visualization of the development of vaccinations administered
on county level.

# Data

As mentioned the main data originates directly from the data published
by the RKI. It’s divided into date of vaccination applied, vaccination
place based on county id, vaccination category (first, second or third
shot), age group and a count of applied vaccinations in a given county.
To achieve a better usability and readability of the county id, which is
derived from the AGS (amtlicher Gemeindeschlüssel), I matched the
vaccination table to data acquired from regionalstatistik.de (see
References for further information). This allowed me to create an easily
searchable string where either county id or county (or in some cases
city) name can be provided to find the desired vaccination place.

# The Shiny App

This section briefly describes the shiny app and covers user inputs and
visualization description.  
### User Selection  
The Shiny application allows currently for three different user
selections. Vaccination place, vaccination categories and time-period.  
1. **Vaccination place:** Uses county id or name. Currently only one
county/ city can be selected.  
2. **Vaccination category:** Select first, second, third shot or all
three. Every category is displayed by a single graph.  
3. **Time-Period:** Allows for a period specification. Default is always
the global earliest and latest data point in the vaccination data. A
special behaviour is triggered when selecting only one day (line graph
changes to a bar-plot).  
### Output  
First and foremost the app provides a visualization of the vaccinations
applied in a given county by drawing a line graph based on the
cumulative sum of administered vaccines. An exception occurs if a single
day is selected. In this case a simple bar plot is drawn (one column per
category). Below the graph a table summarises the end sum of
vaccinations per chosen category in the selected period.

# Future Plans

In the future it’s planned to extend the user input possibilities and
allow for the selection of different age groups. Also a text field
informing about the last update-date of the vaccination-data would
increase reliability. Another possible future idea would be to look
beyond ggplot as the main plot engine and try another more interactive
solution (e.g. dygraphs).

# References

Robert Koch-Institut (2021): COVID-19-Impfungen in Deutschland, Berlin:
Zenodo. DOI: 10.5281/zenodo.5126652

Statistische Ämter des Bundes und der Länder, Regionalstatistik:
11111-01-01-5 Gebietsfläche in qkm - Stichtag 31.12. - regionale Tiefe:
Gemeinden, URL: tinyurl.com/agsdatasource
