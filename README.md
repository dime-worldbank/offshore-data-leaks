---
output:
   pdf_document:
    latex_engine: custom
    latex_engine_path: "\c\Users\roxan\AppData\Roaming\TinyTeX\bin\win32\pdflatex.exe"
  html_document:
    df_print: paged
---

#Offshore Data Leaks and Tax Enforcement in Developing Countries\* (Pierre Bachas, Matthew Collin, Thiago Scot, Hao Lyu, and Tatiana Flores)

## Overview

This repository provides all the code required to replicate the findings presented in the report. The report is divided into two sections: The first section utilizes open-source ICIJ offshore leaks data to explore intriguing patterns and descriptive statistics within this dataset. The second section of the report leverages a combination of ICIJ offshore leaks data and administrative records obtained in collaboration with tax authorities in Ecuador, Honduras, and Senegal.

The goal of the data exercise is to understand the prevalence of taxpayers from each of our three countries in the ICIJ's Offshore Leaks dataset and the degree to which the data could be merged with taxpayer registers to gauge the incidence of offshore ownership along the in- come distribution. To do so, we generate a list of names of shareholders and beneficial owners for each country of interest, using three steps: (i) assigning officers to countries, (ii) selecting firms, and (iii) cleaning name lists.

## Data Accessibility

All the microdata from ICIJ offshore leaks used for the first section analysis of the report is available [here](https://offshoreleaks.icij.org/pages/database). The administrative microdata used to generate the country cases analysis are classified as restricted-access data, which were accessed by researchers in collaboration with the broader World Bank Research Team and the Tax Administration Teams of Servicio de Rentas Internas (SRI) in Ecuador, Servicio de Administracion de Rentas (SAR) in Honduras, and Direction Générale des Impôts et des Domaines in Senegal.

## ICIJ Data disclaimer

There are legitimate uses for offshore companies and trusts. The inclusion of a person or entity in the ICIJ Offshore Leaks Database is not intended to suggest or imply that they have engaged in illegal or improper conduct. Many people and entities have the same or similar names. We suggest you confirm the identities of any individuals or entities included in the database based on addresses or other identifiable information. The data comes directly from the leaked files ICIJ has received in connection with various investigations and each dataset encompasses a defined time period specified in the database. Some information may have changed over time.

### Statement about Rights

-   [x] I certify that the author(s) of the manuscript have legitimate access to and permission to use the data used in this manuscript.
-   [x] I certify that the author(s) of the manuscript have documented permission to redistribute/publish the data contained within this replication package. Appropriate permission are documented below.

### License for Data

The ICIJ Offshore Leaks Database is licensed under the [Open Database License](https://opendatacommons.org/licenses/odbl/1-0/) and contents under [Creative Commons Attribution-ShareAlike](https://creativecommons.org/licenses/by-sa/3.0/) license. The data used for the country cases analysis are not publicly available and were made available to the researchers by the Revenue Service Administration (SAR) of Honduras.

### Summary of Availability

-   [ ] All data **are** publicly available.
-   [x] Some data **cannot be made** publicly available.
-   [ ] **No data can be made** publicly available.

### Details on each Data Source

Datils on the first section of this report:

| Input Datasets           | Description                                                                                                                                                                                                                                | Provided |
|------------------|-------------------------------------|------------------|
| nodes-addresses.csv.     | Main dataset on Contact address as it appears in the original databases obtained by ICIJ. This can be an officer's personal or business address, or a company's registered address                                                         | FALSE    |
| nodes-entities.csv       | Database on a company, trust or fund created by an agent in a low-tax jurisdiction that often attracts non-resident clients through preferential tax treatment                                                                             | FALSE    |
| nodes-intermediaries.csv | Dataset containing a go-between for someone seeking an offshore corporation and an offshore service provider --- usually a law firm, a bank or a middleman that asks an offshore service provider to create an offshore firm for a client. | FALSE    |
| nodes-officers           | Dataset containing information about a person or company who plays a role in an offshore entity                                                                                                                                            | FALSE    |
| relationships.csv        | Database connecting different nodes under specific relationship category, source ID and date                                                                                                                                               | FALSE    |

Details on the second section of this report:

## Computational requirements

### Software Requirements

-   R and Rstudio (code was last run with [64-bit] R-4.3.1 - all packages are as of 2023-11-12) -`here` -`tidyverse` -`dplyr` -`tidyr` -`data.table` -`tidyfast` -`stringi` -`pacman` -`janitor` -`devtools` -`lubridate` -`stringr` -`stargazer` -`ggplot2` -`purrr` -`kableExtra` -`knitr` -`GGally` -`network` -`sna` -`RColorBrewer` -`intergraph` -`igraph` -`plotly` -`plyr` -`reshape2` -`scales` -`WDI` -`rworldmap` -`ggmap` -`sf` -`wesanderson` -`rnaturalearth` -`rnaturalearthdata` -`ggthemes` -`mapproj` -`viridis` -`hrbrthemes` -`naniar` -`tmap` -`cartogram` -`geodaData` -`formattable` -`rgdal`

### Controlled Randomness

-   [x] Random seed is set at

### Memory and Runtime Requirements

#### Summary

Approximate time needed to reproduce the analyses on a standard 2022 desktop machine: XX minutes

#### Details

The code was last run on a MacBook pro (M2) laptop with MacOS version 12.4.

## Description of programs/code

-   The master script `code/00_Master.R` executes all other code necessary to replicate figures and tables in the first section of this paper.
-   The script `01_Master.R` loads, cleans and save the three main datasets on 1) an officer-entity relationship level master dataset, 2) an officer level dataset, and 3) an entity level dataset.
-   The script `02_Analysis.R` produce the first section figures and tables of this paper.
-   The script `03_Analysis_Honduras.R` produce the country case for the second section of the report

## Instructions to Replicators

-   Edit `code/0_Master.do` to adjust the default path
-   All necessary data should be stored at the input folders (`01_Raw` for publicly available data and `input_confidential` for restricted-access data). All other datasets necessary for analysis will be automatically generated from these input datasets when running the code.
-   Run `code/0_Master.do` to execute all scripts and generate tables and figures.

## List of tables and programs

The provided code reproduces:

-   [ ] All numbers provided in text in the paper
-   [x] All tables and figures in the paper
-   [ ] Selected tables and figures in the paper, as explained and justified below.

| Figures | Label                                                                            | Do-file              |
|------------------|------------------------------------|------------------|
| 1       | Figure 1: Distribution of Jurisdiction Where Firms Incorporated in Leaked Data   | 02_Analysis          |
| 2       | Figure 2: Individuals in Offshore Leaks Database (OLD) by Country GDP Per Capita | 02_Analysis          |
| 3       | Figure 3: Share of population named in Offshore Leaks Database (OLD)             | 02_Analysis          |
| 4       | Figure 4: Year of Incorporation of Entities in Tax Havens                        | 02_Analysis          |
| 5       | Figure 5: Share of taxpayers matched to leaks data (Honduras)                    | 03_Analysis_Honduras |
| 6       | Figure 6: Share of taxpayers as shareholders of corporations (Honduras)          | 03_Analysis_Honduras |
| 7       | Figure 7: Share of taxpayers matched to leaks data (Ecuador)                     | 04_Analysis_Ecuador  |
| 8       | Figure 8: Share of taxpayers matched to leaks data (Senegal)                     | 04_Analysis_Senegal  |

| Tables | Label                                                                | Do-file     |
|------------------|------------------------------------|------------------|
| 1      | Table 1: Descriptives Statistics For Leaked Individuals and Entities | 02_Analysis |

## References

Servicio de Administracion de Rentas de Honduras (SAR) (2022). Honduras Firms' Trade Networks dataset.
