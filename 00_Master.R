# ---------------------------------------------------------------------------- #
#
#                        Offshore data leaks and tax enforcement 
#                                 in developing countries
#
#                                  MASTER 
#
#       Author: Hao Lyu                           Update: 11/13/2023
#
# ---------------------------------------------------------------------------- #


# ================================== Goal ==================================== #
# 
#       This R script is to clean 3 datasets for the leaks data project. 
#           1)    an officer-entity relationship level master dataset,
#           2)    an officer level dataset,
#           3)    an entity level dataset;
#       
#       Employing these datasets, this script will also generate graphs: 
#           1)    a descriptive analysis table 
#           2)    graphs for descriptive analysis 
#           
# ---------------------------------------------------------------------------- #


# SET UP  --------------------------------------------------------------------

    # Cleaning the environment 
    rm(list=ls()) 
  
  
    # Setting paths
    if (Sys.info()["user"] == "haolyu"){    
      
      projectFolder   <- "/Users/haolyu/Dropbox/HighNetWorth_Project/Leaks_Data"
      scriptFolder    <- "/Users/haolyu/Documents/GitHub/high-net-worth-taxation/Leaks_Data"
      
    } else if (Sys.info()["user"] == "wb463689") {
      
      projectFolder   <- "/Users/wb463689/Dropbox/HighNetWorth_Project/Leaks_Data" 
      scriptFolder    <- ""
      
    } else if (Sys.info()["user"] == "thiagoscott") {
    
      projectFolder   <- "/Users/thiagoscott/Dropbox/HighNetWorth_Project/Leaks_Data" 
      scriptFolder    <- ""
      
    } else if (Sys.info()["user"] == "roxan") {
      
      projectFolder   <- "/Users/roxan/Dropbox/HighNetWorth_Project/Leaks_Data" 
      scriptFolder    <- ""
      
    }
    
      scripts             <- file.path(scriptFolder, "Scripts")
      raw                 <- file.path(projectFolder, "01_Raw/")
      intermediate        <- file.path(projectFolder, "02_Intermediate")
      cleaned             <- file.path(projectFolder, "03_Cleaned")
      output              <- file.path(projectFolder, "04_Output")
  
      
  # Install & load packages
    packages  <-  c(
    
        "here"        ,
        "tidyverse"   ,
        "dplyr"       ,
        "tidyr"       , 
        "data.table"  ,
        "tidyfast"    ,
        "stringi"     ,
        "pacman"      ,
        "janitor"     ,
        "devtools"    ,
        "lubridate"   ,
        "stringr"     ,
        "stargazer"   ,
        "ggplot2"     ,
        "purrr"       ,
        "kableExtra"  ,
        "knitr"       ,
        "GGally"      ,
        "network"     ,
        "sna"         ,
        "RColorBrewer",
        "intergraph"  ,
        "igraph"      ,
        "plotly"      ,
        "plyr"        ,
        "reshape2"    ,
        "scales"      ,
        "WDI"         ,
        "rworldmap"   ,
        "ggmap"       ,
        "sf"          , 
        "wesanderson" ,
        "rnaturalearth", 
        "rnaturalearthdata",
        "ggthemes"    ,
        "mapproj"     ,
        "viridis"     ,
        "hrbrthemes"  ,
        "naniar"      ,
        "tmap"        ,
        "cartogram"   ,
        "geodaData"   ,
        "formattable",
        "rgdal"
        )
    

    load_package <- function(pkg){
      
      new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
      
      if (length(new.pkg)) 
        
        install.packages(new.pkg, dependencies = TRUE)
      
      sapply(pkg, require, character.only = TRUE)
      
    }
  
  load_package(packages)


  # set visualization theme 
  my_theme = theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    legend.title = element_text(size= 20), #change legend title font size
    legend.text = element_text(size= 20))
  
  
# Running Scripts ------------------------------------------------------------
  
      # 1) This script aims to clean ICIJ data and generate the 3 datasets for analysis:
      #    (a) Officer-level dataset
      #    (b) entity-level dataset
      #    (c) Master dataset 
          source(file.path(scripts, "01_Clean.R"))
  
      # 2) This script aims to conduct the first descriptive section of the report 
          source(file.path(scripts, "02_Analysis.R"))
  
      # 3) This script aims to conduct the country-specific analysis for the second section of the report
          source(file.path(scripts, "03_"))



