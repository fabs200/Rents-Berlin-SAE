
#MASTER FILE SAE WiSe 2018/19
#Authors: Fabian Nemeczek, Anna Gesing, Irakli Sauer
#Title: Estimating Rents in Berlin using Small Area Estimation methods

rm(list = ls())

#turn off scientific notation 
options(scipen = 999)

if (!require("ggplot2")) install.packages("ggplot2")
if (!require("foreign")) install.packages("foreign")
if (!require("Hmisc")) install.packages("Hmisc")
if (!require("stargazer")) install.packages("stargazer")
if (!require("stringr")) install.packages("stringr")
if (!require("reporttools")) install.packages("reporttools")
if (!require("labelled")) install.packages("labelled")
if (!require("readxl")) install.packages("readxl")
if (!require("emdi")) install.packages("emdi")
if (!require("plyr")) install.packages("plyr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("laeken")) install.packages("laeken")
if (!require("simFrame")) install.packages("simFrame")
if (!require("sae")) install.packages("sae")
if (!require("xtable")) install.packages("xtable")
if (!require("mice")) install.packages("mice")
if (!require("rgdal")) install.packages("rgdal")
if (!require("Metrics")) install.packages("Metrics")


# paths, libraries ------------------------------------------------------------------

# read user
user <- Sys.info()["user"]

# define user specific paths
if (user == "Fabian") {
  print(user)
  projectpath  <- "/Users/Fabian/OneDrive/Studium/Armutsmessung/presentation/R/"
  soeppath     <- "/Users/Fabian/Documents/DATA/STATA/SOEP_v33.1/SOEP_wide/"
  regionpath <- "/Users/Fabian/Documents/DATA/STATA/Region/"
  berlindata   <- "/Users/Fabian/OneDrive/Studium/Armutsmessung/presentation/data/berlin/"
}
if (user == "Irakli") {
  print(user)
  projectpath <- "C:/Users/Irakli/OneDrive/presentation/R/"
  soeppath    <- "C:/Users/Irakli/Desktop/UNI/Machine Learning/Data/Stata/SOEP-CORE_v33.1_stata_bilingual/"
  regionpath <- "C:/Users/Irakli/Desktop/UNI/Machine Learning/Data/Stata/data/sae/"
  berlindata  <- "C:/Users/Irakli/OneDrive/presentation/data/berlin/"
}
if (user == "Anna") {
  print(user)
  projectpath  <- "/Users/Anna/OneDrive/Studium/Armutsmessung/presentation/R/"
  soeppath     <- "/Users/Anna/Documents/DATA/STATA/SOEP_v33.1/SOEP_wide/"
  regionpath <- "/Users/Anna/Documents/DATA/STATA/Region/"
  berlindata   <- "/Users/Anna/OneDrive/Studium/Armutsmessung/presentation/data/berlin/"
}

# Loading libraries and the data
library(ggplot2)
library(foreign)
library(Hmisc)
library(stargazer)
library(stringr)
library(reporttools)
library(labelled)
library(readxl)
library(emdi)
library(plyr)
library(dplyr)
library(laeken)
library(simFrame)
library(xtable)
library(mice)
library(Metrics)

# 1. data preparation unit level ----------------------------------------------------
source(paste0( projectpath, "01_data_preparation_unit_level.R"))

# 2. data preparation area level ----------------------------------------------------
source(paste0( projectpath,"02_data_preparation_area_level.R"))

# 3. descriptive statistics ---------------------------------------------------------
source(paste0( projectpath,"03_descriptives.R"))

# 4. FH estimation ------------------------------------------------------------------
source(paste0( projectpath,"04_FH_estimation.R"))

# 5. BHF estimation -----------------------------------------------------------------
source(paste0( projectpath,"05_BHF_estimation.R"))

# 6. Robustness checks --------------------------------------------------------------
source(paste0( projectpath,"06_robustness_checks.R"))

# 7. Map preparation ----------------------------------------------------------------
source(paste0( projectpath,"07_maps.R"))

# 8. Plot maps ----------------------------------------------------------------------
source(paste0( projectpath,"08_plot_maps.R"))

# 9. Plot results -------------------------------------------------------------------
source(paste0( projectpath,"09_results.R"))



###
