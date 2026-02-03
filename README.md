# IJC445 – Music Data Analysis & Visualisation

## Topic
**Musical Characteristics and Popularity Dynamics in Billboard Hot-100 Songs (2000–2023)**

## Overview
This repository contains the data analysis and visualisation work completed for the IJC445 Data Visualisation coursework. The project investigates how Spotify audio features relate to song popularity, measured using Billboard Hot-100 Top-10 performance.

The analysis combines exploratory, explanatory, evaluative, and distributional visualisations to examine non-linear relationships between musical characteristics and chart success.

---

## Contents
- **R scripts** for data cleaning, modelling, and visualisation  
- **Generated figures** used in the coursework report  
- **Supporting code** for Logistic Regression and Random Forest models  

---

## Dataset
The analysis uses a dataset combining:
- Billboard Hot-100 chart data (2000–2023)
- Spotify audio features such as danceability, energy, tempo, and valence

> **Note:** The dataset file (`BillboardDataset.csv`) must be present in the working directory for the scripts to run correctly.

---

## Figures Generated
The scripts generate the following figures referenced in the report:
1. **Figure 1** – Danceability vs Energy scatter plot  
2. **Figure 2** – Random Forest Partial Dependence Plots  
3. **Figure 3** – ROC Curve comparison (Logistic Regression vs Random Forest)  
4. **Figure 4** – Empirical Cumulative Distribution Function (ECDF) of tempo  

---

## Required R Packages
If not already installed, run the following before executing the scripts:

```r
install.packages(c(
  "data.table",
  "tidyverse",
  "caret",
  "pROC",
  "randomForest"
))

## How to Run
1. Open the R script in RStudio  
2. Set the working directory to the project folder  
3. Ensure `BillboardDataset.csv` is present in the directory  
4. Run the script from top to bottom to generate all figures  
