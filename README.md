# Poverty Prediction Models

This repository contains code, data, and outputs for poverty prediction models developed as part of the Problem set 2 solved by team 7.

## Repository Structure

### 📁 Documents
Contains reference materials including:
- Data dictionary with variable definitions
- Initial data exploration in Excel
- Job type documentation

### 📁 Models
Contains CSV output files from different prediction models used for submission, including:
- Random Forest models
- Naive Bayes models
- Additional classification algorithms

### 📁 Scripts
Contains the R code files for:
- Data processing scripts
- Model implementation code
- "Taller 2 -Gm-Rmd.Rmd" - Main R Markdown file with all models and analysis

### 📁 Stores
Contains datasets required for running the scripts:
- Training datasets
- Testing datasets
- Auxiliary data files


## Project Description

This project implements multiple machine learning models to predict poverty status (poor vs. non-poor) of households based on socioeconomic indicators. The models directly classify households without using intermediate metrics, providing straightforward binary predictions.

## Models Implemented

The repository includes several classification algorithms:
- Logistic Regression
- Boosting (GBoost-XGBoost)
- Random Forest
- Naive Bayes
- Elastic Net

## How to Use

1. Start by exploring the `Documents` folder to understand the variables and context
2. Required datasets are available in the `Stores` folder
3. The data analysis and variables construction can be found in `Scripts/data_process.R`
4. The code for model predicctions can be found in `Scripts/Taller 2- G7.Rmd`
5. Model predictions for submission are stored in the `Models` folder


## Contributors
- mvillabonam: Mariana Villabona Martinez
- sasm1: Sergio Alejandro Sánchez Martinez
- amaliavargas2: Amalia Vargas Guayacán

