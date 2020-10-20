# Berries-Project

## Overview

This project is began as practice of Shinyapp and exploratory data analysis for MA615(Data Science in R) of MSSP program in Boston University. 

The project is intended to make some quick stats and comparisons for berries(blueberries, strawberries and raspberries) in United States. 

These data were collected from the USDA database selector: <a href="https://quickstats.nass.usda.gov">https://quickstats.nass.usda.gov</a>

The data were <a href="https://quickstats.nass.usda.gov/results/D416E96E-3D5C-324C-9334-1D38DF88FFF1">stored online</a> and then downloaded as a CSV file.

The author will continue to update the project. 

## Description

By far, data cleaning has only finished on blueberry and strawberry. Further effort on raspberry will be expected. The source code is contained in ag_data_blueberry.Rmd/ag_data_strawberry.Rmd. It is not necessary for you to run these two RMarkdown files. The output datasets from these two files, are bberry.csv and sberry.csv. 

The source code for the Shinyapp is contained in the berry_app.R. You can also try running it from <a href="https://yinfeng.shinyapps.io/Berries_Project/">shinyapps.io</a>.(Though the website doesn't work well sometimes. If you have met an error, run the app locally.)

## The Dataset Overview Tab

This tab provides a basic searching and classifying function for the dataset. You can easily access to any subset you are interested in in this tab.

## The Chemical Comparison Tab

This tab is designed for those who are paying attention to stats of chemicals application and treatment on berries. The tab provides an interface for comparison of  the used of different types of chemicals.

## The Production Analysis Tab

For production, further EDA method should be employed on the dataset. By far, this tab will only provide a basic point plot for elementary EDA.

## Acknowledgment

The work blueberry.Rmd and strawberry.Rmd is based on the work of Professor Haviland Wright. Special thanks to his instruction, and the help from MSSP students Hao Shen, Ziyi Bai and Chun Gao. 

```latex
@article{usda2017quick,
  title={Quick stats},
  author={USDA, NASS},
  journal={USDA National Agriculural Statistics Service, Washington, DC https://quickstats. nass. usda. gov/(Accessed 3 May 2019)},
  year={2017}
}
```

```latex
@article{rstudioshiny,
  title={Shiny, 2019},
  author={RStudio, I},
  journal={URL https://shiny. rstudio. com}
}
```

```latex
@article{rstudioweb,
  title={A web application framework for R},
  author={RStudio, Shiny},
  journal={URL: http://shiny. rstudio. com}
}
```