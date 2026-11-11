# CHARLS Health and Work Dashboard (R, Shiny)

An interactive dashboard I built as my final project for QHS 605, Data Management and Visualization for Health Sciences (University of Hawaii at Manoa, Fall 2025). It is organized around my doctoral research question: how health shocks change work among older adults in China.

**Data.** The app generates a simulated panel (seed 20251208) that mirrors the structure of the China Health and Retirement Longitudinal Study (CHARLS), 2011–2020. No CHARLS data are included; the real data are available only under the CHARLS data use agreement. Numbers shown in the app are therefore illustrative, not research results.

**What the code does**

- Builds a person-by-wave panel with dplyr and tidyr (Part 2 of `app.R`).
- Defines a shared colour palette and ggplot2 theme (Parts 3–4).
- Lays out the dashboard with shinydashboard (Part 5).
- Server logic: a reactive data filter, 18 interactive ggplot2/plotly figures, and a CSV download of the filtered data (Part 6).

**Run it**

```r
install.packages(c("shiny", "shinydashboard", "tidyverse", "plotly", "DT",
                   "scales", "viridis", "RColorBrewer", "lubridate"))
shiny::runApp("app.R")
```

Tested with R 4.4.2.

Quan (Kyle) Zhang · https://quanzhang.org
