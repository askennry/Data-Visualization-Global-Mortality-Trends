# 🌍 Global Burden of Disease & Macroeconomic Analysis (1990-2019)

This project uses R and the `tidyverse` ecosystem to perform in-depth data cleaning, integration, and visual analysis of global mortality, population, and macroeconomic (GDP and income groups) data from 1990 to 2019. The project aims to reveal the macro-trends of the **Epidemiological Transition** over the past 30 years—the shift of primary human causes of death from infectious diseases to chronic non-communicable diseases—and explore the profound impact of economic development on disease structures.

---

## 📊 Core Analysis & Visualization Modules

The main script (`final_analysis.R`) contains multi-dimensional deep visualizations, primarily divided into four modules:

### 1. Global Trends
* **29 Causes of Death Stacked Area Chart**: Shows the compositional changes of all causes of death from 1990 to 2019.
* **Evolution of GBD Broad Categories**: Standardizes causes of death into NCDs (Non-Communicable Diseases), CMNN (Communicable, Maternal, Neonatal, and Nutritional diseases), and Injuries to visually present the global epidemiological shift.
* **Representative Diseases Mortality Line Chart**: Tracks the absolute trends (Rate per 100k) of Cardiovascular Diseases, Malaria, and Road Injuries.
* **Top 10 Causes Comparison (Dumbbell/Bar Chart)**: Compares the ranking and standardized rate changes of the top causes of death globally between 1990 and 2019.
* **Global Cause of Death Slope Chart**: Tracks the ranking jumps and drops of the Top 15 causes of death over 30 years.

### 2. Geospatial Mapping
* **2019 Broad Categories World Map**: Uses the `sf` and `rnaturalearth` packages to plot heatmaps of the proportions of NCDs, CMNN, and Injuries across global countries, revealing regional health inequalities.

### 3. Economic Determinants
* **Evolution of Disease Structure by Income Group**: Compares the different evolutionary paths of disease composition in High, Upper-Middle, Lower-Middle, and Low-income countries.
* **Macro Factors Scatterplot Matrix**: Uses `GGally` to explore the correlations and distributions between GDP per capita, Year, and the mortality rates of the three broad disease categories.
* **Correlation Heatmap**: Generates a Spearman correlation matrix to explore potential associations between different disease mortality rates.
* **Diseases of Affluence vs. Diseases of Poverty**: Scatter plots displaying the association between GDP (log scale) and specific diseases (e.g., Diarrheal Diseases and Malaria vs. Cardiovascular Diseases and Alzheimer's).

### 4. Country Deep Dive: China (Customizable)
* The dramatic shifts in the target country's GBD broad categories and 29 specific causes of death over 30 years.
* **Economic Growth vs. Core NCD Burden (Dual Y-Axis)**: Compares the trajectory of surging GDP with the growth in cardiovascular mortality.
* **Global Health-Wealth Curve Positioning**: Highlights the specific country's relative position on the global scatter plot of GDP vs. cardiovascular mortality.
* **Specific Country Slope Chart**: Visually demonstrates the reshuffling of the top causes of death from 1990 to 2019.

---

## 📁 Data Requirements

To run this script, the following four CSV datasets must be placed in the project root directory:
1.  `cause.csv` - Contains the absolute number of deaths for 29 causes by country and year.
2.  `population.csv` - Historical population data for global countries (World Bank/UN standard).
3.  `gdp.csv` - Historical GDP per capita data for global countries.
4.  `gdp_rank.csv` - Country income level classification data (e.g., High income, Low income).

*(Note: Ensure the datasets contain key merge fields such as `Country/Territory`, `Code` (ISO-3), and `Year`.)*

---

## 🛠️ Prerequisites

This script relies on the following R packages. Please ensure they are installed in your R environment before running:

```R
install.packages(c(
  "tidyverse",      # Core data manipulation and ggplot2
  "scales",         # Axis formatting
  "ggrepel",        # Prevents text label overlapping
  "sf",             # Geospatial data processing
  "rnaturalearth",  # World map spatial data
  "GGally",         # Scatterplot matrices
  "corrplot"        # Correlation heatmaps
))
