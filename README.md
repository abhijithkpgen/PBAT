<!-- badges: start -->
[![R-CMD-check](https://github.com/abhijithkpgen/PBAT/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/abhijithkpgen/PBAT/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

<p align="center">
  <img src="man/figures/LogoNobg.png" alt="PbAT Logo" width="200"/>
</p>


# PbAT: Plant Breeding Analytical Tools

**PbAT** is a Shiny-based R package that provides an **end-to-end pipeline** for statistical, multivariate, and mating design analyses in plant breeding experiments.  
The application is hosted online at: **[https://pbat.online](https://pbat.online)**

---

##  Key Features

- **Experimental Design Analysis (EDA)**
  - Summary statistics, ANOVA, heritability (H²)
  - BLUEs / BLUPs (combined & location-wise)
  - Diagnostics & post-hoc tests
  - Publication-ready visualizations (boxplots, QQ plots, interaction plots)

- **Multivariate Analysis**
  - PCA, GGE Biplot, and AMMI Analysis
  - Trait correlation & path analysis
  - Standalone or linked to EDA results

- **Mating Design Analysis**
  - Diallel (Griffing Methods I–IV, Partial Diallel)
  - Line × Tester analysis
  - GCA/SCA effects, ANOVA tables, variance components

- **Downloads**
  - Export results as ZIP archives containing CSV/PDF outputs
  - High-quality, publication-ready plots

---

##  Application Workflow

1. **Upload Data**
   ![Loading data](man/figures/Loading%20data.jpg)
   - The required format is CSV   and
     add Columns: genotype, location, replication, block (if applicable), and trait(s), you can refer the example data sets for the required format which is given at help and guide section

3. **Select Analysis Type depending on your workflow.**  
   - **Experimental Design Analysis**  
   - **Multivariate Analysis**  
   - **Mating Design Analysis**
     

4. **Map Data Columns**


   ![Map columns](man/figures/Map%20coloumns.jpg)
   - Assign the appropriate columns (genotype, location, replication, block, etc.) when prompted, to enable correct analysis.

6. **Run Analysis**

    ![](man/figures/Descriptive%20analysis.jpg)

   - **Experimental Designs**: Generate summary statistics, ANOVA, heritability (H^2), BLUEs/BLUPs (combined and location-wise), diagnostics, and post-hoc tests. Visualize results with boxplots, QQ plots, and interaction plots. 
   - **Multivariate**: Perform PCA, GGE biplot , AMMI, correlation, and path analysis on selected traits. Run analyses in standalone mode or linked to your EDA results.
   - **Mating Design**: Analyze Diallel (Griffing Methods I-IV, PartialDiallel Designs) and Line x Tester. Obtain GCA/SCA effects, ANOVA tables, 

8. **Review & Download Results**  
   - All results-interactive tables, plots, and summaries-can be downloaded as publication-ready PDF/CSV or Text file as ZIP file.

## 📂 Sample Data

PbAT includes sample CSV templates for each analysis type in `inst/app/www/`.  
They are also downloadable from the “Help & Guide” tab within the app.

---

## 💻 Installation

```r
# Install devtools if not available
install.packages("devtools")

# Install PbAT from GitHub
devtools::install_github("abhijithkpgen/PBAT")
# Load the library
library(PBAT)
#Run the app and the app will be opened in a new window
run_app()

