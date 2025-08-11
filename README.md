<!-- badges: start -->
[![R-CMD-check](https://github.com/abhijithkpgen/PBAT/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/abhijithkpgen/PBAT/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

<p align="center">
  <img src="inst/app/www/LogoNobg.png" alt="PbAT Logo" width="200"/>
</p>

#  PbAT: Plant Breeding Analytical Tools

**PbAT** is a Shiny-based R package that provides an **end-to-end pipeline** for Experimental design , multivariate, and mating design analyses in plant breeding experiments.  
The application is hosted online at: **[https://pbat.online](https://pbat.online)**

---

##  Key Features

- **Experimental Design Analysis**
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

## Application Workflow

1. **Upload Data**  
   - CSV format required  
   - Columns: genotype, location, replication, block (if applicable), and trait(s)

2. **Select Analysis Type**  
   - **Experimental Design Analysis**  
   - **Multivariate Analysis**  
   - **Mating Design Analysis**

3. **Map Data Columns**  
   - Assign columns for genotype, location, replication, block, etc. as prompted

4. **Run Analysis**
   - **Experimental Designs**: EDA pipeline  
   - **Multivariate**: PCA, GGE, AMMI, correlation, path analysis  
   - **Mating Design**: Diallel & Line × Tester

5. **Review & Download Results**  
   - All outputs downloadable as ZIP (CSV + PDF)  
   - Ready for reports and publications

---

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
