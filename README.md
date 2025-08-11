<!-- badges: start -->
[![R-CMD-check](https://github.com/abhijithkpgen/PBAT/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/abhijithkpgen/PBAT/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

<p align="center">
  <img src="https://raw.githubusercontent.com/abhijithkpgen/PBAT/main/inst/app/www/LogoNobg.png" alt="PbAT Logo" width="200"/>
</p>

# PbAT: Plant Breeding Analytical Tools

**PbAT** is a Shiny-based R package that provides an **end-to-end pipeline** for  
Experimental design, Multivariate analysis, and Mating design analyses in plant breeding experiments.  

The application is hosted online at: [**https://pbat.online**](https://pbat.online)

---

## 🌱 Key Features

### 1. Experimental Design Analysis (EDA)
- Summary statistics
- ANOVA
- Heritability (H²)
- BLUEs / BLUPs (combined and location-wise)
- Diagnostics
- Post-hoc tests
- Visualizations: Boxplots, QQ plots, interaction plots

### 2. Multivariate Analysis
- PCA
- GGE biplot
- AMMI analysis
- Correlation analysis
- Path analysis  
- Run in standalone mode or linked to EDA results

### 3. Mating Design Analysis
- Diallel Analysis (Griffing Methods I–IV, Partial Diallel)
- Line × Tester Analysis
- GCA / SCA effects
- ANOVA tables
- Variance components

---

## 📊 Application Workflow Overview

1. **Upload Data**  
   Upload your experimental data in **CSV** format.  
   Include columns for genotype, location, replication, block (if applicable), and trait(s).

2. **Select Analysis Type**  
   Choose one of:  
   - Experimental Design Analysis  
   - Multivariate Analysis  
   - Mating Design Analysis

3. **Map Data Columns**  
   Assign the appropriate columns (genotype, location, replication, block, etc.) when prompted.

4. **Run Analysis**  
   Generate results, tables, and plots according to the chosen workflow.

5. **Review & Download Results**  
   Download interactive tables, plots, and summaries as **publication-ready PDF/CSV ZIP files**.

---

## 📂 Repository Structure

