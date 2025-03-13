# Mentor Training Analysis

This repository provides R scripts and data used to perform all analyses presented in *Balancing value and cost in Initial Teacher Education mentor training: an expectancy-value theory perspective (2025)* paper. The code supports data preprocessing, statistical analyses, table generation, and data visualization related to mentor training effectiveness in Initial Teacher Education (ITE).

## Repository Structure

### Data Folder
- **`data/deriv/`**
  - Contains derived and preprocessed data files used in all analyses.
  - Data files have been cleaned, structured, and prepared to ensure reproducibility of results.

### R Scripts

1. **Data Cleaning & Preparation**
   - `teacherTapp_qual_codes_sanityChecking_finalClean.R`
     - Performs cleaning, sanity checking, and prepares qualitative codes generated from Teacher Tapp open text survey data.

2. **Demographic Analysis**
   - `demographicTable_qual.R`
     - Generates demographic summaries for qualitative data.

   - `demographicTable_quant_by_seniority.R`
     - Produces demographic summaries for quantitative survey data, segmented by seniority.

3. **Statistical Analyses & Results**
   - `chiSquare.R`
     - Conducts chi-square tests assessing associations in survey responses.

   - `closedResponse_overallPercentages.R`
     - Calculates overall percentages of closed-ended responses across survey options.

   - `teacherTapp_qual_codes_analysis.R`
     - Calculates coding frequency summaries for qualitative Teacher Tapp coding data.

   - `teacherTapp_qual_agreement.R`
     - Analyzes agreement levels among Teacher Tapp coding data.

4. **Utility Functions**
   - `utils.R`
     - Provides utility functions, common libraries, and custom themes used across scripts.

## Data File Explanation

The data in `data/deriv/` includes:
- **Qualitative coded data**: Cleaned codes for open-ended survey responses.
- **Quantitative summary data**: Aggregated responses to closed-ended questions, structured for statistical testing and table generation.
- **Demographic data**: Demographic variables including seniority, experience level, school context, and subject specialisation.

Each dataset is named to indicate the type of analysis it supports and each script reads in the correct data file.
