# Phyto_functional_traits_tdbu

This repository contains the data generated from the Eco-DAS phytoplankton functional trait literature review of 253 aquatic papers, as well as code for cleaning the data and generating figures for publication.

# Repository structure

This repository contains the following folders: 1) `data` contains the initial excel file that was used to extract data from papers during the full-text screening step, as well as additional csv files that were generated in the `01_clean_data.R` script, 2) `figures` contains all final figures generated for publication in the `02_manuscript_figs.R` script, and 3) `scripts` contains the two R scripts used to clean the data and generate manuscript figures.

# Metadata

*TD_BU_data_extraction.xlxs* contains 10 tabs, where the first and last (Key and Notes) provide details used to help coauthors define data entry columns and assign numeric and non-numeric responses for each cell. The remaining 8 tabs were assigned to each coauthor and contain the data extracted from each paper. Note that the last column in all data entry tabs identifies whether the paper was included or excluded from our analysis.

# Instructions to reproduce figures and analyses

The final cleaned data used to generate figures are available in the `data` folder (*final_td_bu_df.csv*), so it is only necessary to run `02_manuscript_figs.R'.
