# The Ideological Turing Test: a behavioural measure of open-mindedness and perspective-taking 

This directory contains the data, pre-processing scripts and analysis scripts accompanying the paper (which can be found [here]())

## Pre-requisites

The scripts were developed using R 4.2 and the majority of required packages are recorded in the `renv.lock` file.  
The exception is the `rethinking` package, which requires some additionla steps (including setting up a C++ toolchain). Please see the [`rethinking` github repo](https://github.com/rmcelreath/rethinking) for installation instructions.  
The analysis was originally conducted with `rethinking v1.95` using R 3.6, and later re-run in `rethinking v2.2.1` in R 4.2.

## Pre-Processing

This consists of 3 steps:

1.  Load and combine the raw data for each combination of arguer and rater (`01_load_data.R`). The files produced are saved in `data/loaded_data`

2.  The loaded data are then cleaned (`02_clean_data.R`) and the resulting files are saved in `data/cleaned_data`. Cleaning consists of:

    -   Adding a label for the `arguer_position` (Pro/Anti).

    -   Making the labels for `argument_position` (For/Against) and `rater_position` (Pro/Anti) consistent.

    -   Generating unique and continuous `rater_ID` and `argument_index` variables.
    
3.  The cleaned data are then split into the _Baseline_ and _ITT_ conditions (`03_split_data.R`) and placed in `data/split_data`.

    - Baseline: Arguer Position, Argument Position, and Rater Position are all the same (e.g. Pro, For, Pro)
    
    - ITT: Arguer Postion differs from Argument Position and Rater Position (e.g. Pro, Against, Anti)
    
## Analysis

After the ITT and Baseline Conditions were created, means were calculated, assumptions were checked, Likert answers were converted to 1-7 scales, and smaller dataframes created ready for analysis.

Analyses of the Likert Scale ratings were conducted using ordinal logistic models using Richard McElreath's Rethinking package (2018). The scripts for these analyses can be found in the `analysis` directory.

## Plots

Two different plots are associated with this project:

1. __'Rating' plots__ - These plots show the distribution of ratings for the ITT condition in each topic, and can be generated with `05a_plot_rating_data.R`.
2. __'Stanley' plots__ - These plots show the absolute and cumulative probability of giving a particular ordinal response having either passed or failed the ITT, as well as the 'average' rating for the pass/fail ITT conditions. They can be generated using the `05a_plot_stanley_data.R` scripts, but need `rethinking` installed to work.

## Tables

The tables that correspond to the 'Stanley' plots (i.e. absolute and cumulative probability of giving a particular ordinal response having either passed or failed the ITT) are generated using a R Notebook. This can be found in the `notebooks` directory, knitted to a html file, and the tables can be copied and edited from there.  
__NB:__ This requires the `rethinking` package to be installed.

## Shiny data

Finally, the data used for the Shiny app built for this project (found here) can be generated using the `06_shinydata.R` script.
