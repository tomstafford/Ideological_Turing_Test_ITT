## Processing

This directory contains the scripts and data for analyzing the data collected.

This consists of 3 steps:

1.  Load and combine the raw data for each combination of arguer and rater (`01_load_data.R`). The files produced are saved in `data/loaded_data`

2.  The loaded data are then cleaned (`02_clean_data.R`) and the resulting files are saved in `data/cleaned_data`. Cleaning consists of:

    -   Adding a label for the `arguer_position` (Pro/Anti).

    -   Making the labels for `argument_position` (For/Against) and `rater_position` (Pro/Anti) consistent.

    -   Generating unique and continuous `rater_ID` and `argument_index` variables.
    
3.  The cleaned data are then split into the _Baseline_ and _ITT_ conditions (`03_split_data.R`) and placed in `data/split_data`.

    - Baseline: Arguer Position, Argument Position, and Rater Position are all the same (e.g. Pro, For, Pro)
    
    - ITT: Arguer Postion differs from Argument Position and Rater Position (e.g. Pro, Against, Anti)
    
There is an optional 4th step (`04_shiny_data.R`), which prepares the data for use in the shiny app.


## Analysis

After the ITT and Baseline Conditions were created, means were calculated, assumptions were checked, Likert answers were converted to 1-7 scales, and smaller dataframes created ready for analysis.

Analyses of the Likert Scale ratings were conducted using ordinal logistic models using Richard McElreath's Rethinking package (2018)
