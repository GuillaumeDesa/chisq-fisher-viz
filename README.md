# chisq-fisher-viz

`chisq-fisher-viz` is an interactive Shiny application that allows users to perform Chi-Squared and Fisher's Exact Tests of independence on categorical data. The app visualizes the results with association and mosaic plots, and provides detailed p-value interpretations to guide statistical analysis. The tool is designed for users to upload frequency tables, automatically selects the appropriate test based on data characteristics, and helps interpret the significance of the test results.

## Features

- **Chi-Squared and Fisher's Exact Tests**: Automatically selects and performs the appropriate test based on the data.
- **Interactive Visualizations**: Visualizes test results using association plots (for Chi-Squared) and mosaic plots (for Fisher's Exact Test).
- **P-value Interpretation**: Provides an explanation on how to interpret the p-value based on an alpha level of 0.05.
- **Monte Carlo Simulations**: Uses Monte Carlo simulations for Fisher's Exact Test if the table frequencies are too large.

## Installation

To use this Shiny app locally, you need to install the following R packages:

```r
install.packages(c("shiny", "openxlsx", "ggplot2", "shinycssloaders", "vcd"))
```

These will be installed automatically by the script.

## Usage

Clone this repository:

```
git clone https://github.com/yourusername/chisq-fisher-viz.git
cd chisq-fisher-viz
```

Open the R script in RStudio or run it directly in your R console:

```r
shiny::runApp("path_to_this_repo")
```

The app will:

- Perform the Chi-Squared or Fisher's Exact Test (depending on data).
- Display the corresponding plot (association plot for Chi-Square or mosaic plot for Fisher's exact test).
- Provide p-value interpretation and statistical results.

## How to Interpret the Results

- **Chi-Squared Test**: The test is used when the expected frequencies in each cell are large enough (typically greater than 5). If the p-value is less than 0.05, it indicates a significant association between the variables.

- **Fisher's Exact Test**: Used when the expected frequencies are too low for the Chi-Squared test to be valid. If the p-value is less than 0.05, there is a significant association.

- **Mosaic Plot (Fisher’s Test)**: The size of the tiles indicates the frequency of observations. Shaded tiles highlight significant deviations from independence.

- **Association Plot (Chi-Squared Test)**: Shows the deviations from independence in the form of colored cells, with blue and red colors representing positive and negative residuals respectively.

## License

This work © 2024 by Guillaume Desagulier is licensed under CC BY-NC 4.0 

## Acknowledgements
The `vcd` package is used for visualizing contingency table results through association and mosaic plots.
`shiny` was used to create the interactive web application.


