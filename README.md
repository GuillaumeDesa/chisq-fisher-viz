# chisq-fisher-viz

**chisq-fisher-viz** is an interactive Shiny application that allows users to perform **Chi-Squared and Fisher's Exact Tests of independence** on categorical data. The app visualizes the results with association and mosaic plots, and provides detailed p-value interpretations to guide statistical analysis. The tool is designed for users to upload frequency tables, automatically selects the appropriate test based on data characteristics, and helps interpret the significance of the test results.


### Features
*   **Chi-Squared and Fisher's Exact Tests** : Automatically selects and performs the appropriate test based on the data.
*   **Interactive Visualizations** : Visualizes test results using association plots (for Chi-Squared) and mosaic plots (for Fisher's Exact Test).
*   **P-value Interpretation** : Provides an explanation on how to interpret the p-value based on an alpha level of 0.05.
*   **Monte Carlo Simulations** : Uses Monte Carlo simulations for Fisher's Exact Test if the table frequencies are too large.
*   **Dynamic Plot Scaling**: Plot height and width are automatically calculated based on the dimensions (rows and columns) of the input frequency table.
*   **Exportable HTML Report**: Generate and download a full statistical report (`.html`). The report includes the test message, p-value interpretation, and frequency tables (Observed, Expected, and Residuals for Chi-Squared).
*   **Exportable PNG Plot**: Download the association or mosaic plot as a high-resolution PNG file. Users can customize the plot's width and height using input fields measured in inches.

### Installation
To use this Shiny app locally, you need to install the following R packages:
*   `shiny`
*   `openxlsx`
*   `ggplot2`
*   `shinycssloaders`
*   `vcd`
*   **`rmarkdown`** (Required for HTML report generation)

These dependencies will be installed automatically by the script if they are missing.

### Usage
Clone this repository:
```bash
# Clone the repository
```
Open the R script in RStudio or run it directly in your R console:
```R
# R commands to run the script
```
**Data Upload**: Upload the data (a table of frequencies, a.k.a. a contingency table) as a spreadsheet file (preferably .xls or .xlsx, but also supports .csv or .txt). The file must include row names and headers. Click 'Submit'.

<img width="397" alt="Capture d’écran 2024-11-29 à 12 35 42" src="https://github.com/user-attachments/assets/354c0424-009a-463c-a962-e37b251998ea">

The app will:
*   Perform the Chi-Squared or Fisher's Exact Test (depending on data).
*   Display the corresponding plot (association plot for Chi-Square or mosaic plot for Fisher's exact test).
*   Provide p-value interpretation and statistical results.
*   Allow downloading the full statistical report in HTML format using the dedicated button.
*   Allow downloading the plot in PNG format using customizable width and height settings.

#### How to Interpret the Results
*   **Chi-Squared Test** : The test is used when the expected frequencies in each cell are large enough (typically greater than 5). If the p-value is less than 0.05, it indicates a significant association between the variables.
*   **Fisher's Exact Test** : This test is used when the expected frequencies are too low for the Chi-Squared test to be valid. If the p-value is less than 0.05, there is a significant association.
*   **Mosaic Plot (Fisher’s Test)** : The size of the tiles indicates the frequency of observations. Shading highlights significant deviations from independence.
*   **Association Plot (Chi-Squared Test)** : Shows the deviations from independence in the form of colored cells, with blue representing positive residuals and red representing negative residuals.

#### License
This work **© 2025 by Guillaume Desagulier** is licensed under **CC BY-NC 4.0**.

#### Acknowledgements
The `vcd` package is used for visualizing contingency table results through association and mosaic plots. `shiny` was used to create the interactive web application. The `rmarkdown` package is used to generate downloadable HTML reports.
```