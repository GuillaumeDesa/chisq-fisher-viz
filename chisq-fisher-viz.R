# Required Libraries
if (!require("shiny")) install.packages("shiny")
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("shinycssloaders")) install.packages("shinycssloaders")
if (!require("vcd")) install.packages("vcd")

library(shiny)
library(openxlsx)
library(ggplot2)
library(shinycssloaders)
library(vcd)

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("X² / Fisher's Exact Tests of Independence"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Frequency Table", 
                accept = c(".xlsx", ".xls", ".csv", ".txt")),
      helpText("Upload a file with a frequency table. The first row should contain column headers, 
               and the first column should contain row names."),
      br(),
      actionButton("submit", "Submit"),
      br(), br(),
      uiOutput("p_value"), # Render p-value and test result dynamically
      br(),
      uiOutput("test_message"), # Display which test is being used and why
      br(), br(),
      # Add copyright note
      div(
        HTML("This work © 2024 by Guillaume Desagulier is licensed under 
              <a href='https://creativecommons.org/licenses/by-nc/4.0/' target='_blank'>CC BY-NC 4.0</a>."),
        style = "font-size: 0.9em; margin-top: 20px;"
      ),
      div(
        HTML("<a href='https://github.com/GuillaumeDesa/chisq-fisher-viz' target='_blank'>GitHub Repository</a>"),
        style = "font-size: 0.9em;"
      )
    ),
    
    mainPanel(
      # Add CSS to control the plot size
      tags$head(
        tags$style(HTML("
          #plot-container {
            height: 600px; /* Default height for the plot container */
            overflow-y: auto; /* Allow vertical scrolling if the plot is too large */
          }
          #plot-container + .interpretation {
            margin-top: -10px; /* Slightly reduce space between plot and interpretation text */
          }
        "))
      ),
      
      # Flexible plot container
      div(
        id = "plot-container",
        withSpinner(plotOutput("plot", height = "500px")) # Reserve height for larger plots
      ),
      
      br(),
      uiOutput("plot_interpretation"), # Add comments for plot interpretation
      br(),
      uiOutput("tables") # Render observed, expected, and residual tables dynamically
    )
  )
)

# Server-side logic
server <- function(input, output, session) {
  
  observeEvent(input$submit, {
    req(input$file) # Ensure a file is uploaded
    
    tryCatch({
      # File processing logic
      ext <- tools::file_ext(input$file$name)
      if (ext %in% c("xlsx", "xls")) {
        data <- read.xlsx(input$file$datapath, sheet = 1, rowNames = TRUE)
      } else if (ext == "csv") {
        data <- read.csv(input$file$datapath, row.names = 1)
      } else if (ext == "txt") {
        data <- read.table(input$file$datapath, sep = "\t", header = TRUE, row.names = 1)
      } else {
        stop("Unsupported file format. Please upload .xlsx, .xls, .csv, or .txt.")
      }
      
      # Convert data to matrix
      freq_table <- as.matrix(data)
      
      # Check for empty rows/columns
      if (any(rowSums(freq_table) == 0) || any(colSums(freq_table) == 0)) {
        stop("Input data contains empty rows or columns. Please clean the data and try again.")
      }
      
      # Perform chi-squared test
      chi_squared_result <- chisq.test(freq_table)
      chisq.score <- chi_squared_result$statistic # Chi-squared test statistic
      observed <- chi_squared_result$observed # Observed frequencies
      expected <- chi_squared_result$expected # Expected frequencies
      residuals <- chi_squared_result$residuals # Pearson residuals
      
      # Message for the test used
      test_message <- ""
      if (any(chi_squared_result$expected < 5)) {
        # Use Fisher's Exact Test if expected counts are too low
        fisher_result <- fisher.test(freq_table)
        p_value <- fisher_result$p.value
        plot_type <- "fisher"
        output$p_value <- renderUI({
          if (p_value < 0.05) {
            HTML(paste(
              "p-value (Fisher's Exact Test):", round(p_value, 3), "<br>",
              "<strong>Interpretation:</strong> The p-value is below the significance level (α = 0.05), indicating that we reject the null hypothesis of independence. There is a statistically significant association between the variables."
            ))
          } else {
            HTML(paste(
              "p-value (Fisher's Exact Test):", round(p_value, 3), "<br>",
              "<strong>Interpretation:</strong> The p-value is above the significance level (α = 0.05), indicating that we fail to reject the null hypothesis. There is no statistically significant association between the variables."
            ))
          }
        })
        
        # Set message for Fisher's Exact Test
        test_message <- "Fisher's Exact Test was used instead of X² because the expected frequencies are too low."
        
        # Render tables for observed and expected frequencies for Fisher's Test
        output$tables <- renderUI({
          fluidRow(
            column(6, h4("Observed frequencies"), tableOutput("fisher_observed_table")),
            column(6, h4("Expected frequencies"), tableOutput("fisher_expected_table"))
          )
        })
        
        # Define the table outputs for Fisher's test
        output$fisher_observed_table <- renderTable({
          observed
        }, rownames = TRUE)
        
        output$fisher_expected_table <- renderTable({
          round(expected, 2) # Round for better readability
        }, rownames = TRUE)
        
      } else {
        # Use Chi-Squared Test otherwise
        p_value <- chi_squared_result$p.value
        plot_type <- "chisq"
        output$p_value <- renderUI({
          if (p_value < 0.05) {
            HTML(paste(
              "p-value:", round(p_value, 3), "<br>",
              "X² score:", round(chisq.score, 2), "<br>",
              "<strong>Interpretation:</strong> The p-value is below the significance level (α = 0.05), indicating that we reject the null hypothesis of independence. There is a statistically significant association between the variables."
            ))
          } else {
            HTML(paste(
              "p-value:", round(p_value, 3), "<br>",
              "X² score:", round(chisq.score, 2), "<br>",
              "<strong>Interpretation:</strong> The p-value is above the significance level (α = 0.05), indicating that we fail to reject the null hypothesis. There is no statistically significant association between the variables."
            ))
          }
        })
        
        # Set message for Chi-Squared Test
        test_message <- "X² Test was used as the expected frequencies are sufficiently large."
        
        # Render tables for observed, expected, and residuals
        output$tables <- renderUI({
          fluidRow(
            column(4, h4("Observed frequencies"), tableOutput("observed_table")),
            column(4, h4("Expected frequencies"), tableOutput("expected_table")),
            column(4, h4("Pearson residuals"), tableOutput("residuals_table"))
          )
        })
        
        # Define the table outputs
        output$observed_table <- renderTable({
          observed
        }, rownames = TRUE)
        
        output$expected_table <- renderTable({
          round(expected, 2) # Round for better readability
        }, rownames = TRUE)
        
        output$residuals_table <- renderTable({
          round(residuals, 2) # Round for better readability
        }, rownames = TRUE)
      }
      
      # Display the test message in the sidebar
      output$test_message <- renderUI({
        HTML(paste(test_message))
      })
      
      # Create the plot
      output$plot <- renderPlot({
        if (plot_type == "chisq") {
          # Association plot (Chi-Squared Test)
          assoc(freq_table, shade = TRUE, labeling = labeling_values, main = "Association Plot")
        } else if (plot_type == "fisher") {
          # Mosaic plot (Fisher's Exact Test) with color
          mosaic(~ ., 
                 data = as.table(freq_table), 
                 shade = TRUE, 
                 main = "Mosaic Plot (Fisher's Exact Test)", 
                 labeling = labeling_values,
                 gp = shading_Friendly, 
                 split_vertical = FALSE)  # Adds color based on shading
        }
      })
      
      # Add comments on plot interpretation
      output$plot_interpretation <- renderUI({
        if (plot_type == "chisq") {
          HTML("<strong>Association Plot Interpretation:</strong> 
                <ul>
                  <li>Shaded cells indicate significant deviations from expected frequencies under the assumption of independence.</li>
                  <li>Cells shaded in darker colors suggest stronger deviations.</li>
                  <li>The direction of residuals (positive or negative) determines whether observed frequencies are higher or lower than expected.</li>
                </ul>")
        } else if (plot_type == "fisher") {
          HTML("<strong>Mosaic Plot Interpretation:</strong> 
                <ul>
                  <li>Each tile represents a cell in the contingency table, with its area proportional to the observed frequency.</li>
                  <li>Shading highlights significant differences between observed and expected frequencies.</li>
                  <li>Pay attention to tiles with strong shading, as they indicate associations between variables.</li>
                </ul>")
        }
      })
      
    }, error = function(e) {
      # Show an error modal on failure
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
