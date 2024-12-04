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
  tags$head(
    tags$style(HTML("
      .table-container {
        margin-bottom: 20px;
      }
      .shiny-table {
        width: 100%;
        font-size: 0.9em;
      }
      .shiny-table th, .shiny-table td {
        padding: 5px;
        word-wrap: break-word;
        max-width: 150px;
      }
      #plot-container {
        height: 600px;
        overflow-y: auto;
      }
      #plot-container + .interpretation {
        margin-top: -10px;
      }
    "))
  ),
  titlePanel("X² / Fisher's Exact Tests of Independence"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Frequency Table", accept = c(".xlsx", ".xls", ".csv", ".txt")),
      helpText("Upload a file with a frequency table. The first row should contain column headers, and the first column should contain row names."),
      br(),
      actionButton("submit", "Submit"),
      br(),
      br(),
      uiOutput("p_value"),
      br(),
      uiOutput("test_message"),
      br(),
      br(),
      div(
        HTML("This work © 2024 by Guillaume Desagulier is licensed under <a href='https://creativecommons.org/licenses/by-nc/4.0/' target='_blank'>CC BY-NC 4.0</a>."),
        style = "font-size: 0.9em; margin-top: 20px;"
      ),
      div(
        HTML("<a href='https://github.com/GuillaumeDesa/chisq-fisher-viz' target='_blank'>GitHub Repository</a>"),
        style = "font-size: 0.9em;"
      )
    ),
    mainPanel(
      div(
        id = "plot-container",
        withSpinner(plotOutput("plot", height = "500px"))
      ),
      br(),
      uiOutput("plot_interpretation"),
      br(),
      uiOutput("tables")
    )
  )
)

# Server-side logic
server <- function(input, output, session) {
  observeEvent(input$submit, {
    req(input$file)
    tryCatch({
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
      
      freq_table <- as.matrix(data)
      
      if (any(rowSums(freq_table) == 0) || any(colSums(freq_table) == 0)) {
        stop("Input data contains empty rows or columns. Please clean the data and try again.")
      }
      
      chi_squared_result <- chisq.test(freq_table)
      observed <- chi_squared_result$observed
      expected <- chi_squared_result$expected
      residuals <- chi_squared_result$residuals
      
      if (any(chi_squared_result$expected < 5)) {
        fisher_result <- fisher.test(freq_table)
        p_value <- fisher_result$p.value
        test_message <- "Fisher's Exact Test was used instead of X² because the expected frequencies are too low."
        
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
        
        output$tables <- renderUI({
          div(
            class = "table-container",
            fluidRow(
              column(6, h4("Observed Frequencies"), tableOutput("observed_table")),
              column(6, h4("Expected Frequencies"), tableOutput("expected_table"))
            )
          )
        })
        output$observed_table <- renderTable({ observed }, rownames = TRUE)
        output$expected_table <- renderTable({ round(expected, 2) }, rownames = TRUE)
      } else {
        p_value <- chi_squared_result$p.value
        test_message <- "X² Test was used as the expected frequencies are sufficiently large."
        
        output$p_value <- renderUI({
          if (p_value < 0.05) {
            HTML(paste(
              "p-value:", round(p_value, 3), "<br>",
              "<strong>Interpretation:</strong> The p-value is below the significance level (α = 0.05), indicating that we reject the null hypothesis of independence. There is a statistically significant association between the variables."
            ))
          } else {
            HTML(paste(
              "p-value:", round(p_value, 3), "<br>",
              "<strong>Interpretation:</strong> The p-value is above the significance level (α = 0.05), indicating that we fail to reject the null hypothesis. There is no statistically significant association between the variables."
            ))
          }
        })
        
        output$tables <- renderUI({
          div(
            class = "table-container",
            fluidRow(
              column(6, h4("Observed Frequencies"), tableOutput("observed_table")),
              column(6, h4("Expected Frequencies"), tableOutput("expected_table"))
            ),
            br(),
            fluidRow(
              column(12, h4("Pearson Residuals"), tableOutput("residuals_table"))
            )
          )
        })
        output$observed_table <- renderTable({ observed }, rownames = TRUE)
        output$expected_table <- renderTable({ round(expected, 2) }, rownames = TRUE)
        output$residuals_table <- renderTable({ round(residuals, 2) }, rownames = TRUE)
      }
      
      output$test_message <- renderUI({ HTML(paste(test_message)) })
      
      output$plot <- renderPlot({
        if (any(chi_squared_result$expected < 5)) {
          mosaic(~ ., data = as.table(freq_table), shade = TRUE, main = "Mosaic Plot (Fisher's Exact Test)")
        } else {
          assoc(freq_table, shade = TRUE, labeling = labeling_values, main = "Association Plot")
        }
      })
      
      output$plot_interpretation <- renderUI({
        if (any(chi_squared_result$expected < 5)) {
          HTML("<strong>Mosaic Plot Interpretation:</strong> <ul> <li>Each tile represents a cell in the contingency table, with its area proportional to the observed frequency.</li> <li>Shading highlights significant differences between observed and expected frequencies.</li> </ul>")
        } else {
          HTML("<strong>Association Plot Interpretation:</strong> <ul> <li>Shaded cells indicate significant deviations from expected frequencies under the assumption of independence.</li> <li>The direction of residuals (positive or negative) determines whether observed frequencies are higher or lower than expected.</li> </ul>")
        }
      })
    }, error = function(e) {
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
