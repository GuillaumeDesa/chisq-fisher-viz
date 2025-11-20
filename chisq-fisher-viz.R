# Required Libraries (add rmarkdown)
if (!require("shiny")) install.packages("shiny")
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("shinycssloaders")) install.packages("shinycssloaders")
if (!require("vcd")) install.packages("vcd")
if (!require("rmarkdown")) install.packages("rmarkdown")

library(shiny)
library(openxlsx)
library(ggplot2)
library(shinycssloaders)
library(vcd)
library(rmarkdown)

# Get the current year for automatic copyright update (Fix b)
current_year <- format(Sys.Date(), "%Y")

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
        /* Remove fixed height, let plotOutput handle it dynamically */
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
      helpText("Upload a file with a frequency table (xlsx, xls, csv, or txt). The first row should contain column headers, and the first column should contain row names."),
      br(),
      actionButton("submit", "Submit"),
      br(),
      br(),
      # --- DOWNLOAD BUTTONS ---
      downloadButton("downloadReport", "Download Full Report (.html)"),
      br(),
      br(),
      h4("Export Plot (PNG)"),
      uiOutput("png_width_input"),
      uiOutput("png_height_input"),
      downloadButton("downloadPlotPNG", "Download Plot (.png)"),
      br(),
      br(),
      # ----------------------------------
      uiOutput("p_value"),
      br(),
      uiOutput("test_message"),
      br(),
      br(),
      div(
        # Updated copyright year dynamically (Fix b)
        HTML(paste0("This work © ", current_year, " by Guillaume Desagulier is licensed under <a href='https://creativecommons.org/licenses/by-nc/4.0/' target='_blank'>CC BY-NC 4.0</a>.")),
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
        withSpinner(plotOutput("plot"))
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
  
  # Reactive value to store all necessary results for rendering and export
  results <- reactiveVal(NULL)
  
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
      
      # --- DYNAMIC PLOT HEIGHT/WIDTH CALCULATION ---
      num_rows <- nrow(freq_table)
      num_cols <- ncol(freq_table)
      
      base_height <- 300
      row_factor <- 50 * max(1, num_rows - 1)
      col_factor <- 30 * max(1, num_cols - 1)
      plot_height_px <- max(base_height, base_height + row_factor + col_factor)
      
      plot_width_px <- 500 + 50 * num_cols
      
      plot_height_in <- plot_height_px / 72
      plot_width_in <- plot_width_px / 72
      # ----------------------------------------
      
      chi_squared_result <- chisq.test(freq_table)
      observed <- chi_squared_result$observed
      expected <- chi_squared_result$expected
      residuals <- chi_squared_result$residuals
      
      current_results <- list(
        freq_table = freq_table,
        observed = observed,
        expected = expected,
        residuals = residuals,
        plot_height_px = plot_height_px,
        plot_width_px = plot_width_px,
        plot_height_in = plot_height_in,
        plot_width_in = plot_width_in,
        p_value = NA,
        test_message = "",
        test_type = "",
        p_interpretation = "",
        plot_interpretation = ""
      )
      
      if (any(chi_squared_result$expected < 5)) {
        fisher_result <- fisher.test(freq_table)
        p_value <- fisher_result$p.value
        test_message <- "Fisher's Exact Test was used instead of X² because the expected frequencies are too low."
        current_results$p_value <- p_value
        current_results$test_message <- test_message
        current_results$test_type <- "Fisher"
        
        if (p_value < 0.05) {
          p_interpretation <- paste(
            "p-value (Fisher's Exact Test):", round(p_value, 3), "<br>",
            "<strong>Interpretation:</strong> The p-value is below the significance level (α = 0.05), indicating that we reject the null hypothesis of independence. There is a statistically significant association between the variables."
          )
        } else {
          p_interpretation <- paste(
            "p-value (Fisher's Exact Test):", round(p_value, 3), "<br>",
            "<strong>Interpretation:</strong> The p-value is above the significance level (α = 0.05), indicating that we fail to reject the null hypothesis. There is no statistically significant association between the variables."
          )
        }
        current_results$p_interpretation <- p_interpretation
        
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
        
        current_results$plot_interpretation <- "<strong>Mosaic Plot Interpretation:</strong> <ul> <li>Each tile represents a cell in the contingency table, with its area proportional to the observed frequency.</li> <li>Shading highlights significant differences between observed and expected frequencies.</li> </ul>"
        
      } else {
        p_value <- chi_squared_result$p.value
        test_message <- "X² Test was used as the expected frequencies are sufficiently large."
        current_results$p_value <- p_value
        current_results$test_message <- test_message
        current_results$test_type <- "ChiSq"
        
        if (p_value < 0.05) {
          p_interpretation <- paste(
            "p-value:", round(p_value, 3), "<br>",
            "<strong>Interpretation:</strong> The p-value is below the significance level (α = 0.05), indicating that we reject the null hypothesis of independence. There is a statistically significant association between the variables."
          )
        } else {
          p_interpretation <- paste(
            "p-value:", round(p_value, 3), "<br>",
            "<strong>Interpretation:</strong> The p-value is above the significance level (α = 0.05), indicating that we fail to reject the null hypothesis. There is no statistically significant association between the variables."
          )
        }
        current_results$p_interpretation <- p_interpretation
        
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
        
        current_results$plot_interpretation <- "<strong>Association Plot Interpretation:</strong> <ul> <li>Shaded cells indicate significant deviations from expected frequencies under the assumption of independence.</li> <li>The direction of residuals (positive or negative) determines whether observed frequencies are higher or lower than expected.</li> </ul>"
        
      }
      
      # UI elements that depend on calculation
      output$p_value <- renderUI({ HTML(p_interpretation) })
      output$test_message <- renderUI({ HTML(paste(test_message)) })
      output$plot_interpretation <- renderUI({ HTML(current_results$plot_interpretation) })
      
      output$plot <- renderPlot({
        if (current_results$test_type == "Fisher") {
          mosaic(~ ., data = as.table(freq_table), shade = TRUE, main = "Mosaic Plot (Fisher's Exact Test)")
        } else {
          assoc(freq_table, shade = TRUE, labeling = labeling_values, main = "Association Plot")
        }
      }, height = plot_height_px)
      
      # PNG export size inputs (use dynamic inches for defaults)
      output$png_width_input <- renderUI({
        numericInput("png_width", "Width (inches):", 
                     value = round(current_results$plot_width_in, 2), min = 2, step = 0.5)
      })
      output$png_height_input <- renderUI({
        numericInput("png_height", "Height (inches):", 
                     value = round(current_results$plot_height_in, 2), min = 2, step = 0.5)
      })
      
      # Store results for the download handler
      results(current_results) 
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
      results(NULL)
    })
  })
  
  # --- DOWNLOAD HANDLER FOR REPORT (HTML) ---
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("Statistical_Report-", Sys.Date(), ".html", sep="")
    },
    content = function(file) {
      req(results())
      
      # Prepare interpretation as pure Markdown/text by stripping HTML tags
      clean_interpretation <- function(html_string) {
        clean <- gsub("<strong>(.*?)</strong>", "\\1", html_string, perl=TRUE)
        clean <- gsub("<ul>", "", clean)
        clean <- gsub("</ul>", "", clean)
        clean <- gsub("<li>", "* ", clean)
        clean <- gsub("</li>", "\n", clean)
        clean <- gsub("<br>", "\n", clean)
        return(trimws(clean))
      }
      
      p_interp_markdown <- clean_interpretation(results()$p_interpretation)
      
      tempReport <- file.path(tempdir(), "report.Rmd")
      
      # FIX (a): The plot visualization and interpretation section is removed entirely from the Rmd template.
      writeLines(c(
        "---",
        "title: 'Test of Independence Report'",
        paste0("date: '", Sys.Date(), "'"),
        "output: html_document",
        "params:",
        "  results: NULL", 
        "---",
        
        "## Statistical Test Results",
        paste("### Test Performed:", results()$test_message),
        paste("### P-value:", round(results()$p_value, 4)),
        
        "### P-value Interpretation",
        p_interp_markdown, 
        
        "\n",
        "## Frequency Tables",
        "### Observed Frequencies",
        "```",
        paste(capture.output(results()$observed), collapse = "\n"),
        "```",
        
        "### Expected Frequencies",
        "```",
        paste(capture.output(round(results()$expected, 2)), collapse = "\n"),
        "```",
        
        if (results()$test_type == "ChiSq") {
          c(
            "### Pearson Residuals",
            "```",
            paste(capture.output(round(results()$residuals, 2)), collapse = "\n"),
            "```"
          )
        }
        # Plot and interpretation sections are now omitted.
        
      ), con = tempReport)
      
      # Render the Rmd to HTML
      rmarkdown::render(tempReport, output_file = file,
                        params = list(results = results()), 
                        envir = new.env(parent = globalenv()))
    }
  )
  
  # --- DOWNLOAD HANDLER FOR PLOT (PNG) ---
  output$downloadPlotPNG <- downloadHandler(
    filename = function() {
      paste("Association_Plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      req(results(), input$png_width, input$png_height)
      
      # Use ggsave parameters (width/height in inches) to generate the PNG
      # This handles custom margins/sizing based on user input
      png(file, 
          width = input$png_width, 
          height = input$png_height, 
          units = "in", 
          res = 300) # Use 300 DPI for high resolution
      
      # Plotting logic based on test type
      freq_table <- results()$freq_table
      if (results()$test_type == "Fisher") {
        mosaic(~ ., data = as.table(freq_table), shade = TRUE, main = "Mosaic Plot")
      } else {
        assoc(freq_table, shade = TRUE, labeling = labeling_values, main = "Association Plot")
      }
      
      dev.off() # Close the PNG device
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)