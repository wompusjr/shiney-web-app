library(shiny)
library(ggplot2)
library(broom)

ui <- fluidPage(
  titlePanel("Use Linear Regression to Explore Factors Affecting Grocery Store Closures in DC"),

  sidebarLayout(
    sidebarPanel(
      helpText("This app uses the dredge() function and Grocery Store Locations dataset from https://catalog.data.gov/dataset/grocery-store-locations to explore how different predictors influence whether grocery stores closed between 2022 and 2025. The factors chosen for this test were a store's zipcode, ward, if it was part of the 29 largest US grocery brands, and if it was open prior to 2022. The data was cleaned and subsetted but that is recorded in ./data/scratchpad.R in case you're curious"),

      checkboxGroupInput("selected_predictors",
                         "Select predictor variables to include in your model:",
                         choices = c("ZIPCODE", "WARD", "BRAND", "PREDATE")),

      actionButton("run_model", "Run Model"),
      helpText("Click 'Run Model' to fit a logistic regression model comparing a store's open status to the selected factors."),

      actionButton("lock_best_model", "Lock Best Model"),
      helpText("Lock the model with the lowest AICc value in the Model Comparison section"),

      actionButton("reset_models", "Reset Model History"),
      helpText("Click this button to reset the list of models in the Model Comparison section (but keep the locked model if set).")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Model Overview",
                 h4("Model Summary"),
                 helpText("This shows the logistic regression output including the coefficients, p-values, and overall model fit. The AICc is displayed at the bottom"),
                 verbatimTextOutput("model_summary"),
                 textOutput("aic_value")
        ),
        tabPanel("Coefficient Plot",
                 h4("Visualize Coefficient Estimates"),
                 helpText("The bar plot shows the effects of each factor on the likelihood of store closure between the selected dates. Positive values increase the probability, negative values decrease them."),
                 plotOutput("coef_plot")
        ),
        tabPanel("Model Comparison (ΔAICc)",
                 h4("Track AICc Changes Over Time"),
                 helpText("This plot shows how the AICc values change with each model. The best model (lowest AICc) is highlighted in green."),
                 plotOutput("delta_aicc_plot")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  #loading data
  clean_d <- read.csv("./data/Cleaned_Grocery.csv")  # Make sure to specify the correct path

  #safety check
  if (is.null(clean_d) || nrow(clean_d) == 0) {
    stop("Data could not be loaded or is empty.")
  }

  #creating variables
  model_history <- reactiveVal(data.frame(
    model_id = character(),
    predictors = character(),
    AICc = numeric(),
    stringsAsFactors = FALSE
  ))

  locked_model <- reactiveVal(NULL)

  observeEvent(input$run_model, {
    req(input$selected_predictors)

    formula_text <- paste("CLOSED ~", paste(input$selected_predictors, collapse = " + "))
    model <- glm(as.formula(formula_text), data = clean_d, family = "binomial")

    # Save the model details in history
    history <- model_history()
    new_entry <- data.frame(
      model_id = paste0("Model_", nrow(history) + 1),
      predictors = paste(input$selected_predictors, collapse = " + "),
      AICc = AIC(model)
    )
    model_history(rbind(history, new_entry))
  })

  model_result <- reactive({
    req(input$selected_predictors)
    glm(as.formula(paste("CLOSED ~", paste(input$selected_predictors, collapse = " + "))),
        data = clean_d, family = "binomial")
  })

  output$model_summary <- renderPrint({
    req(model_result())
    summary(model_result())
  })

  output$aic_value <- renderText({
    req(model_result())
    paste("AIC:", round(AIC(model_result()), 2))
  })

  output$coef_plot <- renderPlot({
    req(model_result())
    coef_df <- broom::tidy(model_result())
    coef_df <- subset(coef_df, term != "(Intercept)")

    ggplot(coef_df, aes(x = reorder(term, estimate), y = estimate)) +
      geom_col(fill = "steelblue") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      coord_flip() +
      labs(
        title = "Coefficient Estimates for Each Predictor",
        x = "Predictor",
        y = "Log-Odds Estimate"
      )
  })

  observeEvent(input$lock_best_model, {
    history <- model_history()
    req(nrow(history) > 0)
    best_model <- history[which.min(history$AICc), ]
    locked_model(best_model)
  })

  observeEvent(input$reset_models, {
    locked <- locked_model()
    if (!is.null(locked)) {
      model_history(locked)
    } else {
      model_history(data.frame(
        model_id = character(),
        predictors = character(),
        AICc = numeric(),
        stringsAsFactors = FALSE
      ))
    }
  })

  output$delta_aicc_plot <- renderPlot({
    history <- model_history()
    req(nrow(history) > 0)

    min_aicc <- min(history$AICc)
    history$DeltaAICc <- history$AICc - min_aicc
    history$is_locked <- FALSE

    locked <- locked_model()
    if (!is.null(locked)) {
      lock_id <- locked$model_id
      if (lock_id %in% history$model_id) {
        history$is_locked[history$model_id == lock_id] <- TRUE
      }
    }

    ggplot(history, aes(x = seq_along(model_id), y = DeltaAICc, color = is_locked)) +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      geom_text(aes(label = paste0("ΔAICc=", round(DeltaAICc, 1))), vjust = -1.2, size = 3.5) +
      scale_color_manual(values = c("FALSE" = "darkred", "TRUE" = "forestgreen"), guide = FALSE) +
      scale_x_continuous(breaks = seq_along(history$model_id), labels = history$model_id) +
      labs(
        title = "Model Comparison: ΔAICc Across Models (Best Model in Green)",
        x = "Model",
        y = "ΔAICc (Relative to Best)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui, server)
