## ---------------------------------Package-----------------------------------##
# install.packages("shiny")
# install.packages("DT")
library(shiny)
library(ggplot2)  
library(readr)                                                                  # Read the csv file
library(ggplot2)                                                                # Pie chart, box plot, scatter plot
library(dplyr)                                                                  # data process
library(DT)                                                                     # Show data table
library(car)                                                                    # Levene's test, influence plot
library(DescTools)                                                              # The Spearmam's correlation
library(lmtest)                                                                 # bptest
library(MASS)                                                                   # Robust regression model
library(mgcv)                                                                   # GAM 
library(broom)                                                                  # for tidy() / glance()library(rsconnect)
## ------------------------------LOAD DATASET---------------------------------##
sleep_data <- read.csv("Sleep_Efficiency.csv") |>
  rename(
    REM = REM.sleep.percentage,
    Caffeine = Caffeine.consumption,
    Alcohol = Alcohol.consumption,
    Smoking = Smoking.status,
    Exercise = Exercise.frequency
  )
sleep_new <- sleep_data[, c("ID", "Age", "Gender", "REM", 
                            "Caffeine", "Alcohol", 
                            "Smoking", "Exercise")]                             # Choose the data we need for the RQ and build a new data frame

## -----------------------------MISSING VALUE---------------------------------##
sleep_new$ Caffeine[is.na(sleep_new$Caffeine)] <- median(sleep_new$Caffeine, na.rm = TRUE)          # Insert the median to the missing value (caffeine)
sleep_new$ Alcohol[is.na(sleep_new$Alcohol)] <- median(sleep_new$Alcohol, na.rm = TRUE)             # Insert the median to the missing value (alcohol)
sleep_new$ Exercise[is.na(sleep_new$Exercise)] <- median(sleep_new$Exercise, na.rm = TRUE)          # Insert the median to the missing value (Exercise freq)

## ------------------------------CATEGORICAL----------------------------------##
sleep_new$Gender <- as.factor(sleep_new$Gender)                 
sleep_new$Smoking <- as.factor(sleep_new$Smoking)               
sleep_new$Exercise <- as.factor(sleep_new$Exercise)  

## -----------------------------USER INTERFACE--------------------------------##

ui <- fluidPage(
  titlePanel("Exploring the Impact of Lifestyle Factors on REM Sleep Percentage:
A Multiple Linear Regression Approach"),                                        # Add webpage title
  
  tabPanel("1. Data Introduction",                                              ### Tab 1: Data Introduction
           h4("1-1. Raw Data"),                                                 ## Section 1-1: Raw data
           DT::dataTableOutput("data_table"),                                   # Show Raw data     
           
           
           h4("1-2. Explore a Variable"),                                       ## Section 1-2: Explore a Variable
           selectInput("var_explore", "Select a variable to view full analysis", 
                       choices = c("REM", "Caffeine", "Alcohol", "Age", 
                                   "Gender", "Smoking", "Exercise")),           # Drop down menu
           h5("1-2-1. Data Type"),                                              # Section 1-2-1: Data type
           verbatimTextOutput("var_type"),                                      # Show Data Type
           
           h5("1-2-2. Summary Statistics"),                                     # Section 1-2-2: Summary statistics
           verbatimTextOutput("var_summary_single"),                            # Show summary statistics
           
           h5("1-2-3. Normality (Histogram & Shapiro-Wilk Test)"),              # Section 1-2-3: Normality (Histogram & Shapiro-Wilk Test)
           plotOutput("hist_plot"),                                             # Histogram
           verbatimTextOutput("shapiro_result"),                                # Shapiro-wilk test result
           
           h5("1-2-4.Outliers (Boxplot)"),                                      # Section 1-2-4: Outliers (Boxplot)
           plotOutput("box_plot"),                                              # Boxplot
           verbatimTextOutput("outliers_list")                                  # Outliers list
  ),
  tabPanel("2. Univariate Linear Model",                                        ### Tab 2: Univariate Linear Model
           selectInput("uni_var", "Select a predictor:", 
                       choices = c("Age", "Gender", "Smoking",
                                   "Exercise", "Caffeine", "Alcohol")),         ## Drop down menu

           h4("2-1. Linear Regression Summary"),                                ## Section 2-1: Linear Regression Summary
           tableOutput("uni_lm_summary"),                                       
           
           h4("2-2. Plot"),                                                     ## Section 2-2: Plot
           plotOutput("uni_plot"),
           
           h4("2-3. Nonparametric Test"),                                       ## Section 2-3: Nonparametric Test
           verbatimTextOutput("nonparam_result"),
           
           h4("2-4. Conclusion"),                                               ## Section 2-4: Conclusion
           verbatimTextOutput("uni_conclusion")
  ),
  tabPanel("3. Model Comparison",                                               ### Tab 3:
           h4("Select Model to View Details"),                                  # Drop down menu
           selectInput("model_select", "Model:", 
                       choices = c("M1: Raw", "M2: Removed Influential", 
                                   "M3: Log REM", "M4: Log X", "M5: GAM", "M6: Stepwise(M3)")),
           
           h4("3-1. Model Fit Comparison"),                                     # Section 3-1: Model fit comparison
           tableOutput("model_metrics"),
           
           h4("3-2. Coefficient Estimates"),                                    # Section 3-2: Coefficient estimates
           DTOutput("model_coef"),                                              # Interactive table output
           
           h4("3-3. Interpretation"),                                           # Section 3-3: Interpretation
           verbatimTextOutput("model_interpret")                 
  ),
  tabPanel("4. Data Diagnosis",                                                 ### Tab 4
           selectInput("diagnose_model", "Select a model to diagnose:", 
                       choices =c("M1: Raw", "M2: Removed Influential", 
                                  "M3: Log REM", "M4: Log X", "M5: GAM", "M6: Stepwise(M3)")),
           
           h4("4-2. OLS Assumption Check"),                                     # Section 4-2: OLS Assumption Check
           verbatimTextOutput("ols_check"),
           
           h4("4-3. Summary Conclusion"),                                       # Section 4-3: Summary (Conclusion)
           verbatimTextOutput("diagnosis_conclusion")
  )
)


## ---------------------------------SERVER------------------------------------##
server <- function(input, output) {
  ##### 1-1. Raw data
  output$data_table <- DT::renderDataTable({                                    # Return Table
    DT::datatable(sleep_new,                                                    # Data Table
                  options = list(pageLength = 10, scrollX = TRUE),              # Horizontally scrolling
                  rownames = FALSE)                                             # Hide row names
  })
  
  ##### 1-2. Explore a Variable Data
  output$var_summary_single <- renderPrint({                                    # Data type + summary
    var <- input$var_explore                                                    # If variable = categorical -> table; if variable = continuous -> summary
    values <- sleep_new[[var]]
    if (is.numeric(values)) {
      summary(values)
    } else {
      table(values)
    }
  })                                                                            
  ### 1-2-3. Normality
  # Histogram
  output$hist_plot <- renderPlot({
    var <- input$var_explore
    values <- sleep_new[[var]]
    if (is.numeric(values)) {
      ggplot(sleep_new, aes_string(x = var)) +
        geom_histogram(bins = 10, fill = "skyblue", color = "white") +
        labs(title = paste("Histogram of", var),
             subtitle = "bins = 10",
             x = var, y = "Count")
    }else{
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Histogram is not applicable for categorical variables") +
        theme_void()
    }
  })
  # Shapiro-Wilk
  output$shapiro_result <- renderPrint({
    var <- input$var_explore
    values <- sleep_new[[var]]
    if (is.numeric(values)) {
      values <- values[!is.na(values)]
      if (length(values) >= 3) {
        test <- shapiro.test(values)
        cat(paste0("Shapiro-Wilk Test for ", var, ":\n",
                   "W = ", round(test$statistic, 4), 
                   ", p-value = ", signif(test$p.value, 4)))
        if (test$p.value < 0.05) {
          cat("\n ❌ Samples DID NOT come from a normal distribution (reject H0)\n")
        } else {
          cat("\n ✅ Samples came from a normal distribution (fail to reject H0)\n")
        }
         } else {
        cat("The sample size is too small to perform a Shapiro-Wilk test.")
      }
    } else {
      cat("The Shapiro-Wilk test was not applicable for categorical variables.")
    }
  })
  
  ### 1-2-4. Outliers
  # Boxplot
  output$box_plot <- renderPlot({
    var <- input$var_explore
    if (is.numeric(sleep_new[[var]])) {
      ggplot(sleep_new, aes_string(y = var)) +
        geom_boxplot(fill = "orange") +
        labs(title = paste("Boxplot of", var), y = var)
    } else{ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Boxplot is not applicable for categorical variables") +
        theme_void()
      }
  })
  # Outliers list
  output$outliers_list <- renderPrint({
    var <- input$var_explore
    values <- sleep_new[[var]]
    if (!is.numeric(values)) {
      cat("The outliers was not applicable for categorical variables.")
    } else {
      values <- values[!is.na(values)]
      Q1 <- quantile(values, 0.25)
      Q3 <- quantile(values, 0.75)
      IQR <- Q3 - Q1
      lower <- Q1 - 1.5 * IQR
      upper <- Q3 + 1.5 * IQR
      outliers <- values[values < lower | values > upper]
      if (length(outliers) > 0) {
        cat("Outliers:\n")
        print(outliers)
      } else {
        cat("There is no outliers")
      }
    }
  })
  ##### 2-1: Linear Regression Summary
  output$uni_lm_summary <- renderTable({
    var <- input$uni_var
    formula <- as.formula(paste("REM ~", var))
    model <- lm(formula, data = sleep_new)
    
    summary_df <- broom::tidy(model, conf.int = TRUE)
    rsq <- summary(model)$r.squared
    
    summary_df <- summary_df[, c("term", "estimate", "conf.low", "conf.high", "p.value")]
    summary_df$R_squared <- c(round(rsq, 3), rep(NA, nrow(summary_df)-1))   
    summary_df
  })

 ##### 2-2: Plot
output$uni_plot <- renderPlot({
  var <- input$uni_var
  
  if (is.numeric(sleep_new[[var]])) {
    # Numerical：Scatter + LM line
    ggplot(sleep_new, aes_string(x = var, y = "REM")) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(title = paste("REM vs", var), x = var, y = "REM (%)")
  } else {
    # Categorical: Boxplot
    ggplot(sleep_new, aes_string(x = var, y = "REM")) +
      geom_boxplot(fill = "orange") +
      labs(title = paste("REM by", var), x = var, y = "REM (%)")
  }
})
 ##### 2-3: Nonparametric
output$nonparam_result <- renderPrint({
  var <- input$uni_var
  values <- sleep_new[[var]]
  
  if (is.numeric(values)) {
    test <- cor.test(sleep_new[[var]], sleep_new$REM, method = "spearman", exact = FALSE)
    cat("Spearman correlation:\n")
    print(test)
  } else {
    test <- wilcox.test(REM ~ get(var), data = sleep_new)
    cat("Mann-Whitney U Test:\n")
    print(test)
  }
})
 ##### 2-4: Conclusion
output$uni_conclusion <- renderPrint({
  var <- input$uni_var
  model <- lm(as.formula(paste("REM ~", var)), data = sleep_new)
  pval <- summary(model)$coefficients[2, 4]
  
  if (pval < 0.05) {
    cat("✅", var, "is significantly associated with REM (p <", round(pval, 4), ")")
  } else {
    cat("❌", var, "is NOT significantly associated with REM (p =", round(pval, 4), ")")
  }
})
##### 3.  Model Comparison
# Drop down menu
M1 <- lm(REM ~ Age + Gender + Smoking + Exercise + Alcohol + Caffeine, data = sleep_new)

influential_removed <- sleep_new[-c(63, 82, 97, 162, 213, 258), ] |>
  mutate(
    Smoking = droplevels(as.factor(Smoking)),
    Gender = droplevels(as.factor(Gender)),
    Exercise = droplevels(as.factor(Exercise))
  )
M2 <- lm(REM ~ Age + Gender + Smoking + Exercise + Alcohol + Caffeine, 
         data = influential_removed)                                            # M2: Remove Influential Points
M3 <- lm(log(REM) ~ Age + Gender + Smoking + Exercise + Alcohol + Caffeine, 
         data = influential_removed)                                            # M3: log(REM)
M4 <- lm(REM ~ Age + Gender + Exercise + log(Caffeine + 1) + log(Alcohol + 1),  # M4: log(Caffeine+ 1), log(Alcohol+ 1) 
         data = influential_removed)
M5 <- gam(REM ~ Gender + Smoking + Exercise +
    s(Age) + Caffeine + Alcohol, data = influential_removed)                    # M5: GAM
M6 <- stepAIC(M3, direction = "both", trace = FALSE)                            # M6: stepwise(M3)

model_list <- list("M1: Raw" = M1, "M2: Removed Influential" = M2,
  "M3: Log REM" = M3, "M4: Log X" = M4,
  "M5: GAM" = M5, "M6: Stepwise(M3)" = M6)

# 3-1. Model Comparison Table（R², Adj R², AIC, BIC）
output$model_metrics <- renderTable({
  metrics <- lapply(model_list, function(mod) {
    tryCatch({
      rsq <- if ("r.squared" %in% names(summary(mod))) summary(mod)$r.squared else NA
      adjrsq <- if ("adj.r.squared" %in% names(summary(mod))) summary(mod)$adj.r.squared else NA
      devex <- if ("dev.expl" %in% names(summary(mod))) summary(mod)$dev.expl else NA
      
      data.frame(
        R_squared = rsq,
        Adj_R_squared = adjrsq,
        Deviance_Explained = devex,
        AIC = AIC(mod),
        BIC = BIC(mod)
      )
    }, error = function(e) {
      data.frame(R_squared = NA, Adj_R_squared = NA, Deviance_Explained = NA, AIC = NA, BIC = NA)
    })
  })
  
  do.call(rbind, metrics) |> 
    tibble::rownames_to_column("Model")
})
# 3-2. Show coefficient estimates and confidence intervals of the selected model
output$model_coef <- DT::renderDT({
  mod <- model_list[[input$model_select]]
  
  if (inherits(mod, "gam")) {
    coef_table <- summary(mod)$p.table
    df <- as.data.frame(coef_table)
    df$term <- rownames(df)
    df <- df[, c("term", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
    colnames(df) <- c("Term", "Estimate", "Std.Error", "t value", "p value")
  } else {
    df <- broom::tidy(mod, conf.int = TRUE)
    df <- df[, c("term", "estimate", "std.error", "statistic", "p.value")]
    colnames(df) <- c("Term", "Estimate", "Std.Error", "t value", "p value")
  }
  # Find p < 0.05
  highlight_terms <- unique(df[df$`p value` < 0.05, "Term"])
  highlight_colors <- rep("lightyellow", length(highlight_terms))
  
  DT::datatable(df, escape = FALSE, options = list(dom = 't')) |>
    DT::formatStyle(
      "Term",
      target = "row",
      backgroundColor = DT::styleEqual(
        levels = highlight_terms,
        values = rep("lightyellow", length(highlight_terms))
      )
    ) |>
    DT::formatStyle(
      "p value",
      fontWeight = DT::styleInterval(0.05, c("bold", "normal"))
    )
})
# 3-3. Summarize and interpret the selected model’s performance (R², AIC, explanation)
output$model_interpret <- renderPrint({
  mod <- model_list[[input$model_select]]
  s <- summary(mod)
  
  # retrieve the model’s performance
  adjr <- if (!is.null(s$adj.r.squared)) s$adj.r.squared else NA
  devex <- if (!is.null(s$dev.expl)) s$dev.expl else NA
  aic <- tryCatch(AIC(mod), error = function(e) NA)
  
  # print AIC
  cat("AIC =", round(aic, 2), "\n")
  if (!is.na(adjr)) {
    cat("Adjusted R² =", round(adjr, 3), "\n")
  } else if (!is.na(devex)) {
    cat("Deviance Explained (GAM) =", round(devex, 3), "\n")
  } else {
    cat("Model does not provide R² or deviance explained.\n")
  }
  
# 3-4. conclusion
  if (!is.na(adjr) && adjr > 0.05 || !is.na(devex) && devex > 0.05) {
    cat("✅ This model explains part of the variance in REM sleep.\n")
  } else {
    cat("❌ This model has weak explanatory power.\n")
  }
})

##### 4-2. OLS Assumption
output$ols_check <- renderPrint({
  mod <- model_list[[input$diagnose_model]]
  res <- residuals(mod)
  cat("1. Normality of residuals (Shapiro-Wilk test):\n")
  print(shapiro.test(res))
  
  cat("\n2. Homoscedasticity (Breusch-Pagan test):\n")
  if (!requireNamespace("lmtest", quietly = TRUE)) install.packages("lmtest")
  library(lmtest)
  print(bptest(mod))
})
##### 4-3. Conclusion
output$diagnosis_conclusion <- renderPrint({
  mod <- model_list[[input$diagnose_model]]
  res <- residuals(mod)
  
  shap_p <- tryCatch(shapiro.test(res)$p.value, error = function(e) NA)
  bp_p <- tryCatch(bptest(mod)$p.value, error = function(e) NA)
  
  cat("Summary Conclusion for", input$diagnose_model, "\n")
  
  cat("🔹 Normality: ")
  if (!is.na(shap_p) && shap_p < 0.05) {
    cat("❌ Violated (p =", round(shap_p, 4), ")\n")
  } else {
    cat("✅ Satisfied (p =", round(shap_p, 4), ")\n")
  }
  
  cat("🔹 Equal Variance: ")
  if (!is.na(bp_p) && bp_p < 0.05) {
    cat("❌ Violated (p =", round(bp_p, 4), ")\n")
  } else {
    cat("✅ Satisfied (p =", round(bp_p, 4), ")\n")
  }
  cat("🧠 Linearity & Independence: check residual vs fitted plot (not shown here).\n")
})
}
## --------------------------------SHINY APP----------------------------------##
shinyApp(ui = ui, server = server)