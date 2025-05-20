# Lifestyle Factors & REM Sleep – Statistical Modeling Project

This project explores how lifestyle behaviors (e.g., caffeine, alcohol, smoking, and exercise) impact REM sleep percentage using multiple linear regression models in R.

## 🔍 Project Summary<br>
- **Dataset:** 400+ observations on sleep and lifestyle habits
- **Goal:** Identify which lifestyle factors significantly affect REM sleep
- **Method:** Built and compared 5 linear regression models; tuned variables and removed outliers to improve model fit (adjusted R² from 0.10 to 0.17)
- **Tools Used:** R, ggplot2, dplyr, car, MASS, mgcv, Shiny

## 📂 Repository Structure
`app.R`: R Shiny app for interactive model visualization<br>
`Sleep_Efficiency.csv`: Cleaned dataset<br>
`Sleep_Efficiency (Revised).Rmd`: R Markdown statistical report<br>
`Final Project Report.pdf`: Final write-up (formatted version)<br>

## 📊 Live App<br>
👉 [Shiny App](https://ellenlee.shinyapps.io/REM_ShinyApp)

## 📎 Report & Analysis<br>
📘 [Final Project Report (PDF)](link-to-pdf-if-hosted)  
📗 [R Markdown Report](Sleep_Efficiency%20(Revised).Rmd)

## 💻 How to Run
1. Clone the repo:
   ```bash
   git clone https://github.com/Ellen0120/REM-sleep-analysis.git
Open app.R in RStudio

Click “Run App” to launch the Shiny dashboard

✨ Key Takeaways
Converted non-linear variables into categorical factors to improve interpretability

Diagnosed and removed influential outliers using Cook's distance and leverage

Published interactive visualizations for better accessibility

