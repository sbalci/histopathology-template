library("shiny")
library("dplyr")
library("readxl")
library("survival")
library("survminer")
library("finalfit")
library("glue")

# Data pre-processing ----

mydata <- rio::import(file = here::here(), format = "xlsx")
mydata$int <- lubridate::interval(
  lubridate::ymd(mydata$SurgeryDate),
  lubridate::ymd(mydata$LastFollowUpDate)
)
mydata$OverallTime <- lubridate::time_length(mydata$int, "month")
mydata$OverallTime <- round(mydata$OverallTime, digits = 1)

mydata$Outcome <- forcats::fct_recode(as.character(mydata$Death),
                                      "1" = "TRUE",
                                      "0" = "FALSE")

mydata$Outcome <- as.numeric(as.character(mydata$Outcome))

mydata %>% 
  select(-ID,
         -Name) %>% 
  inspectdf::inspect_types() %>% 
  dplyr::filter(type == "character") %>% 
  dplyr::select(col_name) %>% 
  pull() %>% 
  unlist() -> characterVariables



# Define UI for miles per gallon app ----
ui <- fluidPage(


  # App title ----
  titlePanel("Survival Analysis"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Selector for variable to plot against mpg ----
      selectInput(
        inputId = "Factor",
        label = "Choose a Factor Affecting Survival",
        choices = characterVariables,
        selected = "LVI"
      )
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      
      tags$br(),
      tags$h3("Kaplan-Meier Plot, Log-Rank Test"),
      tags$br(),
      tags$hr(),
      plotOutput("KMPlot"),
      tags$br(),
      tags$br(),
      tags$hr(),
      tags$br(),
      tags$h3("Univariate Cox-Regression"),
      tags$br(),
      tags$hr(),
      tableOutput("CoxTable"),
      tags$br(),
      tags$br(),
      tags$hr(),
      tags$br(),
      tags$h3("Median Survival"),
      tags$br(),
      tags$hr(),
      tableOutput("Median"),
      tags$br(),
      tags$br(),
      tags$hr(),
      tags$br(),
      tags$h3("1-3-5-yr Survival"),
      tags$br(),
      tags$hr(),
      tableOutput("YearSurv"),
      tags$br(),
      tags$br(),
      tags$hr(),
      tags$br(),
      tags$h3("Comment 1-3-5-yr Survival"),
      tags$br(),
      tags$hr(),
      textOutput("YearSurvComment"),
      tags$br(),
      tags$br(),
      tags$hr(),
      tags$br()
      
      

    )
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {

  
  output$KMPlot <- 
    
  renderPlot({
    
    mydata %>%
      finalfit::surv_plot(
        .data = .,
        dependent = "Surv(OverallTime, Outcome)",
        explanatory = input$Factor,
        xlab = 'Time (months)',
        pval = TRUE,
        legend = 'none',
        break.time.by = 12,
        xlim = c(0, 60)
      )
    
  })

  
  output$CoxTable <- 
  
  renderTable({
    
    mydata %>%
      finalfit::finalfit("Surv(OverallTime, Outcome)", input$Factor)
    
  })
  
  
  
  output$Median <- 
  
  
    renderTable({
    
    formula_text <- paste0("Surv(OverallTime, Outcome) ~ ",input$Factor)
    
    km_fit <- survfit(as.formula(formula_text),
                      data = mydata)
    
    km_fit_median_df <- summary(km_fit)
    
    km_fit_median_df <- as.data.frame(km_fit_median_df$table) %>% 
      janitor::clean_names() %>% 
      tibble::rownames_to_column()
    
  })
  
  
  
  
  output$YearSurv <- 
  
    renderTable({
    
    formula_text <- paste0("Surv(OverallTime, Outcome) ~ ",input$Factor)
    
    km_fit <- survfit(as.formula(formula_text),
                      data = mydata)
    
    km_fit_summary <- summary(km_fit, times = c(12,36,60))
    
    km_fit_df <- as.data.frame(km_fit_summary[c("strata", "time", "n.risk", "n.event", "surv", "std.err", "lower", "upper")])
    
    km_fit_df
    
  })
  
  
  output$YearSurvComment <- 
  
  renderPrint({
    
    
    formula_text <- paste0("Surv(OverallTime, Outcome) ~ ",input$Factor)
    
    km_fit <- survfit(as.formula(formula_text),
                      data = mydata)
    
    km_fit_summary <- summary(km_fit, times = c(12,36,60))
    
    km_fit_df <- as.data.frame(km_fit_summary[c("strata", "time", "n.risk", "n.event", "surv", "std.err", "lower", "upper")])
    
    km_fit_df %>% 
      dplyr::mutate(
        description = 
          glue::glue(
            "When {strata}, {time} month survival is {scales::percent(surv)} [{scales::percent(lower)}-{scales::percent(upper)}, 95% CI]."
          )
      ) %>% 
      dplyr::select(description) %>% 
      pull() -> comment
    
    print(comment)
    
  })

}

# Create Shiny app ----
shinyApp(ui, server)
