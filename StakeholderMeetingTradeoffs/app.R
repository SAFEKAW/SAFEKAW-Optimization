#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Land use and trade off explorer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("corn","Corn (%)", 0, 100, 40, step = 1),
            sliderInput("soy","Soybeans (%)", 0, 100, 30, step = 1),
            sliderInput("sor","Sorghum (%)", 0, 100, 10, step = 1),
            hr(),
            verbatimTextOutput("share_check")
        ),
        mainPanel(
          h4("Model outputs"),
          verbatimTextOutput("eval_out")
        )
    ) 
)

server <- function(input, output, session) {

    shares <- reactive({
      corn <- input$corn
      soy <- input$soy
      sor <- input$sor
      wheat <- 100 - (corn + soy + sor)
      
      
      list(corn = corn, soy = soy, sor = sor, wheat = wheat)
    })
  
    
    output$share_check <- renderText({
      s <- shares()
      paste0(
        "Wheat (remainder): ", s$wheat, "%\n",
        if (s$wheat < 0) " Total exceeds 100% - reduce slider." else "OK"
      )
    })
    
    eval_res <- reactive({
      s <- shares()
      req(s$wheat >= 0)
      
      #convert % to fractions for decision vector
      x <- c(s$corn, s$soy, s$sor) / 100
      
      #call fx here
      eval_candidate_all_years(
        x = x,
        years_vec = years_vec,
        df_opt = df_opt,
        wq_model_log1p = wq_model_log1p,
        wq_context = wq_base_by_year,
        lu_baseline = lu_baseline,
        universal_costs = universal_costs,
        crop_params = crop_params,
        baseline_irrig_frac = baseline_irrig_frac,
        hist_mix = hist_mix
      )

    })
    
    output$eval_out <- renderPrint({
      res <- eval_res()   
      
      # if res is unnamed, name it here
      if (is.null(names(res))) names(res) <- c("nitrate_kgyr", "neg_profit_usdyr", "irr_m3_yr")
      
      cat(
        "Nitrate flux (kg/yr): ", format(res["nitrate_kgyr"], big.mark=","), "\n",
        "Profit ($/yr):        ", format(-res["neg_profit_usdyr"], big.mark=","), "\n",
        "Irrigation (m3/yr):   ", format(res["irr_m3_yr"], big.mark=","), "\n",
        sep = ""
      )
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
