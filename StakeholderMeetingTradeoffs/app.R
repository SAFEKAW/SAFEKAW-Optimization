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
            verbatimTextOutput("share_check"),
            
            sliderInput("cult_area_pct", "Cultivated area change (%)", min = -20, max = 20, value = 0, step = 1),
            
            sliderInput("irr_area_pct", "Irrigated area change (%)", min = -50, max = 50, value = 0, step = 1),
            sliderInput("irr_eff_pct", "Irrigation efficiency (%)", min = 0, max = 20, value = 0, step = 1),
            
            sliderInput("fert_pct", "Fertilizer efficiency (%)", min = -30, max = 20, value = 0, step = 1),
            
            
        ),
        mainPanel(
          h4("Model outputs"),
          verbatimTextOutput("eval_out"),
          hr(),
          h4("% improvement over baseline"),
          plotOutput("pct_bar", height = 380)
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
    
    policy <- reactive({
      list(
        cult_area_factor = 1 + input$cult_area_pct/100,

        fert_factor = 1 - input$fert_pct / 100,
        fert_factor = max(0, 1 - input$fert_pct / 100),
        fert_gamma  = 0.1,   # keep fixed for now (or make a slider later)
        
        irrig_frac_factor = 1 + input$irr_area_pct/100,
        irr_eff = 1 + input$irr_eff_pct/100
        
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
        wq_model_mlr = wq_model_mlr,              # or wq_model_mlr if you rename in global
        wq_base_by_year = wq_base_by_year,
        lu_baseline = lu_baseline,
        universal_costs = universal_costs,
        crop_params = crop_params,
        baseline_irrig_frac = baseline_irrig_frac,   
        hist_mix = hist_mix,
        fert_ref_by_year = fert_ref_by_year,
        policy = policy()
      )

    })
    
    # ---- BASELINE (fixed reference; no sliders) -----------------------
    baseline_x <- obs_x
    baseline_policy <- list(
      cult_area_factor = 1,
      fert_factor = 1,
      fert_gamma = 0.1,
      irrig_frac_factor = 1,
      irr_eff = 1
    )
    
    baseline_res <- reactiveVal(NULL)
    
    observeEvent(TRUE, {
      res <- eval_candidate_all_years(
        x = baseline_x,
        years_vec = years_vec,
        df_opt = df_opt,
        wq_model_mlr = wq_model_mlr,
        wq_base_by_year = wq_base_by_year,
        lu_baseline = lu_baseline,
        universal_costs = universal_costs,
        crop_params = crop_params,
        baseline_irrig_frac = baseline_irrig_frac,
        hist_mix = hist_mix,
        fert_ref_by_year = fert_ref_by_year,
        policy = baseline_policy
      )
      if (is.null(names(res))) names(res) <- c("mean_n_flux_kgyr", "mean_neg_profit_usdyr", "mean_irr_m3_yr")
      baseline_res(res)
    }, once = TRUE)
    
    pct_res <- reactive({
      base <- baseline_res()
      req(!is.null(base))
      scen <- eval_res()
      
      base_profit <- as.numeric(-base["mean_neg_profit_usdyr"])
      scen_profit <- as.numeric(-scen["mean_neg_profit_usdyr"])
      
      out <- c(
        n_flux_pct = 100 * (as.numeric(scen["mean_n_flux_kgyr"]) - as.numeric(base["mean_n_flux_kgyr"])) /
          as.numeric(base["mean_n_flux_kgyr"]),
        profit_pct = 100 * (scen_profit - base_profit) / base_profit,
        irr_pct    = 100 * (as.numeric(scen["mean_irr_m3_yr"]) - as.numeric(base["mean_irr_m3_yr"])) /
          as.numeric(base["mean_irr_m3_yr"])
      )
      
      out
    })   
    
    output$pct_bar <- renderPlot({
      pct <- pct_res()
      validate(need(all(is.finite(pct)), "Percent change not available (check baseline/scenario values)."))
      
      df <- data.frame(
        metric = c("Nitrate flux", "Net returns", "Irrigation volume"),
        pct = c(pct["n_flux_pct"], pct["profit_pct"], pct["irr_pct"])
      )
      
      op <- par(mar = c(10, 4, 2, 1),   xpd = NA)  # room for x labels
      on.exit(par(op), add = TRUE)
      
      bp <- barplot(
        height = df$pct,
        names.arg = df$metric,
        ylab = "% improvement over baseline",
        las = 2,
        cex.names = 1.25,
        cex.axis  = 1.15,
        cex.lab   = 1.3
      )
      
      abline(h = 0)
     # text(bp, df$pct, labels = sprintf("%+.1f%%", df$pct), pos = ifelse(df$pct >= 0, 3, 1), cex = 1.1)
    })
    
    
    
    output$eval_out <- renderPrint({
      base <- baseline_res()
      req(!is.null(base))
      
      scen <- eval_res()
      pct  <- pct_res()
      
      scen_p <- -scen["mean_neg_profit_usdyr"]
      
      
      cat(
       
        "Change vs baseline (%):\n",
        "  Nitrate flux: ", sprintf("%+.1f%%", pct["n_flux_pct"]), "\n",
        "  Profit:       ", sprintf("%+.1f%%", pct["profit_pct"]), "\n",
        "  Irrigation:   ", sprintf("%+.1f%%", pct["irr_pct"]), "\n",

        "Outputs (absolute):\n",
        "  Nitrate flux (kg/yr): ", format(scen["mean_n_flux_kgyr"], big.mark=","), "\n",
        "  Profit ($/yr):        ", format(scen_p, big.mark=","), "\n",
        "  Irrigation (m3/yr):   ", format(scen["mean_irr_m3_yr"], big.mark=","), "\n\n",
        sep = ""
      )
    })
    
}
    
#      res <- eval_res()   
      
      #print(eval_res())  # should print numbers
      
      
      # if res is unnamed, name it here
#      if (is.null(names(res))) names(res) <- c("mean_n_flux_kgyr", "mean_neg_profit_usdyr", "mean_irr_m3_yr")
      
#      cat(
#        "Nitrate flux (kg/yr): ", format(res["mean_n_flux_kgyr"], big.mark=","), "\n",
#        "Profit ($/yr):        ", format(-res["mean_neg_profit_usdyr"], big.mark=","), "\n",
#        "Irrigation (m3/yr):   ", format(res["mean_irr_m3_yr"], big.mark=","), "\n",
#        sep = ""
#      )
#    })
  

# Run the application 
shinyApp(ui = ui, server = server)
