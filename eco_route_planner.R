library(shiny)
library(httr2)
library(jsonlite)

# Define `%||%` operator (returns left if not NULL, else right)
`%||%` <- function(a, b) if (!is.null(a)) a else b

# --- Gemini API Config ---
api_key <- "AIzaSyDx0rSIpRv9_qqWcFmAsOg2c1xIG6dANzg"
model_name <- "gemini-1.5-flash"
api_url <- paste0(
  "https://generativelanguage.googleapis.com/v1beta/models/",
  model_name,
  ":generateContent?key=",
  api_key
)

# --- Function to call Gemini API ---
call_gemini <- function(prompt_text, max_output_tokens = 1024, temperature = 0.7) {
  tryCatch({
    request_body <- list(
      contents = list(
        list(parts = list(list(text = prompt_text)))
      ),
      generationConfig = list(
        maxOutputTokens = max_output_tokens,
        temperature = temperature
      )
    )
    
    response <- request(api_url) %>%
      req_method("POST") %>%
      req_body_json(request_body) %>%
      req_headers("Content-Type" = "application/json") %>%
      req_perform()
    
    parsed <- resp_body_json(response)
    
    text <- parsed$candidates[[1]]$content$parts[[1]]$text
    return(text)
  }, error = function(e) {
    paste("Error in API call:", e$message)
  })
}

# --- UI ---
ui <- fluidPage(
  titlePanel("Eco-Friendly Route Optimization with Gemini"),
  
  sidebarLayout(
    sidebarPanel(
      textAreaInput("routeA", "Route A Description:", 
                    value = "Route A: 15km, mostly flat, city driving with moderate traffic, estimated fuel use: 1.2L.", 
                    rows = 4),
      textAreaInput("routeB", "Route B Description:", 
                    value = "Route B: 18km, includes a 2km steep hill, 10km highway at 80km/h, then 6km city driving with 2 traffic lights.", 
                    rows = 4),
      actionButton("analyzeBtn", "Analyze Routes"),
      hr(),
      actionButton("tipsBtn", "Get Fuel Efficiency Tips"),
      hr(),
      downloadButton("downloadData", "Download Results CSV")
    ),
    
    mainPanel(
      h3("Route Comparison Analysis"),
      verbatimTextOutput("routeAnalysis"),
      hr(),
      h3("Fuel Efficiency Tips"),
      verbatimTextOutput("fuelTips"),
      hr(),
      h3("Summary of Chosen Route"),
      verbatimTextOutput("routeSummary")
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  # Reactive values to store results
  rv <- reactiveValues(
    analysis = NULL,
    tips = NULL,
    summary = NULL
  )
  
  observeEvent(input$analyzeBtn, {
    prompt <- paste(
      "I am planning an eco-friendly route. Here are two options:\n",
      "Option 1:", input$routeA, "\n",
      "Option 2:", input$routeB, "\n",
      "Which route is more fuel-efficient and environmentally friendly? Justify your answer."
    )
    
    rv$analysis <- call_gemini(prompt, temperature = 0.5)
    
    summary_prompt <- paste(
      "Summarize the following eco-route in 2-3 sentences:\n",
      input$routeA
    )
    rv$summary <- call_gemini(summary_prompt)
  })
  
  observeEvent(input$tipsBtn, {
    tips_prompt <- "Give 5 practical tips for maximizing fuel efficiency during car trips."
    rv$tips <- call_gemini(tips_prompt)
  })
  
  output$routeAnalysis <- renderText({
    req(rv$analysis)
    rv$analysis
  })
  
  output$fuelTips <- renderText({
    req(rv$tips)
    rv$tips
  })
  
  output$routeSummary <- renderText({
    req(rv$summary)
    rv$summary
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("eco_route_analysis-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- data.frame(
        Prompt_Type = c("Route Comparison", "Fuel Tips", "Summary"),
        Output_Text = c(rv$analysis %||% "", rv$tips %||% "", rv$summary %||% "")
      )
      write.csv(data, file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui, server)

