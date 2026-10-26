library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

ui <- fluidPage(
  titlePanel("Exploring Coin Flip Proportions"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n_flips", "Number of flips per set:", value = 16, min = 1, step = 1),
      radioButtons("mode", "Choose input type:",
                   choices = c("Enter your own results" = "user",
                               "Simulate coin flips" = "sim")),
      conditionalPanel(
        condition = "input.mode == 'user'",
        textAreaInput("user_data",
                      "Enter counts of heads (comma-separated):",
                      placeholder = "Example: 7, 8, 11, 14, 9"),
        helpText("Each number must be an integer between 0 and the chosen number of flips.")
      ),
      conditionalPanel(
        condition = "input.mode == 'sim'",
        numericInput("n_sim", "How many sets?", value = 1000, min = 1, step = 1),
        actionButton("simulate", "Generate")
      ),
      hr(),
      sliderInput("p0", "Assumed chance of heads:", min = 0, max = 1, value = 0.5, step = 0.01),
      radioButtons("side", "Type of check:",
                   choices = c("Different from this value" = "two",
                               "Higher than this value" = "greater",
                               "Lower than this value" = "less")),
      checkboxInput("shade", "Highlight most surprising results", value = FALSE),
      
      conditionalPanel(
        condition = "input.shade == true",
        numericInput("surprise_count", "Outcome count that defines 'surprising':",
                     value = 10, min = 0, step = 1), 
        helpText("All outcomes equally or more extreme (by distance from the mean) than this count will be highlighted (guarantees symmetry for the 'two' side check).")
      ),
      
      hr(),
      helpText("Highlighting shows which outcomes are considered surprising based on their distance from the expected average.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Histogram of Counts",
                 br(),
                 textOutput("highlight_list"),
                 plotOutput("hist_counts", height = "420px")),
        tabPanel("Histogram of Proportions",
                 br(),
                 textOutput("highlight_list_prop"), # Added text output for proportions
                 plotOutput("hist_props", height = "420px"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # parse user input into integer vector (0..n)
  parse_user_input <- function(txt, n_flips) {
    if (is.null(txt) || txt == "") return(numeric(0))
    nums <- suppressWarnings(as.numeric(unlist(strsplit(txt, "[,\\s]+"))))
    nums <- nums[!is.na(nums)]
    # keep only integers in [0, n_flips]
    nums <- nums[floor(nums) == nums & nums >= 0 & nums <= n_flips]
    nums
  }
  
  # reactive: the raw vector of observed counts (0..n)
  observed_vals <- reactive({
    if (input$mode == "user") {
      req(input$user_data)
      vals <- parse_user_input(input$user_data, input$n_flips)
      validate(need(length(vals) > 0, "No valid user values found. Enter integers between 0 and the chosen number of flips."))
      vals
    } else {
      # simulate when button pressed
      req(input$simulate)
      isolate(rbinom(input$n_sim, size = input$n_flips, prob = input$p0))
    }
  })
  
  # vector of possible outcomes 0..n
  possible_vals <- reactive(0:input$n_flips)
  
  # Function to define critical region based on distance (now the primary/only method)
  get_distance_based_vals <- function(n, p0, surprise_k) {
    # Check for valid surprise_k based on current n_flips
    if (is.null(surprise_k) || surprise_k < 0 || surprise_k > n || floor(surprise_k) != surprise_k) {
      surprise_k <- n 
    }
    
    # Expected number of heads under H0
    expected_heads <- n * p0
    
    # Distance of the surprising outcome from the expected value
    D <- abs(surprise_k - expected_heads)
    
    # Identify all possible outcomes (x) that are at least this distance D away.
    x <- 0:n
    highlight_vals <- x[abs(x - expected_heads) >= D]
    
    return(sort(highlight_vals))
  }
  
  # Reactive that calculates the set of highlighted outcomes
  most_surprising_vals <- reactive({
    if (!isTruthy(input$shade)) return(integer(0))
    
    n <- input$n_flips
    p0 <- input$p0
    
    return(get_distance_based_vals(n, p0, input$surprise_count))
  })
  
  # Reactive to calculate the observed stats for BOTH User and Sim modes
  highlight_stats <- reactive({
    hs <- most_surprising_vals()
    if (length(hs) == 0) return(list(count = 0, total = 0, prob = 0))
    
    vals <- observed_vals()
    total_obs <- length(vals)
    
    # Count how many of the observed flips (user or sim) fall into the surprising region
    surprising_count <- sum(vals %in% hs)
    observed_prob <- surprising_count / total_obs
    
    list(
      count = surprising_count,
      total = total_obs,
      prob = observed_prob
    )
  })
  
  # precompute frequency table for counts (0..n) and proportions
  freq_table <- reactive({
    vals <- observed_vals()
    possible <- possible_vals()
    freq <- as.integer(table(factor(vals, levels = possible)))
    tibble(
      heads = possible,
      count = freq,
      prop = freq / sum(freq)
    )
  })
  
  # Helper function to generate the common status text
  get_status_text <- function() {
    stats <- highlight_stats()
    paste0(
      "Observed frequency of highlighted outcomes: ",
      stats$count, " out of ", stats$total, " (",
      format(stats$prob, digits = 3), ")"
    )
  }
  
  # 1. Output for COUNTS tab
  output$highlight_list <- renderText({
    if (!input$shade) return("Highlighting is off.")
    hs <- most_surprising_vals()
    if (length(hs) == 0) return("No outcomes selected for highlighting (adjust the 'Outcome count' input).")
    
    paste0(
      "Highlighted outcomes (by # heads): ", paste(hs, collapse = ", "),
      ". | ", get_status_text()
    )
  })
  
  # 2. Output for PROPORTIONS tab
  output$highlight_list_prop <- renderText({
    if (!input$shade) return("Highlighting is off.")
    hs <- most_surprising_vals()
    if (length(hs) == 0) return("No outcomes selected for highlighting (adjust the 'Outcome count' input).")
    
    # Convert integer counts to proportions
    hs_props <- hs / input$n_flips
    # Format them nicely
    hs_props_str <- paste(format(hs_props, digits = 2), collapse = ", ")
    
    paste0(
      "Highlighted outcomes (by # proportions): ", hs_props_str,
      ". | ", get_status_text()
    )
  })
  
  # counts plot
  output$hist_counts <- renderPlot({
    tbl <- freq_table()
    highlight_vals <- if (input$shade) most_surprising_vals() else integer(0)
    tbl <- tbl %>% mutate(highlight = heads %in% highlight_vals)
    
    ggplot(tbl, aes(x = heads, y = count, fill = highlight)) +
      geom_col(color = "black", width = 0.9) +
      scale_fill_manual(values = c("TRUE" = "tomato", "FALSE" = "grey80"), guide = FALSE) +
      scale_x_continuous(breaks = tbl$heads) +
      theme_bw(base_size = 15) +
      labs(
        title = "Histogram of Counts",
        x = paste0("# of heads (out of ", input$n_flips, ")"),
        y = "Number of repetitions"
      )
  })
  
  # proportions plot (Updated x-axis format)
  output$hist_props <- renderPlot({
    tbl <- freq_table()
    highlight_vals <- if (input$shade) most_surprising_vals() else integer(0)
    tbl <- tbl %>% mutate(highlight = heads %in% highlight_vals,
                          prop_value = heads / input$n_flips)
    
    ggplot(tbl, aes(x = prop_value, y = count, fill = highlight)) +
      geom_col(color = "black", width = 1 / (input$n_flips + 1)) +
      scale_fill_manual(values = c("TRUE" = "tomato", "FALSE" = "grey80"), guide = FALSE) +
      # Changed labels from percent_format to number_format (decimal proportions)
      scale_x_continuous(breaks = tbl$prop_value, labels = number_format(accuracy = 0.01)) +
      theme_bw(base_size = 15) +
      labs(
        title = "Histogram of Proportions",
        x = "Proportion of heads",
        y = "Number of repetitions"
      )
  })
  
}

shinyApp(ui, server)