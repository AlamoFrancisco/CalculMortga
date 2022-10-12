library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(reshape2)
library(scales)
options(scipen = 999)

# Define UI for slider demo app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Mortgage Calculator"),
  #h4(tags$a(href = "link", "Francisco Alamo")),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Simple integer interval ----
      selectInput("LocalA", "Local Authority", c("Basildon","Braintree","Brentwood",
                                                "Castle Point", "Chelmsford", "Colchester",
                                                "Epping Forest", "Harlow", "Maldon", 
                                                "Rochford","Tendring","Uttlesford"),selected = "Colchester"),
      hr(),
      numericInput("principal", "House Price ", 400000, min = 0),
      hr(),
      sliderInput("deposit","Percentage of the deposit", 
                  value = 5, min = 5, max = 75, step = 0.5),
      hr(),
      sliderInput("interest", "Annual interest rate (in %)", 
                  value = 2, min = 2, max = 6, step = 0.01),
      hr(),
      sliderInput("length", "Duration of the loan (in years)",
                  min = 0,
                  max = 30,
                  value = 25,
                  step = 1
      ),
      hr(),
      checkboxInput("plot", "Display plot?", TRUE),
      hr(),
      HTML('<p>Report a <a href="https://github.com/AntoineSoetewey/mortgage-calculator/issues">bug</a> or view the <a href="https://github.com/AntoineSoetewey/mortgage-calculator">code</a>. See more information about this app in this <a href="https://www.statsandr.com/blog/mortgage-calculator-r-shiny">article</a>. Back to <a href="https://www.antoinesoetewey.com/">www.antoinesoetewey.com</a> or <a href="https://www.statsandr.com/">www.statsandr.com</a>.</p>')
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table summarizing the values entered ----
      uiOutput("text"),
      br(),
      plotOutput("distPlot"),
      br(),
      DT::dataTableOutput("tbl"),
      br(),
      #p(em("Disclosure: Note that this application does not include investment advice or recommendations, nor a financial analysis. This application is intended for information only and you invest at your own risks. I cannot be held liable for any decision made based on the information contained in this application, nor for its use by third parties.")),
      #p(em("This R Shiny app is partially based on the R code of Prof. Thomas Girke.")),
      br(),
      br()
    )
  )
)

# Define server logic for slider examples ----
server <- function(input, output) {
  mortgage <- function(P = 500000, D = 5, I = 6, L = 30, LA = input$LocalA ,amort = TRUE, plotData = TRUE) {
    J <- I / (12 * 100) #Interest by month
    A <- (D * P) / 100 #Total deposit
    N <- 12 * L  #Total number of month
    M <- (P-A) * J / (1 - (1 + J)^(-N)) #Total you pay by month
    monthPay <<- M
    # Calculate Amortization for each Month
    if (amort == TRUE) {
      Pt <- P - A # current principal or amount of the loan
      currP <- NULL
      while (Pt >= 0) {
        H <- Pt * J # this is the current monthly interest
        C <- M - H # this is your monthly payment minus your monthly interest, so it is the amount of principal you pay for that month
        Q <- Pt - C # this is the new balance of your principal of your loan
        Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
        currP <- c(currP, Pt)
      }
      monthP <- c(P-A, currP[1:(length(currP) - 1)]) - currP
      aDFmonth <<- data.frame(
        Local_Authority = LA,
        Month = 1:length(currP),
        Year = sort(rep(1:ceiling(N / 12), 12))[1:length(monthP)],
        Balance = c(currP[1:(length(currP))]),
        Payment = monthP + c((monthPay - monthP)[1:(length(monthP))]),
        Principal = monthP,
        Interest = c((monthPay - monthP)[1:(length(monthP))]),
        stringsAsFactors = FALSE
      )
      aDFmonth <<- subset(aDFmonth, Year <= L * 12)
      aDFyear <- data.frame(
        Amortization = tapply(aDFmonth$Balance, aDFmonth$Year, max),
        Annual_Payment = tapply(aDFmonth$Payment, aDFmonth$Year, sum),
        Annual_Principal = tapply(aDFmonth$Principal, aDFmonth$Year, sum),
        Annual_Interest = tapply(aDFmonth$Interest, aDFmonth$Year, sum),
        Year = as.factor(na.omit(unique(aDFmonth$Year)))
      )
      aDFyear <<- aDFyear
    }
    if (plotData == TRUE) {
      aDFyear2 <- aDFyear %>%
        rename(
          Interest = Annual_Interest,
          Payment = Annual_Payment,
          Principal = Annual_Principal
        )
      aDFyear2$Year <- as.factor(aDFyear2$Year)
      aDFyear2 <- melt(aDFyear2[, c("Interest", "Principal", "Year")], id.vars = "Year")
      
      ggplot(aDFyear2, aes(x = Year, y = value, fill = variable)) +
        geom_bar(position = "fill", stat = "identity") +
        labs(title = LA, y = "Payment") +
        scale_y_continuous(labels = percent) +
        theme_minimal() +
        theme(legend.title = element_blank(), legend.position = "top")
    }
  }
  
  output$text <- renderUI({
    mortgage(P = input$principal, D = input$deposit, I = input$interest, L = input$length, LA = input$LocalA, plotData = FALSE)
    HTML(paste0(
      "<h3>", "Summary", "</h3>",
      "Local Authority: ", input$LocalA,
      "<br>",
      "Total Price of House: ", format(round(input$principal, 2), big.mark = ","),
      "<br>",
      "Initial deposit percetage: ", input$deposit, "%", " (", format(round((input$deposit*input$principal)/100, 2),big.mark = ",") ,")",
      "<br>",
      "Annual interest rate: ", input$interest, "%",
      "<br>",
      "Term: ", input$length, " years (", input$length * 12, " months)",
      "<br>",
      "<b>", "Monthly payment: ", format(round(monthPay, digits = 2), big.mark = ","), "</b>",
      "<br>",
      "<b>", "Total cost to be paid: ", "</b>", format(round(input$principal-((input$deposit*input$principal)/100), 2), big.mark = ","), " (Loan) + ", format(round(monthPay * 12 * input$length - (input$principal-((input$deposit*input$principal)/100)), 2), big.mark = ","), " (interest) = ", "<b>", format(round(monthPay * 12 * input$length, digits = 2), big.mark = ","), "</b>"
    ))
  })
  
  output$distPlot <- renderPlot({
    mortgage(P = input$principal, D = input$deposit, I = input$interest, L = input$length, LA = input$LocalA, plotData = input$plot)
  })
  
  # Data output
  output$tbl <- DT::renderDataTable({
    mortgage(P = input$principal, D = input$deposit, I = input$interest, L = input$length, LA = input$LocalA, plotData = FALSE)
    df_month <- DT::datatable(data.frame(aDFmonth),
                              caption = "Table 1: Insert information",
                              extensions = "Buttons",
                              options = list(
                                searching = TRUE,
                                lengthChange = TRUE,
                                dom = "Blrtip",
                                buttons = c("copy", "csv", "excel", "pdf", "print"),
                                
                                lengthMenu = list(c(-1, 10, 12, 15, 25, 50, 100), c("All", "10", "12", "15", "25", "50", "100"))
                              ),
                              rownames = FALSE,
    ) %>%
      formatCurrency(c("Balance", "Payment", "Principal", "Interest"), currency = "\u00A3", interval = 3, mark = ",")
  })
}

# Run the application
shinyApp(ui = ui, server = server)