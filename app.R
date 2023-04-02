library(shiny)
library(tidyverse)

## globals ----

equilibrium <- function(b, s) {
  if (min(b) >= max(s) || max(b) < min(s)) return(c(NA, NA))
  m <- min(length(b), length(s))
  a <- which(b[1:m] < s[1:m])[1]
  z <- s[a] - s[a - 1] - b[a] + b[a - 1]
  x <- (a * z + b[a] - s[a]) / z
  y <- (b[a] - b[a - 1]) * (x - a) + b[a]
  c(x, y)
}

bsrect <- function(x, e, y, type) {
  if (type == "buyer") {
    fill <- "red"
  } else {
    fill <- "blue"
  }
  alpha <- ifelse(x <= e[1], 1, 0.3)
  geom_rect(
    data = data.frame(
      x = x, e = e[2], y = y
    ),
    aes(
      xmin = x - 1,
      xmax = x,
      ymin = min(e, y), ymax = max(e, y)
    ),
    fill = fill, alpha = alpha
  )
}

all_rects <- function(df, e, t) {
  df <- df %>% 
    filter(x > 0) %>% 
    filter(floor(x) <= e[1] & type == t) %>% 
    mutate(
      ymin = pmin(e[2], y),
      ymax = pmax(e[2], y)
    )
  if (t == "buyer") {
    fill <- "red"
  } else {
    fill <- "blue"
  }
  alpha <- 0.1
  geom_rect(
    data = df,
    aes(
      xmin = x - 1,
      xmax = x,
      ymin = ymin, ymax = ymax
    ),
    fill = fill, alpha = alpha
  )
}

surplus <- function(df, e) {
  df <- df %>% 
    filter(x > 0 & floor(x) <= e[1])
  cs <- sum(df[df$type == "buyer", ]$y - e[2])
  ps <- sum(e[2] - df[df$type == "seller", ]$y)
  tg <- cs + ps
  c(cs = cs, ps = ps, tg = tg)
}

## ui -----

ui <- fluidPage(
  titlePanel("Gain from Trade"),
  sidebarLayout(
    sidebarPanel(
      numericInput("b", "Number of possible buyer/seller pairs", value = 20, min = 10, max = 30),
      numericInput("bmean", "Average buyer maximum price:", 50, min = 1, max = 100),
      numericInput("smean", "Average seller minimum price:", 50, min = 1, max = 100),
      radioButtons(
        "show",
        "Show: ",
        choices = list(
          "consumer surplus", "producer surplus"
        ),
        selected = "consumer surplus",
        inline = TRUE
      ),
      actionButton("go", "Create buyers and sellers!"),
      uiOutput("bnumber"),
      uiOutput("snumber")
    ),
    mainPanel(
      plotOutput("sd"),
      br(),
      verbatimTextOutput("narrative"),
      br(),
      DT::dataTableOutput("data")
    )
  )
)

## server logic ----

server <- function(input, output, session) {
  buyers <- eventReactive(input$go, {
    rnorm(input$b, mean = input$bmean, sd = 10) %>% 
      sort(decreasing = TRUE)
  }) 
  
  sellers <- eventReactive(input$go, {
    rnorm(input$b, mean = input$smean, sd = 10) %>% sort()
  })
  
  df <- reactive({
    df_b <- data.frame(
      x = 0:isolate(input$b),
      y = c(max(buyers()), buyers()),
      type = "buyer"
    )
    df_s <- data.frame(
      x = 0:isolate(input$b),
      y = c(min(sellers()), sellers()),
      type = "seller"
    )
    df_b %>% bind_rows(df_s) %>% 
      mutate(
        text = ifelse(
          type == "buyer",
          glue::glue("Buyer {x} is willing to pay at most {round(y, 2)}."),
          glue::glue("Seller {x} will sell for at least {round(y, 2)}.")
        )
      )
  })
  
  e <- reactive({
    equilibrium(b = buyers(), s = sellers())
  })
  
  output$sd <- renderPlot({
    req(!is.na(e()[1]))
    input$go
    input$show
    bn <- input$bn
    sn <- input$sn
    isolate({
      e <- e()
      plot <- df() %>% 
        ggplot(aes(x = x, y = y)) +
        geom_line(aes(color = type)) +
        geom_point(size = 0.5, aes(color = type)) +
        geom_segment(aes(
          x = 0,
          xend = max(x),
          y = e[2],
          yend = e[2]
        )) +
        geom_point(aes(x = e[1], y = e[2]), size = 2) +
        labs(
          x = "number supplied (q)",
          y = "price (p)"
        ) +
        scale_x_continuous(breaks = 0:input$b, labels = 0:input$b)
      if (input$show == "consumer surplus" & !is.null(input$bn)) {
        plot <- plot + 
          bsrect(bn, e, buyers()[bn], type = "buyer") +
          all_rects(df(), e(), "buyer")
      } else if (input$show == "producer surplus" & !(is.null(input$sn))) {
        plot <- plot + 
          bsrect(sn, e, sellers()[sn], type = "seller") +
          all_rects(df(), e(), "seller")
      }
      plot
    })
  }, res = 96)
  
  output$bnumber <- renderUI({
    req(!is.na(e()[1]) & input$show == "consumer surplus")
    input$go
    sliderInput("bn", "Experience of Buyer #:", 1, input$b, 1, step = 1)
  })
  
  output$snumber <- renderUI({
    req(!is.na(e()[1]) & input$show == "producer surplus")
    input$go
    sliderInput("sn", "Experience of Seller #:", 1, input$b, 1, step = 1)
  })
  
  output$narrative <- renderText({
    if (!is.na(e()[2])) {
      gains <- round(surplus(df(), e()), 2)
      bn <- input$bn
      sn <- input$sn
      bgain <- round(buyers()[bn] - e()[2], 2)
      sgain <- round(e()[2] - sellers()[sn], 2)
      bres <- ifelse(bgain > 0, "gains", "would lose")
      sres <- ifelse(sgain > 0, "gains", "would lose")
      msg <- glue::glue("
               {floor(e()[1])} trades occur. Equilibirum price is {round(e()[2], 2)}. 
               Total gain from trade is {gains['tg']}.")
      if (input$show == "consumer surplus") {
        msg <- paste(
          msg,
          glue::glue('
                     
                     Consumer surplus is {gains["cs"]}.'),
          glue::glue('Buyer {bn} {bres} {abs(bgain)} from trading.'),
          sep = " "
        )
      }
      if (input$show == "producer surplus") {
        msg <- paste(
          msg,
          glue::glue('
                     
                     Producer surplus is {gains["ps"]}.'),
          glue::glue('Seller {sn} {sres} {abs(sgain)} from trading.'),
          sep = " "
        )
      }
    } else {
      msg <- "Sorry, there is no equilibrium."
    }
    msg
  })
  
  output$data <- DT::renderDataTable({
    df() %>%
      filter(x != 0) %>% 
      rename(price = y) %>% 
      select(type, price, text)
  }, rownames = FALSE)
  
}

shinyApp(ui, server)