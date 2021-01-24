library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyanimate)

# Define UI for application
ui <- fluidPage(
    useShinyjs(),
    withAnim(),

    # Application title
    titlePanel("Secret Hitler Card Draw"),

    mainPanel(
        textOutput("count"),
        br(),
        setSliderColor(c("dodgerblue","firebrick"), c(1,2)),
        sliderTextInput(
            inputId = "nLiberal",
            label = "Number Liberals played", 
            choices = seq(from = 0, to = 4, by = 1),
            grid = FALSE
        ),
        sliderTextInput(
            inputId = "nFacist",
            label = "Number Facists played", 
            choices = seq(from = 0, to = 6, by = 1),
            grid = FALSE
        ),
        actionBttn(
            inputId = "drawCards",
            label = "Draw cards", 
            style = "material-flat",
            color = "success"
        ),
        hr(),
        uiOutput("cards"),
        br(),
        br(),
        br()
        ),
    
    ## keep alive here ##
    tags$head(
        HTML(
            "
          <script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
          "
        )
    ),
    span(textOutput("keepAlive"), style="color:white"),

    )

# Define server logic
server <- function(input, output){
    i <<- 1
    
    draw = eventReactive(input$drawCards, {
        cards = rbinom(n=3, size=1, prob=11/17)
        ifelse(cards==1, "facist.png", "liberal.png")
    })
    
    #count number of draws
    counter <- reactiveValues(countervalue = 0) 
    observeEvent(input$drawCards, {
        counter$countervalue <- counter$countervalue + 1
    })
    output$count <- renderText({
        paste("Number of draws: ", counter$countervalue)
    })
    
    output$cards <- renderUI({
        images = draw()
        
        startAnim(
            session = getDefaultReactiveDomain(), 
            "cards",
            "bounce"
        )
        fluidRow(id="los-cards",
                 column(2,img(id="card1", src=images[1], width=100, height=135)),
                 column(2,img(id="card2", src=images[2], width=100, height=135)),
                 column(2,img(id="card3", src=images[3], width=100, height=135))
        )
    })
    
    ## keep alive ##
    output$keepAlive <- renderText({
        req(input$count)
        paste("keep alive ", input$count)
    })

    observeEvent(input$drawCards, {
        startAnim(session = getDefaultReactiveDomain(), "los-cards", "bounceInLeft")
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
