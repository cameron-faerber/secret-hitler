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
        actionBttn(
            inputId = "hideCards",
            label = "Hide all",
            style = "material-flat",
            color = "royal"
        ),
        br(),
        br(),
        textOutput("count"),
        hr(),
        fluidRow(
            column(2, HTML("<div style='height: 140px;'>"), imageOutput("card1"), HTML("</div>")),
            column(2, HTML("<div style='height: 140px;'>"), imageOutput("card2"), HTML("</div>")),
            column(2, HTML("<div style='height: 140px;'>"), imageOutput("card3"), HTML("</div>")),
        ),
        uiOutput("cards"),
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
    
    #set reactive values (these can be changed )
    drawValues = reactiveValues()
    draw = eventReactive(input$drawCards, {
        random_draw = rbinom(n=3, size=1, prob=11/17)
        cards = ifelse(random_draw==1, "facist.png", "liberal.png")
        drawValues[['cardValues']] = cards
        drawValues[['hiddenState']] = c(0,0,0)
        
        startAnim(session = getDefaultReactiveDomain(), "card1","bounce")
        startAnim(session = getDefaultReactiveDomain(), "card2","bounce")
        startAnim(session = getDefaultReactiveDomain(), "card3","bounce")
        
        return(cards)
    })
    
    observeEvent(input$hideCards, {
        drawValues[['hiddenState']] = c(1,1,1)
    })
    
    onclick("card1", {
        drawValues[['cardValues']][1] = "hidden.png"
        drawValues[['hiddenState']][1] = 1 - drawValues[['hiddenState']][1] 
    })
    
    onclick("card2", {
        drawValues[['cardValues']][2] = "hidden.png"
        drawValues[['hiddenState']][2] = 1 - drawValues[['hiddenState']][2] 
    })
    
    onclick("card3", {
        drawValues[['cardValues']][3] = "hidden.png"
        drawValues[['hiddenState']][3] = 1 - drawValues[['hiddenState']][3] 
    })
    
    #count number of draws
    counter <- reactiveValues(countervalue = 0) 
    observeEvent(input$drawCards, {
        counter$countervalue <- counter$countervalue + 1
    })
    output$count <- renderText({
        paste("Number of draws: ", counter$countervalue)
    })
    
    output$card1 = renderImage({
        cards = draw()
        image = cards[1]
        if(drawValues[['hiddenState']][1]==1){
            image = 'hidden.png'
        }
        
        list(src = paste0("www/", image), 
             contentType = 'image/png',
             width = 100,
             height = 135)
    }, deleteFile = FALSE)
    
    output$card2 = renderImage({
        cards = draw()
        image = cards[2]
        if(drawValues[['hiddenState']][2]==1){
            image = 'hidden.png'
        }
        list(src = paste0("www/", image), 
             contentType = 'image/png',
             width = 100,
             height = 135)
    }, deleteFile = FALSE)
    
    output$card3 = renderImage({
        cards = draw()
        image = cards[3]
        if(drawValues[['hiddenState']][3]==1){
            image = 'hidden.png'
        }
        list(src = paste0("www/", image), 
             contentType = 'image/png',
             width = 100,
             height = 135)
    }, deleteFile = FALSE)

    ## keep alive ##
    output$keepAlive <- renderText({
        req(input$count)
        paste("keep alive ", input$count)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
