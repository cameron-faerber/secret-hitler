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
        fluidRow(column(2,imageOutput("card1")),
                 column(2,imageOutput("card2")),
                 column(2,imageOutput("card3"))
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
    drawValues = reactiveValues()
    draw = eventReactive(input$drawCards, {
        random_draw = rbinom(n=3, size=1, prob=11/17)
        cards = ifelse(random_draw==1, "facist.png", "liberal.png")
        drawValues$value = cards
        drawValues[['hiddenState']] = c(0,0,0)
        
        startAnim(session = getDefaultReactiveDomain(), "card1","bounce")
        startAnim(session = getDefaultReactiveDomain(), "card2","bounce")
        startAnim(session = getDefaultReactiveDomain(), "card3","bounce")
        
        
        return(cards)
    })
    
    onclick("card1", {
        drawValues$value[1] = "hidden.png"
        drawValues[['hiddenState']][1] = 1 - drawValues[['hiddenState']][1] 
    })
    
    onclick("card2", {
        drawValues$value[2] = "hidden.png"
        drawValues[['hiddenState']][2] = 1 - drawValues[['hiddenState']][2] 
    })
    
    onclick("card3", {
        drawValues$value[3] = "hidden.png"
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
        images = draw()
        image = images[1]
        if(drawValues[['hiddenState']][1]==1){
            image = 'hidden.png'
        }
        
        list(src = paste0("www/", image), 
             contentType = 'image/png',
             width = 100,
             height = 135)
    }, deleteFile = FALSE)
    
    output$card2 = renderImage({
        images = draw()
        image = images[2]
        if(drawValues[['hiddenState']][2]==1){
            image = 'hidden.png'
        }
        list(src = paste0("www/", image), 
             contentType = 'image/png',
             width = 100,
             height = 135)
    }, deleteFile = FALSE)
    
    output$card3 = renderImage({
        images = draw()
        image = images[3]
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
