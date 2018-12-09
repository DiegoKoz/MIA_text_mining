library(tm)
library(wordcloud)
library(memoise)
library(tidyverse)
library(magrittr)

textos <- readRDS("textos.RDS")

textos <- textos %>% 
  filter(titulo !="aquí")

titulos <- textos$titulo


# cachea resultados y no recalcula todo
getTermMatrix <- memoise(function(titulo) {

  if (!(titulo %in% titulos))
    stop("Unknown book")
  
  text <-  textos$texto[textos$titulo==titulo]
  
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords(kind = "es"), "internet","regresar","actualizar"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})



### UI

ui <- fluidPage(
  # titulo
  titlePanel("Nube de palabras"),
  
  sidebarLayout(
    # inputs
    sidebarPanel(
      selectInput("selection", "Eliga un texto",
                  choices = titulos),
      actionButton("update", "Actualizar"),
      hr(),
      sliderInput("freq",
                  "Frecuencia mínima:",
                  min = 1,  max = 50, value = 5),
      sliderInput("max",
                  "Máxima cantidad de palabras:",
                  min = 1,  max = 300,  value = 25)
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("plot",height ="600px")
    )
  )
)


###

server <- function(input, output, session) {
  # Define una reactive expression para el dtm
  terms <- reactive({
    # actualiza
    input$update
    isolate({
      withProgress({
        setProgress(message = "procesando el texto...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  # para que se repita el mismo wordcloud en la sesion
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
}

##### RUN ##### 

shinyApp(ui, server)

