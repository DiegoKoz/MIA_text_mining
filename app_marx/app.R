#https://diegokoz.shinyapps.io/wordcloud_marx/
library(tm)
library(wordcloud2)
# library(wordcloud)
library(memoise)
library(tidyverse)
library(magrittr)

textos <- readRDS("textos_marx.RDS")

textos <- textos %>% 
  filter(!titulo %in% c("aquí","Vers. alterna"))

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
                    c(stopwords(kind = "es"), "internet","regresar","actualizar", 
                      "así", "tan","uno", "dos", "tres", "cuatro", "pues", "hacia", "incluso", 
                      "cosas", "dado", "mayor","hace", "ayuda", "vol","pág", "tal", 
                      "ello", "través","libro", "sólo", "sino"))
  
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
      wordcloud2Output("plot",height ="600px")
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
  wordcloud_rep <- repeatable(wordcloud2)
  
  output$plot <- renderWordcloud2({
    v <- terms()
    vv <- data_frame(word = names(v), freq = v)
    
    vv <- vv %>% 
      filter(freq>=input$freq) %>% 
      top_n(.,n=input$max, wt = freq)
    
    wordcloud2(vv, shuffle = FALSE)
    
  })
}

##### RUN ##### 

shinyApp(ui, server)

