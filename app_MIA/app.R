#https://diegokoz.shinyapps.io/wordcloud_marx/
library(tm)
library(wordcloud2)
# library(wordcloud)
library(memoise)
library(tidyverse)
library(magrittr)

textos <- readRDS("textos.RDS")

textos <- textos %>% 
  filter(!titulo %in% c("aquí","Vers. alterna"))


autores <-names(sort(table(textos$autor),decreasing = T))

# cachea resultados y no recalcula todo
getTermMatrix <- memoise(function(aut, titulo) {
  
  if (!(aut %in% autores)) {
    stop("Autor desconocido")
  } 
  titulos <- textos %>% 
    filter(autor==aut) %$% 
    titulo
  if (!(titulo %in% titulos)) {
    stop("Titulo desconocido")
  }
  
  text <- textos$texto[textos$autor==aut & textos$titulo==titulo]
  
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
      selectInput("aut", "Elija un autor",
                  choices = autores, 
                  selected="marx engels"),
      # uiOutput("seleccion_titulo"),
      selectInput("selection", "Elija un texto",
                  choices = textos$titulo,
                  selected="Tesis sobre Feuerbach"),
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

  
  observe(
    {
      input$aut
      # Update based on the month change event
      updateSelectInput(session, "selection", "Elija un texto",
                        choices = as.character(textos$titulo[textos$autor==input$aut]),
                        selected="Tesis sobre Feuerbach")
})

  terms <- reactive({
    # actualiza
    input$update
    isolate({
      withProgress({
        setProgress(message = "procesando el texto...")
        getTermMatrix(input$aut, input$selection)
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

