#Entrenamiento de word embeddings con FastText --------------
library(fastrtext)
library(tidyverse)
source('WordVectors/limpiar_texto.R')
#cargo corpus

corpus <- readRDS('data/txt/textos_limpio.RDS')

#preprocesamiento de los textos

corpus <-
  limpiar_textos_para_tokenizar(corpus$texto,
                                reemplazar_numeros = T,
                                normalizar_acentos = T)

#Entreno modelo --------

entrenar_modelos_fasttext <-
  function(x, dim, mincount, nombre_embedding){
    
    tmp_file_txt <- tempfile()
    
    writeLines(text = x, 
               con = tmp_file_txt)
    
    model <- paste0("WordVectors/fasttext_word_vectors",
                    '_',
                    nombre_embedding)
    
    
    #Entreno el modelo skipgram con parametros estándar, vectores de 200 dimensiones y negative sampling -----
    execute(commands = c("skipgram", 
                         "-input", tmp_file_txt, 
                         "-output", model,
                         "-dim", dim,
                         "-minCount", mincount,
                         "-verbose", 1))
  }

entrenar_modelos_fasttext(corpus,
                          200,
                          5,
                          'embeddings')
