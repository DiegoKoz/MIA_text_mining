library(rvest)
library(tidyverse)
library(glue)
#### funciones #####

get_text <- function(link){
  tryCatch(html_text(read_html(link)),error = function(err) {glue::glue("error_in_link: {link}")})
}

get_new_links <- function(link){
  tryCatch({
  
  nodes <- read_html(link) %>% 
    rvest::html_nodes("a")  
  
  data.frame(link= html_attr(nodes, "href"), titulo = html_text(nodes)) %>% 
    filter(!str_detect(link,"(\\.\\.)|NA|.doc|.zip|.pdf|wikipedia|http|mailto"),
           link!="") %>% 
    na.omit(.) %>%
    mutate(link = gsub("#.*","",link)) %>% 
    distinct(link,.keep_all = TRUE )

  },error = function(err) {glue::glue("err in {link}")})
}
#### scrapping del MIA central #####

url <- "https://www.marxists.org/espanol/indice.htm"
webpage <- read_html(url)

refs <- webpage %>%
  html_nodes("a") 

df_links <- data.frame(autor = html_text(refs),
                       link = html_attr(refs, "href")) %>% 
  na.omit() %>% 
  filter(!str_detect(autor,"Sección"),
         !str_detect(autor,"Selección"),
         !str_detect(autor,"Archivo"),
         !autor %in% c("","Busqueda"," ","\r\n","Archivos Temáticos"),
         !str_detect(link,"#")) %>% 
  distinct(link,.keep_all = TRUE) %>% 
  mutate(link=paste0("https://www.marxists.org/espanol/",link))

df_links <- df_links %>% 
  mutate(nested_link = map(link, get_new_links))

df_links <- df_links %>%
  filter(!str_detect(nested_link,"err in ")) %>% 
  unnest()


## separo el texto directo, de los libros (que tienen indice y sub links)

texto_simple <- df_links %>%
  filter(!str_detect(link1,"index")) %>% 
  mutate(nested_link = paste0(gsub("/index.htm","",link),"/",link1),
         texto = map(nested_link,get_text)) %>%
  unnest() %>% 
  select(autor, titulo, texto)

saveRDS(texto_simple,"data/txt/main_text.RDS") #guardo parcial porque tarda


libros <- df_links %>% 
  filter(str_detect(link1,"index")) %>% 
  mutate(nested_link = paste0(gsub("index.htm","",link),link1),
         nested_nested_link = map(nested_link, get_new_links)) %>% #busco los nuevos links dentro de los libros
  filter(!str_detect(nested_nested_link,"err in ")) %>% 
  unnest(.)

#descargo el texto de cada capitulo
libros <- libros %>%
  mutate(nested_nested_link = paste0(gsub("index.htm","",nested_link),link2),
         texto = map(nested_nested_link,get_text)) %>%
  unnest(.) %>% 
  filter(!str_detect(texto, "error_in_link"))

# pego todos los capitulos de cada libro
libros <- libros %>%
  group_by(autor,titulo) %>%
  summarise(texto = paste(texto, collapse = "/n"))

saveRDS(libros,"data/txt/main_libros.RDS") #guardo parcial porque tarda


texto_simple <- texto_simple %>% 
  mutate(tipo="notas")
libros <- libros %>% 
  mutate(tipo="libros")

textos <- bind_rows(libros,texto_simple)

saveRDS(textos, "data/txt/textos.RDS")






#### Marx-Engels, Lenin, Trotsky y el Che vienen en otro formato ####

urls <- c("https://www.marxists.org/espanol/m-e/indice.htm",
         "https://www.marxists.org/espanol/lenin/obras/escritos.htm",
         "https://www.marxists.org/espanol/trotsky/indice.htm",
         "https://www.marxists.org/espanol/guevara/escritos/index.htm")


df_links <- data.frame(autor= c('marx_engels','lenin','totsky','guevara'),
                       link = urls) 


df_links <- df_links %>% 
  na.omit() %>% 
  filter(!str_detect(autor,"Sección"),
         !str_detect(autor,"Selección"),
         !str_detect(autor,"Archivo"),
         !autor %in% c("","Busqueda"," ","\r\n","Archivos Temáticos"),
         !str_detect(link,"#")) %>% 
  distinct(link,.keep_all = TRUE) %>% 
  mutate(link=as.character(link))


df_links <- df_links %>% 
  mutate(nested_link = map(link, get_new_links)) %>% 
  filter(!str_detect(nested_link,"err in ")) %>% 
  unnest()

## separo el texto directo, de los libros (que tienen indice y sub links)

texto_simple <- df_links %>%
  filter(!str_detect(link1,"index")) %>% 
  mutate(nested_link = paste0(gsub("/index.htm","",link),"/",link1),
         texto = map(nested_link,get_text)) %>%
  unnest() %>% 
  select(autor, titulo, texto)

saveRDS(texto_simple,"data/txt/meltg_text.RDS") #guardo parcial porque tarda


libros <- df_links %>% 
  filter(str_detect(link1,"index")) %>% 
  mutate(nested_link = paste0(gsub("index.htm|escritos.htm|indice.htm","",link),link1),
         nested_nested_link = map(nested_link, get_new_links)) %>% #busco los nuevos links dentro de los libros
  filter(!str_detect(nested_nested_link,"err in ")) %>% 
  unnest(.)

#descargo el texto de cada capitulo
libros <- libros %>%
  mutate(nested_nested_link = paste0(gsub("index.htm","",nested_link),link2),
         texto = map(nested_nested_link,get_text)) %>%
  unnest(.) %>% 
  filter(!str_detect(texto, "error_in_link"))

# pego todos los capitulos de cada libro
libros <- libros %>%
  group_by(autor,titulo) %>%
  summarise(texto = paste(texto, collapse = "/n"))

saveRDS(libros,"data/txt/meltg_libros.RDS") #guardo parcial porque tarda




## junto todo

texto_simple <- texto_simple %>% 
  mutate(tipo="notas")
libros <- libros %>% 
  mutate(tipo="libros")

textos <- bind_rows(libros,texto_simple)

saveRDS(textos, "data/txt/meltg_textos.RDS")


###### Junto MELTG y main MIA

meltg_textos <- read_rds("data/txt/meltg_textos.RDS")
main_textos <- read_rds("data/txt/textos.RDS")


textos <- bind_rows(meltg_textos,main_textos)

table(textos$tipo)

