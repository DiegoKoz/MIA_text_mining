library(rvest)
library(xml2)
library(tidyverse)
library(glue)
#### funciones #####

get_text <- function(link){
  tryCatch(html_text(read_html(link)),error = function(err) {glue::glue("error_in_link: {link}")})
}

get_new_links <- function(link){
  tryCatch({
    
    base <- link
    nodes <- read_html(link) %>% 
      rvest::html_nodes("a")  
    
    data.frame(link= html_attr(nodes, "href"), titulo = html_text(nodes)) %>% 
      filter(!str_detect(link,"(\\.\\.)|NA|.doc|.zip|.pdf|wikipedia|http|mailto"),
             link!="") %>% 
      na.omit(.) %>%
      mutate(link = gsub("#.*","",link),
             link= xml2::url_absolute(link, base= base)) %>% 
      distinct(link,.keep_all = TRUE )
    
  },error = function(err) {glue::glue("error_in_link: {link}")})
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
  filter(!str_detect(nested_link,"error_in_link")) %>% 
  unnest()

#### fechas MIA central

fechas_central <- df_links %>% 
  mutate(fecha= str_extract(link1, '\\d\\d\\d\\d'))


range(fechas_central$fecha,na.rm = T)
plot(fechas_central$fecha)

fechas_central %>% 
  filter(fecha <1800)


fechas_central$fecha[which(fechas_central$fecha==1105)] <- NA #elimino error de parseo

#### Marx-Engels, Lenin, Trotsky y el Che vienen en otro formato ####

urls <- c("https://www.marxists.org/espanol/m-e/indice.htm",
          "https://www.marxists.org/espanol/lenin/obras/escritos.htm",
          "https://www.marxists.org/espanol/trotsky/indice.htm",
          "https://www.marxists.org/espanol/guevara/escritos/index.htm")


df_links <- data.frame(autor= c('marx_engels','lenin','trotsky','guevara'),
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
  filter(!str_detect(nested_link,"error_in_link")) %>% 
  unnest()


meltg <- df_links %>% 
  mutate(fecha = as.numeric(str_extract(link1, '\\d\\d\\d\\d')))

meltg$fecha[which(meltg$fecha== 1011)] <- NA #elimino error de parseo


fechas_all <- bind_rows(meltg, fechas_central %>% 
                          mutate(fecha = as.numeric(fecha))) 


fechas_all <- fechas_all %>% 
  group_by(autor) %>% 
  mutate(fecha = case_when(is.na(fecha) ~ floor(mean(lag(fecha), lead(fecha), na.rm = T, trim = 0)),
                           TRUE ~ fecha), #pongo como fecha la interpolacion de los textos consecutivos del autor
         fecha = case_when(is.na(fecha) ~ floor(mean(fecha, na.rm = T, trim = 0)), 
                           TRUE ~ fecha)) # pongo el promedio del autor para los que tenian NA a ambos lados



sum(is.na(fechas_all$fecha))

fechas_all %>% 
  filter(is.na(fecha)) %>% 
  group_by(autor) %>% 
  summarise(len=n()) %>% 
  arrange(-len)


fechas_all %>% 
  filter(autor=='Mikhail Bakunin') %>% 
  select(link)


# estos autores tienen una estructura muy distinta
# 
fechas_all %>%
  write_csv(.,'data/txt/fechas.csv')
# fechas_all  <-  read_csv('data/txt/fechas.csv')

fechas_all$autor[fechas_all$autor == "marx_engels"] <- "marx engels"

## limpieza y join

limpiar_textos <- function(x){
  
  x %>% 
    iconv(., from = 'UTF-8', to = 'ASCII//TRANSLIT') %>% 
    # rvest::repair_encoding(., from = Encoding(x)) %>%
    str_replace_all(pattern = "[^[:alnum:]]", replacement = " ") %>%
    str_replace_all(pattern = "(?i)#([0-9A-F]{2})\1{2}", replacement = " ") %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "<.*>", replacement = " ") %>%
    str_replace_all(pattern = "<[^>]*>", replacement = " ") %>%
    str_replace_all(pattern = "<.*?>", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_replace_all(pattern = "\\{ .* \\}", replacement = " ") %>%
    str_trim(side = "both")
}

fechas_all <- read_csv('data/txt/fechas.csv')

fechas_all <- fechas_all %>% 
  mutate(id = paste(autor, titulo, '_'),
      titulo = limpiar_textos(titulo))

texto_limpio <- read_csv('data/txt/texto_limpio.txt')

texto_limpio <- texto_limpio %>% 
  select(-link, -link1,-fecha, -id )

# texto_limpio <- read_rds('data/txt/textos_limpio.RDS')

texto_limpio <- texto_limpio %>% 
  left_join(fechas_all,by = c("autor", "titulo"))

texto_limpio <- texto_limpio %>% 
  distinct(autor, titulo,.keep_all = T)

texto_limpio %>% saveRDS('data/txt/textos_limpio.RDS')
texto_limpio %>% write_csv('data/txt/texto_limpio.txt')
