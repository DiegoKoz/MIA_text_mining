library(rvest)
library(tidyverse)



#### funciones #####

get_text <- function(link){
  html_text(read_html(link))
}
get_new_links <- function(link){
  
  nested_link <- read_html(link) %>% 
    rvest::html_nodes("a") %>% 
    html_attr(., "href") %>% 
    .[!str_detect(.,"../|NA|.doc|.zip|.pdf")] %>% 
    na.omit(.)
  
  nested_link <- unique(gsub("#.*","",nested_link))
  
  nested_link[nested_link!=""]
  
}

##### scrap #####

url <- "https://www.marxists.org/espanol/m-e/indice.htm"

webpage <- read_html(url)
refs <- webpage %>% 
  rvest::html_nodes("a")

df_links <- data.frame(titulo = html_text(refs),
           link = html_attr(refs, "href"))


#saco cosas que no son links a textos
df_links <- df_links[2:160,]

df_links <- df_links %>% 
  mutate(link = paste0("https://www.marxists.org/espanol/m-e/",link))


## Los libros tienen indices

#### texto simple ####

simple_links <- df_links %>% 
  filter(!str_detect(link, "index"))

simple_links <- simple_links %>% 
  mutate(texto = map(link,get_text))

saveRDS(simple_links, "data/links/simple_links.RDS")

simple_links <- read_rds("data/links/simple_links.RDS")


#### scrapeo libros ####

book_links <- df_links %>% 
  filter(str_detect(link, "index"))

book_links <- book_links %>% 
  mutate(nested_link = map(book_links$link, get_new_links))

book_links <- book_links %>%
  unnest(.) %>%
  mutate(nested_link = paste0(gsub("index.htm","",link),nested_link),
         texto = map(nested_link,get_text)) %>%
  unnest()

saveRDS(book_links, "data/links/books.RDS")
book_links <- read_rds("data/links/books.RDS")


## junto los textos

libros <- book_links %>% 
  group_by(titulo) %>% 
  summarise(texto = paste(texto, collapse = "/n"))


textos <- simple_links %>% 
  select(titulo, texto) %>% 
  unnest(.) %>% 
  group_by(titulo) %>% 
  summarise(texto = paste(texto, collapse = "/n")) %>% 
  bind_rows(libros)


## limpieza


# textos <- read_rds("data/txt/textos.RDS")

textos$texto <- textos$texto %>% 
  str_replace_all(pattern = "\n", replacement = " ") %>%
  str_replace_all(pattern = "[\\^]", replacement = " ") %>%
  str_replace_all(pattern = "\"", replacement = " ") %>%
  str_replace_all(pattern = "<!--.*-->", replacement = " ") %>%
  str_replace_all(pattern = "\\s+", replacement = " ") %>%
  str_trim(side = "both")

textos$titulo <- textos$titulo %>% 
str_replace_all(pattern = "\n", replacement = " ") %>%
  str_replace_all(pattern = "[\\^]", replacement = " ") %>%
  str_replace_all(pattern = "\"", replacement = " ") %>%
  str_replace_all(pattern = "<!--.*-->", replacement = " ") %>%
  str_replace_all(pattern = "\\s+", replacement = " ") %>%
  str_trim(side = "both")

## saco a her vogt que era un pdf

textos <- textos %>% 
  filter(titulo != "Herr Vogt")

saveRDS(textos, "textos.RDS")




           