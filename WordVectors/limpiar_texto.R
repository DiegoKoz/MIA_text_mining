#' Función para limpiar el texto previo a tokenización por espacios blancos
#'
#' Esta función permite limpiar el texto normalizando espacios en blanco, quitando simbolos repetidos y agregando espacios en blanco alrededor de simbolos y puntuación para facilitar la tokenización.
#' Además es posible quitar números, quitar acentos, normalizar acentos, reemplazar lines breaks y carriage returns.
#'
#' @param x Character vector de textos.
#' @param quitar_stop_words Logical. TRUE reeemplaza todas las stopwords por ''.
#' @param quitar_numeros Logical. TRUE reemplaza los números por ''. 
#' @param to_lower Logical. TRUE pasa todo a minúscula.
#' @param reemplazar_line_breaks_carriage_return Logical. TRUE reemplaza todos los line breaks y carriage returns por 'lbr_cr' para que pueden 
#' ser tonekizados.
#' @param quitar_line_breaks_carriage_return Logical. Solo aplica si 'reemplazar_line_breaks_carriage_return' es TRUE también. Reemplaza los 'lbr_cr' por ''.
#' @param normalizar_acentos Logical. TRUE normaliza los acentos (ej. à -> á)
#' @param quitar_acentos Logical. TRUE quita los acentos del texto (ej. á -> a)
#' @param quitar_puntuacion_simbolos Logical. TRUE reemplaza todos los signos de puntuación y símbolos.
#' @param reemplazar_numeros Logical. TRUE reemplaza todos los números por el token especial '<NUM>'.
#' @param agrego_start_token Logical. TRUE agrega el token especial '<START>' al inicio del texto.
#' @param agrego_end_token Logical. TRUE agrega el token especial '<END>' al final del texto.
#'
#' @export
#' @examples
#' texto <- 'héllo wòrld2018'
#' limpiar_textos_para_tokenizar(texto)


limpiar_textos_para_tokenizar <-
    function (x,
              quitar_stop_words = FALSE,
              quitar_numeros = FALSE,
              to_lower = TRUE,
              reemplazar_line_breaks_carriage_return = TRUE,
              normalizar_acentos = TRUE,
              quitar_acentos = FALSE,
              quitar_line_breaks_carriage_return = FALSE,
              quitar_puntuacion_simbolos = FALSE,
              reemplazar_numeros = FALSE,
              agrego_start_token = FALSE,
              agrego_end_token = FALSE)
    {
        if (to_lower == TRUE) {
            x <- tolower(x)
        }

        if (quitar_numeros == TRUE) {
            x <- stringr::str_replace_all(x, stringr::regex("[0-9]*"),
                                          "")
        }
        if (quitar_puntuacion_simbolos == TRUE) {
            x <-
                stringr::str_replace_all(
                    x,
                    stringr::regex(
                        "(\\+|\\-|\\=|\\:|;|\\.|,|_|\\?|¿|\\!|¡|\\\\|\\(|\\)|\\||\\^|\\>|\\<|\\/|#|\\$|%|&|\"|\\*|\\{|\\}|`|\\[|´|\\]|@|¨|°|ª)"
                    ),
                    ""
                )
        }
        
        #arreglo line breaks y carriage returns repetidos
        x <- stringr::str_replace_all(x,
                                      '(\r\n)|(\n\r)',
                                      '\n') %>%
            stringr::str_replace_all('\n+',
                                     '\n') %>%
            stringr::str_replace_all('\r+',
                                      '\r') %>%
            stringr::str_replace_all('(\r\n)|(\n\r)',
                                      '\n') %>%
            stringr::str_replace_all('\n+',
                                     '\n')
            
        
        
        if (reemplazar_line_breaks_carriage_return == TRUE) {
            x <- stringr::str_replace_all(x, stringr::regex("[^[:print:]]|[\n\r]"),
                                          " lbr_cr ")
        }
        
        if (quitar_line_breaks_carriage_return == TRUE &
            reemplazar_line_breaks_carriage_return ==
            TRUE) {
            x <- stringr::str_replace_all(x, stringr::regex(" lbr_cr "),
                                          "")
        }
        
        x <-
            stringr::str_replace_all(
                x,
                stringr::regex(
                    ignore_case = T,
                    "(\\+|\\=|\\:|;|\\.|,|(?<!lbr)_|\\?|¿|\\!|¡|\\\\|\\(|\\)|\\||\\^|\\>|\\<|#|\\$|%|&|\"|\\*|\\{|\\}|`|\\[|´|\\]|@|¨|°|ª)"
                ),
                " \\1 "
            ) %>%
            stringr::str_replace_all(stringr::regex(ignore_case = T,
                                                    "([a-zA-Z])(\\/|\\-)"),
                                     "\\1 \\2") %>%
            stringr::str_replace_all(stringr::regex(ignore_case = T,
                                                    "(\\/|\\-)([a-zA-Z])"),
                                     "\\1 \\2") %>%
            stringr::str_replace_all(stringr::regex(ignore_case = T,
                                                    "([0-9])([a-zA-Z])"),
                                     "\\1 \\2") %>%
            stringr::str_replace_all(stringr::regex(ignore_case = T,
                                                    "([0-9])(\\/|\\-)(\\s)"),
                                     "\\1 \\2 \\3") %>%
            stringr::str_replace_all(stringr::regex(ignore_case = T,
                                                    "([0-9])(\\/|\\-)([a-zA-Z])"),
                                     "\\1 \\2 \\3") %>%
            stringr::str_replace_all(stringr::regex(ignore_case = T,
                                                    "(\\s)(\\/|\\-)([0-9])"),
                                     "\\1 \\2 \\3") %>%
            stringr::str_replace_all(stringr::regex(ignore_case = T,
                                                    "\\/{2,30}"), "/") %>%
            stringr::str_replace_all(stringr::regex(ignore_case = T,
                                                    "([a-zA-Z])([0-9])"),
                                     "\\1 \\2") %>%
            stringr::str_replace_all(stringr::regex(ignore_case = T,
                                                    "^(\\s){0,4}\\."), "") %>%
            stringr::str_replace_all(
                stringr::regex(
                    ignore_case = T,
                    "#|\\$|%|&|\"|\\*|\\{|\\}|`|\\[|´|\\]|@|¨|°|ª"
                ),
                " "
            ) %>%
            stringr::str_replace_all(stringr::regex(ignore_case = T,
                                                    "'"), " ") %>%
            stringr::str_replace_all(stringr::regex(ignore_case = T,
                                                    "(\\. )+|(\\. \\.)"), "\\.") %>%
            stringr::str_replace_all(stringr::regex(ignore_case = T,
                                                    "(\\. ?\\.)"), "\\.") %>%
            stringr::str_replace_all(stringr::regex(ignore_case = T,
                                                    "(?<=\\w)(\\.)"), " \\.") %>%
            stringr::str_replace_all(stringr::regex(ignore_case = T,
                                                    "(\\.)(?=\\w)"), "\\. ") %>%
            stringr::str_replace_all(stringr::regex(ignore_case = T,
                                                    "(\\.)+"), ".") %>%
            stringr::str_replace_all(stringr::regex(ignore_case = T,
                                                    "(\\s){2,10}"), " ")
       
        
        if(reemplazar_numeros == TRUE & quitar_numeros == FALSE){
            
            x <-
                stringr::str_replace_all(x,
                                         stringr::regex('[0-9]'),
                                         '<NUM>')
            
        }
        
        
         if (quitar_stop_words == TRUE) {
            x <-
                stringr::str_replace_all(x,
                                         stringr::regex(
                                             tm::stopwords(kind = 'es')
                                         ),
                                         "")
        }
        
        if (normalizar_acentos == TRUE) {
            x <- stringr::str_replace_all(x, c(
                `à`  = "á",
                `è`  = "é",
                `ì`  = "í",
                `ò`  = "ó",
                `ù` = "ú"
            ))
        }
        
        if (quitar_acentos == TRUE) {
            x <- stringr::str_replace_all(x,
                                          c(
                                              `à|á` = "a",
                                              `è|é` = "e",
                                              `ì|í` = "i",
                                              `ò|ó` = "o",
                                              `ù|ú` = "u"
                                          ))
        }
        
        
        if(agrego_start_token == TRUE){
          
          x <- paste('<START>',
                     x)
        }
        
        
        if(agrego_end_token == TRUE){
          
          x <- paste('<END>',
                     x)
        }
        

        return(x)
    }