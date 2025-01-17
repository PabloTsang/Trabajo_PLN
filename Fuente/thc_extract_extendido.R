if (!requireNamespace("rvest", quietly = TRUE)) install.packages("rvest")
if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
if (!requireNamespace("reticulate", quietly = TRUE)) install.packages("reticulate")

library(rvest)
library(httr)
library(reticulate)

# función de obtención de contenido del HTML y limpieza de este
limpiar_html <- function(lineas){
  
  contenido <- grep('.*<p>.*</p>',lineas, perl = TRUE, value = TRUE)
  solo_parrafos <- contenido[-grep('.*<p><.*>.*</.*></p>', contenido, perl = TRUE)]
  parrafos_sin_ws <- trimws(solo_parrafos, which = "both", whitespace = "[ \t\r\n]")
  sin_marks_parrafo <- gsub('<p>(.*)</p>', '\\1', parrafos_sin_ws)
  patron_etiquetas <- '<(/?a.*?|/?em|/?li|/?strong|/?div.*?)>'
  sin_etiquetas <- sapply(sin_marks_parrafo, function(x) gsub(patron_etiquetas, '', x))
  contenido_limpio <- paste(sin_etiquetas, collapse = "\n")
  
  return(contenido_limpio)
}

source_python("prueba_selnium_r.py")
# función principal, obtención del HTML a partir del url y retorna en artículo limpio mediante limpiar_html además de la lista de url relacionadas
extraer_articulo <- function(url){
  contenido <- tryCatch({
    readLines(url, warn = FALSE)
  }, error = function(e) {
    message(paste("Error al descargar la URL:", url))
    return(NULL)
  })
  
  if (is.null(contenido)) {
    message("Error al extraer el contenido de la url.")
    return(NULL)
  }
  
  texto <- limpiar_html(contenido)
  
  if (nchar(texto) == 0) {
    message("No se ha extraído ningún texto.")
    return(NULL)
  }
  
  # obtener lista de urls relacionadas
  urls_rel <- get_urls_rel(url)
  
  return(list(texto = texto, urls_rel = urls_rel))
}


#prueba 

url_art <- "https://theconversation.com/los-dumbphones-o-telefonos-tontos-una-ayuda-en-la-tendencia-de-la-desconexion-digital-246452"
artic_con_urls <- extraer_articulo(url_art)
articulo <- artic_con_urls$texto
urls <- artic_con_urls$urls_rel
cat(articulo)
print(urls)
