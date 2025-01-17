if (!requireNamespace("rvest", quietly = TRUE)) install.packages("rvest")
if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")

library(rvest)
library(httr)

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

# función principal, obtención del HTML a partir del url y retorna en artículo limpio mediante limpiar_html
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
  
  return(texto)
}

# prueba:

url_art <- "https://theconversation.com/los-dumbphones-o-telefonos-tontos-una-ayuda-en-la-tendencia-de-la-desconexion-digital-246452"
articulo <- extraer_articulo(url_art)
cat(articulo)

