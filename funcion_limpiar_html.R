limpiar_html <- function(url){
  
  lineas <- readLines(url, encoding = "UTF-8")
  contenido <- grep('.*<p>.*</p>',lineas, perl = TRUE, value = TRUE)
  solo_parrafos <- contenido[-grep('.*<p><.*>.*</.*></p>', contenido, perl = TRUE)]
  parrafos_sin_ws <- trimws(solo_parrafos, which = "both", whitespace = "[ \t\r\n]")
  sin_marks_parrafo <- gsub('<p>(.*)</p>', '\\1', parrafos_sin_ws)
  patron_etiquetas <- '<(/?a.*?|/?em|/?li|/?strong|/?div.*?)>'
  sin_etiquetas <- sapply(sin_marks_parrafo, function(x) gsub(patron_etiquetas, '', x))
  contenido_limpio <- paste(sin_etiquetas, collapse = "\n")
  
  return(contenido_limpio)
}
