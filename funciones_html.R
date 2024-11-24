
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

sacar_publicacion <- function(lineas){
  
  patron_publicacion <- '<time datetime=.*>Publicado: (.*)</time>'
  publicacion_sucio <- grep(patron_publicacion, lineas, perl = TRUE, value = TRUE)
  publicacion_limpio <- gsub(patron_publicacion,'\\1', publicacion_sucio)
  publicacion_sin_ws <- trimws(publicacion_limpio, which = "both", whitespace = " ") 
  
  return(publicacion_sin_ws)
}

sacar_autoria <- function(lineas){
  
  patron_autor <- '<meta name="author" content="(.*)" />'
  autor_sucio <- grep(patron_autor, lineas, perl = TRUE, value = TRUE)
  autor_limpio <- gsub(patron_autor, '\\1', autor_sucio)
  autor_sin_ws <- trimws(autor_limpio, which = "both", whitespace = " ")
  
  return(autor_sin_ws)
}

sacar_institucion <- function(lineas){
  
  patron_institucion <- '.*<em class="institution_name">(.*)</em>'
  institucion_sucio <- grep(patron_institucion, lineas, perl = TRUE, value = TRUE)
  institucion_limpio <- gsub(patron_institucion, '\\1', institucion_sucio)
  institucion_sin_ws <- trimws(institucion_limpio, which = "both", whitespace = " ")
  
  return(institucion_sin_ws)
}

sacar_descripcion <- function(lineas){
  
  patron_descripcion <- '<meta property="og:description" content=(.*)/>'
  descripcion_sucio <- grep(patron_descripcion, lineas, perl = TRUE, value = TRUE)
  descripcion_limpio <- gsub(patron_descripcion, '\\1', descripcion_sucio)
  descripcion_limpio <- substring(descripcion_limpio, 2, nchar(descripcion_limpio)-2)
  descripcion_sin_ws <- trimws(descripcion_limpio, which = "both", whitespace = " ")

  return(descripcion_sin_ws)
}

sacar_keywords <- function(lineas){
  
  patron_keywords <- '<meta name="news_keywords" content="(.*?)" />'
  keywords_sucio <- grep(patron_keywords, lineas, perl = TRUE, value = TRUE)
  keywords_limpio <- gsub(patron_keywords, '\\1', keywords_sucio)
  keywords_sin_ws <- trimws(keywords_limpio, which = "both", whitespace = " ")
  
  return(keywords_sin_ws)
}

