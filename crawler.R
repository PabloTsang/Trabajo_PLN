library(rvest)
library(httr)
library(quanteda)
library(reticulate)
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
source_python("prueba_selnium_r.py")
thc_crawler <- function(url_articulo, tiempo_espera, num_articulos) {
  # Llamar al crawler de Python para obtener las URLs relacionadas
  urls <- get_urls_rel(url_articulo)
  
  if (is.null(urls)) {
    stop("No se pudieron obtener URLs relacionadas.")
  }
  
  corpus <- list()  # Para almacenar los artículos descargados
  visited_urls <- c()  # Para rastrear las URLs que ya hemos visitado
  
  # Mientras no hayamos llegado al número deseado de artículos
  while(length(corpus) < num_articulos) {
    # Elegir una URL relacionada de la lista (solo si no se ha visitado antes)
    url <- urls[1]
    urls <- urls[-1]  # Eliminar la URL seleccionada de la lista de URLs a explorar
    if (url %in% visited_urls) {
      next  # Si ya hemos visitado esta URL, saltamos al siguiente
    }
    
    # Agregar la URL a la lista de URLs visitadas
    visited_urls <- c(visited_urls, url)
    
    # Descargar contenido
    contenido <- tryCatch({
      readLines(url, warn = FALSE)
    }, error = function(e) {
      message(paste("Error al descargar la URL:", url))
      return(NULL)
    })
    
    if (is.null(contenido)) next
    
    # Limpiar y extraer la información
    texto <- limpiar_html(contenido)
    publicacion <- sacar_publicacion(contenido)
    autoria <- sacar_autoria(contenido)
    institucion <- sacar_institucion(contenido)
    descripcion <- sacar_descripcion(contenido)
    keywords <- sacar_keywords(contenido)
    
    # Guardar en el corpus
    documento <- list(
      texto = texto,
      publicacion = publicacion,
      autoria = autoria,
      institucion = institucion,
      descripcion = descripcion,
      keywords = keywords,
      url = url
    )
    corpus[[length(corpus) + 1]] <- documento
    
    # Obtener más URLs relacionadas del artículo actual
    urls_relacionadas <- get_urls_rel(url)
    
    # Añadir estas nuevas URLs a la lista de URLs a explorar
    if (!is.null(urls_relacionadas)) {
      # Filtrar URLs para que no estén ya en la lista
      urls <- unique(c(urls, setdiff(urls_relacionadas, visited_urls)))
    }
    
    Sys.sleep(tiempo_espera)  # Esperar antes de cada nueva petición
  }
  
  # Crear un corpus de quanteda con los textos descargados
  docs <- sapply(corpus, function(doc) doc$texto)
  metadata <- data.frame(
    publicacion = sapply(corpus, function(doc) ifelse(is.null(doc$publicacion), "", doc$publicacion)),
    autoria = sapply(corpus, function(doc) ifelse(is.null(doc$autoria), "", doc$autoria)),
    institucion = sapply(corpus, function(doc) ifelse(is.null(doc$institucion), "", doc$institucion)),
    descripcion = sapply(corpus, function(doc) ifelse(is.null(doc$descripcion), "", doc$descripcion)),
    keywords = sapply(corpus, function(doc) ifelse(is.null(doc$keywords), "", doc$keywords)),
    url = sapply(corpus, function(doc) ifelse(is.null(doc$url), "", doc$url)),
    stringsAsFactors = FALSE
  )
  
  quanteda_corpus <- corpus(docs, docvars = metadata)
  return(quanteda_corpus)
}

resultado <- thc_crawler("https://theconversation.com/por-que-tergiversamos-la-informacion-cuando-amenaza-nuestras-creencias-asi-funciona-el-razonamiento-motivado-243555",tiempo_espera =  2, num_articulos = 10)
print(resultado)
docvarss <- docvars(resultado)
