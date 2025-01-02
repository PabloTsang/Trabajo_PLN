library(rvest)
library(httr)
library(quanteda)
library(reticulate)
library(ggplot2)

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
thc_crawler <- function(url_articulo, tiempo_espera, duracion) {
  # Llamar al crawler de Python para obtener las URLs relacionadas
  urls <- get_urls_rel(url_articulo)
  
  if (is.null(urls)) {
    stop("No se pudieron obtener URLs relacionadas.")
  }
  
  corpus <- list()  # Para almacenar los artículos descargados
  visited_urls <- c()  # Para rastrear las URLs que ya hemos visitado
  
  # Mientras no hayamos llegado al tiempo impuesto
  inicio <- Sys.time()
  while(as.numeric(difftime(Sys.time(), inicio, units = "secs")) < duracion) {
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

resultado <- thc_crawler("https://theconversation.com/por-que-tergiversamos-la-informacion-cuando-amenaza-nuestras-creencias-asi-funciona-el-razonamiento-motivado-243555",tiempo_espera =  1, duracion = 28800)
print(resultado)
docvarss <- docvars(resultado)

#Realizamos una ordenación para presentar los autores más frecuentes
autores_mas_frecuentes<- sort(table(docvarss$autoria), decreasing = TRUE)[1:10]

#Realizamos una ordenación para presentar cuáles son las instituciones con más publicaciones
instituciones_mas_frecuentes <- sort(table(docvarss$institucion), decreasing = TRUE)[1:10]

#Calculamos el número de artículos
numero_articulos <- length(resultado)

#Convertimos las keywords en tokens, eliminamos las stopwords para quedarnos sólo
#con las palabras esenciales, las convertimos en minúsculas y posteriormente en 
#un dataframe para realizar la nube de palabras
tokens <- tokens(docvarss$keywords, remove_punct = TRUE)
sin_stopwords <- tokens_remove(tokens, pattern = stopwords("es"))
token_minuscula <- tolower(sin_stopwords)
dfm <- dfm(token_minuscula)
#Asignamos una semilla para evitar la aletoriedad
set.seed(123)
nube_keywords <- quanteda.textplots::textplot_wordcloud(dfm)

#asignamos una lista para su posterior uso a la hora de modificar las fechas
meses <- c("enero" = "01", "febrero" = "02", "marzo" = "03", "abril" = "04", 
           "mayo" = "05", "junio" = "06", "julio" = "07", "agosto" = "08", 
           "septiembre" = "09", "octubre" = "10", "noviembre" = "11", "diciembre" = "12")

publicacion_cambiado <- docvarss$publicacion

#cambiar el nombre de los meses por su correspondiente valor numérico
for (mes in names(meses)) {
  publicacion_cambiado <- gsub(mes, meses[mes], publicacion_cambiado, ignore.case = TRUE)
}

#Sólo nos quedamos con la fecha y eliminamos la hora
solo_fecha <- lapply(publicacion_cambiado, function(x){
  substr(x, 1, nchar(x) - 10)
})
#Modificamos la separación acorde según el formato de las funciones empleadas
fecha_final <- gsub("\\s", "-", solo_fecha)

#Convertimos el string en una fecha,m  y posteriormente en un dataframe
fechas <- as.Date(fecha_final, format = "%d-%m-%Y")
datos <- data.frame(
  fecha = fechas
)

#creamos un rango de fechas para el índice del timeline
rango_fechas <- seq(min(fechas), max(fechas), by = "6 month")

#creación del timeline
ggplot(datos, aes(x = fechas)) +
  geom_segment(aes(xend = fechas, y = 0, yend = 1), color = "blue", size = 0.5) + 
  scale_x_date(
    breaks = rango_fechas, 
    date_labels = "%d-%m-%Y"
  ) + 
  labs(x = "Fecha", y = NULL, title = "Línea Temporal") +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(), 
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
