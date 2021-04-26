# Text Mining Functions: Funciones para minería de texto

library(tidyverse)
library(tm)

# Función para limpiar texto con TM Package (Text Mining)
tm_clean <- function(corpus) {
    text  <- tm_map(corpus, content_transformer(tolower))
    text  <- tm_map(text, stripWhitespace)
    text  <- tm_map(text, removePunctuation)
    text  <- tm_map(text,removeNumbers)
    text  <- tm_map(text, removeWords, stopwords("spanish"))
    return(text)
}

# Función para limpiar el contenido de un texto.
text_clean <- function(text) {
    special_chars <- "áéíóú"
    special_chars_replace <- "aeiou"
    # Se convierte todo el texto a minúsculas
    new_text <- tolower(text)
    # Eliminación de páginas web (palabras que empiezan por "http.")
    new_text <- str_replace_all(new_text,"http\\S*", "")
    # Eliminación de signos de puntuación
    new_text <- str_replace_all(new_text,"[[:punct:]]", " ")
    # Eliminación de números
    new_text <- str_replace_all(new_text,"[[:digit:]]", " ")
    # Eliminación de espacios en blanco múltiples
    new_text <- str_replace_all(new_text,"[\\s]+", " ")
    # Reemplazo de carecteres especiales
    new_text <- chartr(special_chars,special_chars_replace,new_text)
    return(new_text)
}

# Función que devuelve wordcloud de un contenido de texto
wordcloud_corpus <- function(corpus) {
    # Matriz de términos (Term-Document-Matrix)
    tdm = TermDocumentMatrix(corpus, control = list(removePunctuation = TRUE, removeNumbers = TRUE, tolower = TRUE))
    matrix_tdm = as.matrix(tdm)
    
    # Obtenemos las palabras frecuentes de mayor a menor
    words_freqs = sort(rowSums(matrix_tdm), decreasing=TRUE)
    
    # Creamos dataset de palabras frecuentes
    data_words_freqs = data.frame(word=names(words_freqs), freq=words_freqs)
    
    # Mostramos la nube de palabras
    wordcloud(data_words_freqs$word, data_words_freqs$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
}
