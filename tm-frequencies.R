# Pre-procesamiento de texto y Gráficas de Frecuencia
# author: Gonzalo Chacaltana

# Instalación de paquetes
# install.packages(c("tokenizers", "tidyverse","corpus", "pacman", "stopwords", "textstem", "udpipe"))

# Limpiar entorno de trabajo
rm(list = ls(all.names = TRUE))

# Definir entorno de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Librerias a utilizar
library(tokenizers)
library(dplyr)
library(tm)
library(wordcloud)
library(ggplot2)
require(plyr)

# Máxima cantidad de lineas a mostrar
options(max.print = 20)

# Importación de funciones
source("tm-functions.R")

###################################################
## VARIABLES
###################################################
encoding <- "UTF-8"
file_publication <- "datasets/Que-es-la-governanza-de-datos.txt"
file_stopwords <- "stopwords/stop_words_spanish.txt"
max_words <- 80

###################################################
## CARGA DE ARCHIVO DE TEXTO
###################################################

# Almacenamos el contenido del archivo de texto en la variable "publication"
publication <- readLines(file_publication, encoding = encoding)

# Mostramos un fragmento del contenido
str(publication)

# Almacenamos las palabras del contenido (tokenización de palabras)
words = tokenize_words(publication)

# Mostrar cantidad de palabras
length(words)

# Almacenos las párrafos (tokenización de párrafos)
sentences = tokenize_sentences(publication)

# Creamos data frame de la publicación
df_publication <- as.data.frame(publication)

###################################################
# ANÁLISIS DEL CORPUS
###################################################

# Analizando el corpus del contenido de la publicación
corpus <- Corpus(VectorSource(publication))

# Creamos la Matriz de documento de términos (Term Document Matrix)
corpus_tdm <- TermDocumentMatrix(corpus)
corpus_tdm
# Observamos que tenemos 730 palabras diferentes y 122 párrafos.

# Mostramos gráfico de nube de palabras
wordcloud_corpus(corpus)

###################################################
# LIMPIEZA DE TEXTO
###################################################

# Alternativa 1: Limpieza de texto utilizando funciones de R
publication_clean <- text_clean(publication)

# Alternativa 2: Limpieza de texto utilizando TM (Text Mining)
# publication_clean <- tm_clean(publication)

# Retirando stopwords (utilizamos archivo externo: stopwordses.txt)

stop_words <- readLines(file_stopwords,encoding = encoding)
stop_words = iconv(stop_words, to="ASCII//TRANSLIT")
publication_clean <- removeWords(publication_clean, words = stop_words)

# Analizando el corpus del texto limpio y sin stopwords
corpus_clean <- Corpus(VectorSource(publication_clean))

# Term Document Matrix (Matriz de documento de términos)
corpus_clean_tdm <- TermDocumentMatrix(corpus_clean)
corpus_clean_tdm
# Observamos que tenemos 515 palabras diferentes y 122 párrafos.

# Nube de palabras (corpus limpio)
wordcloud_corpus(corpus_clean)

###################################################
# FRECUENCIA DE PALABRAS
###################################################

# Creamos matriz para frecuencia de palabras.
matrix_freq <- as.matrix(corpus_clean_tdm)
dim(matrix_freq)
# Devuelve 515 122

matrix_freq <- matrix_freq %>% rowSums() %>% sort(decreasing = TRUE)
matrix_freq <- data.frame(palabra = names(matrix_freq), frec = matrix_freq)

# Mostramos nueva de palabras de la matriz de frecuencia
wordcloud(
    words = matrix_freq$palabra, 
    freq = matrix_freq$frec, 
    max.words = max_words, 
    random.order = F, 
    colors=brewer.pal(name = "Dark2", n = 8)
)

# Mostramos las 20 palabras con mayor frecuencia
matrix_freq[1:20, ]

###################################################
# GRÁFICAS DE FRECUENCIA
###################################################

col_fill_freq_qty <- "#FAC810"
col_fill_freq_per <- "#4AD395"

# Gráfica de frecuencia (en base a cantidades)
matrix_freq[1:10, ] %>%
    ggplot(aes(palabra, frec)) +
    geom_bar(stat = "identity", fill = col_fill_freq_qty, color = "black") +
    geom_text(aes(hjust = 1.5, label = frec)) + 
    coord_flip() + 
    labs(title = "LAS DIEZ PALABRAS MÁS FRECUENTES", y = "Cantidad de uso", x = "Palabras")

# Gráfica de frecuencia (en base a porcentajes)

matrix_freq %>%
    mutate(perc = (frec/sum(frec))*100) %>%
    .[1:10, ] %>%
    ggplot(aes(palabra, perc)) +
    geom_bar(stat = "identity", fill = col_fill_freq_per, color = "black") +
    geom_text(aes(hjust = 1.1, label = round(perc, 2))) + 
    coord_flip() +
    labs(title = "LAS DIEZ PALABRAS MÁS FRECUENTES", y = "Porcentaje de uso", x = "Palabras")
