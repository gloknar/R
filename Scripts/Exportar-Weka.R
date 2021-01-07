# Exportar datos a Weka para analisis por ML
exportar_weka <- as.data.frame(LOQUESEA)
View(exportar_weka)
str(exportar_weka)

clases <- c("AT Deficient","AT Deficient","Control","Control")
exportar_weka <- cbind(exportar_weka, clases)
str(exportar_weka)


library(foreign)
write.arff(x=exportar_weka, file="jeje.arff", eol = "\n", relation = "Transcriptoma") # Pasamos el dataset de probes a archivo ARFF, para procesarlo en Weka
