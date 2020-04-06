




#Instalamos las librerias necesarias

#####################################################
#####################################################
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.10")
#####################################################
#####################################################
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("oligo")
browseVignettes("oligo") #Documentacion de la libreria oligo
#####################################################
#####################################################
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("limma")
browseVignettes("limma") #Documentacion de la libreria limma
#####################################################
#####################################################

#####################################################################
#####################################################################
#####################################################################

# Establecemos el directorio de trabajo

carpeta_archivos <- setwd("C:/Users/Adam_/Desktop/CRH/Proyecto 1 Transcriptoma/RawData transcriptome/")
getwd() #Nos debe salir la misma ruta que en la linea anterior
load(".RData")
#####################################################################
#####################################################################
#####################################################################
# library(affy) Clariom S Human es de 3a generacion y no es soportado por libreria affy



library(oligo) 
# ¡CUIDADO! The following object is masked from 'package:limma':
#  
#  backgroundCorrect
###############################################################
# Cuando uses limma, ten cuidado y detach oligo si usas el comando backgroundCorrect, ¿o es un objeto?

archivos <- list.celfiles(carpeta_archivos, full.names=TRUE)

rawData <- oligo::read.celfiles(archivos)
View(rawData)
str(rawData) # Vemos la estructura del dataframe rawData
dim(rawData)



nombres_muestras <- pData(rawData)
annotation(rawData) #anotacion usada para estos transcriptomas


#MA plot sin normalizar
# x <- rawData@assayData$exprs[,3:4]  # esto son WT
# y <- rawData@assayData$exprs[,1:2]  # Esto es AT Deficiency
plot (x= rawData@assayData$exprs[,3:4], y= rawData@assayData$exprs[,1:2], pch=".", main= "Unnormalized MA-plot", xlab= "WT intensity signal", ylab = "AT deficient intensity signal")



#Correcion de ruido de fondo y normalizacion de los datos
Expression_set <- oligo::rma(rawData) #mirar si implementa SST-RMA
dim(Expression_set)
View(Expression_set)
str(Expression_set)


# Intensidad de sennal de los genes, para las 4 muestras
Expression_set@assayData$exprs
probes <- exprs(Expression_set)
View(probes)
str(probes)
dim(probes)


# Generamos un scatter plot con la intensidad media de cada fila (=gen), y comparamos 
# el nivel de expresion de transcrito del wildtype (eje X) con el de la cepa con 
# la mutacion (eje Y)

intensidad_media_AT_Deficiency <- rowMeans(probes[,1:2])
intensidad_media_WT <- rowMeans(probes[,3:4])

Scatter_plot <- plot(x= intensidad_media_AT_Deficiency, y= intensidad_media_WT, pch=".", 
     main="Scatter plot of WT and AT Deficiency", 
     xlab="WT intensity signal (log2)", ylab= "AT Deficiency intensity signal (log2)")


ratio <- (intensidad_media_AT_Deficiency - intensidad_media_WT)  
# Calculamos M (el ratio de expresion de  las distintas condiciones). Se calcula como 
# log2(intensidad_patologia/intensidad_control) =  log2(intensidad_patologia) - log2(intensidad_control).
# Puesto que RMA nos devuelve las intensidades en log2, solo tenemos que restar dichas intensidades.


# Intepretacion= Puesto que ratio o M es el log2(AT/WT), si elevamos 2 a los valores de ratio, obtenemos
# el valor del ratio o division de AT/WT. Si AT/WT=1, no hay expresion diferencial, por tanto log2(1)=0
# Si AT/WT = 2, AT esta sobreexpresado en un factor de 2 respecto al control, por lo que log2(2)=1.
# Si AT/WT = 0.5 ó 2**(-1), significa que AT esta infraexpresado respecto al control en un factor de 2,
# por lo que log2(0.5) = -1. Es por esta razon que marco en el grafico los genes con valores de |M|> 1,
# pues estan diferencialmente expresados en un valor absoluto de 2 (i.e. estan infra o sobreexpresados en AT respecto WT en
# un factor de 2)



hist(ratio, xlab="ratio(log2)") # histograma con el ratio de intensidades entre AT Deficiency y WT, calculado como
# log2(AT)- log2(WT) 

hist(ratio, ylim=c(0,20), xlab="ratio(log2)") # Histograma anterior, ampliado para poder ver los genes con un ratio de 
# expresion AT/WT distinto de 0 

View(ratio)
# Vemos que el gen más sobrexpresado en deficiencia antitrombina pertenece al cluster de transcritos TC0600011814.hg.1, con 
# un ratio de	0.9786096(log2). Eso significa que dicho gen esta sobreexpresado en AT Deficiency respecto
# la condicion control en un factor de 2**0.9786096 = 1.970565

# Ademas, el gen mas infraexpresado en AT Deficiency pertenece al cluster de transcritos TC1000010870.hg.1, con 
# un ratio de	-1.0243555(log2). Eso significa que dicho gen esta infraexpresado en AT Deficiency respecto
# la condicion control en un factor de 2**1.0243555 = 2.034051


intensidad_media_global <- (intensidad_media_AT_Deficiency + intensidad_media_WT)/2


MA_plot <- plot(x=intensidad_media_global, y=ratio, xlab="A", ylab="M", main="MA-plot", abline(h=c(-1,1)))
# Los puntos con un |M|>1 significa que estan sobre o infraexpresados en AT respecto condicion control en un
# factor de 2.



groups <- factor(rep(c('AT-Deficiency', 'Wild Type'), each=2))
data.frame(sampleNames(Expression_set), groups)

## S4 method for signature 'ExpressionSet'
MA_plot_oligo <- MAplot(Expression_set, what=exprs, transfo=identity,
       refSamples=2, which=1, groups= groups, pch=".", summaryFun=rowMedians,
       plotFun=smoothScatter, pairs=FALSE, ylim=c(-.5, .5))




# Exportar datos a Weka para analisis por ML
exportar_weka <- as.data.frame(t(probes))
View(exportar_weka)
str(exportar_weka)

clases <- c("AT Deficient","AT Deficient","Control","Control")
exportar_weka <- cbind(exportar_weka, clases)
str(exportar_weka)


library(foreign)
write.arff(x=exportar_weka, file="jeje.arff", eol = "\n", relation = "Transcriptoma") # Pasamos el dataset de probes a archivo ARFF, para procesarlo en Weka

# No sabemos el nivel de significancia estadistica de estos genes, por lo que realizaremos un Volcano plot 
# para evaluar si realmente esos genes estan expresados diferencialmente, o es que dichas diferencias 
# se deben al azar (Recuerda que el analisis con TAC devolvio 21 genes cuyos niveles de expresion
# eran significativos a nivel estadistico con un p-valor<<0.05)

###################################################################

#Realizamos PCA a los datos.

probes_transpuesta <- t(probes) 

View(probes_transpuesta)
dim(probes_transpuesta)

# Calculamos la varianza de cada gen (=columna) de probes_transpuesta
rankings_varianza <- apply(probes_transpuesta, 2, var)
View(rankings_varianza)


# Ordenamos los transcritos de probes_transpuesta en funcion de su varianza (estan ordenados de mayor a menor, 
# puesto que transcritos con mayor varianza explican mas del fenomeno en cuestion)


transcritos_ordenados <- probes_transpuesta[,order(rankings_varianza, decreasing = TRUE)]
View(transcritos_ordenados)

xaxa <- rbind(transcritos_ordenados, rankings_varianza)
View(xaxa)


# Vamos a quedarnos con los 31 transcritos con mas varianza

#transcritos_31 <- transcritos_ordenados[,1:31]
transcritos_31 <- transcritos_ordenados[,]    # Los primeros 60 resultados eran sondas de calibrado, no utiles para el PCA
dim(transcritos_31)                                                      # Nota para mi: deberia ver como filtrar dichas sondas en R
View(transcritos_31)



####################################################################
####################################################################
####################################################################
#Ahora les hacemos el procedimiento del PCA

# Varianza transcritos
lista_nombres <- colnames(transcritos_31) 

# donde pone i in c(loquesea), escribe el rango de columnas (=transcritos) que quieras evaluar
for (i in c(1:20)) {
  print( paste("la varianza del transcrito", lista_nombres[i], "es ", var(transcritos_31[,i])))
}



#a) estudio de la idoneidad de la aplicacion del ACP. 

# Para ello, primero tenemos que testear si las variables en cuestion (expresion
# de 31 transcritos con mayor varianza) estan interrelacionadas entre si o si son
# independientes. Para ello, haremos el test de Bartlett con hipotesis:
# H0: |R| = 1
# H1: |R| != 1

# Calculamos la matriz de correlaciones
R <- cor(transcritos_31) ; View(R)
dim(R)

# Establecemos semilla de aleatoriedad para garantizar la reproducibilidad de
# nuestro trabajo y hacemos el test de esfericidad de Bartlett
set.seed(1)

library("psych")
cortest.bartlett(R, n=4)
cor.plot(R) # Grafico de correlaciones

# pvalor >> 0.05 ergo aceptamos H0. Las variables no estan
# correlacionadas, por lo que no es adecuado realizar el ACP. Probaremos a
# calcularemos la medida de adecuacion muestral de KMO

KMO(transcritos_31)


# La adecuacion global del ACP es 0.50 => El ACP es minimamente adecuado.



# b) Determinacion del numero optimo de componentes principales 
# Procedemos a realizar el PCA


condition <- factor(c("Deficient", "Deficient", "WT", "WT"), levels= c("Deficient", "WT"))
transcritos_31_condition <- cbind(transcritos_31, condition)

par(mfrow=c(1,1))
library("FactoMineR")
transcritos.pca <- PCA(transcritos_31_condition, scale.unit=T, ncp=10, graph=T,quali.sup=27131)


View(transcritos.pca$eig)
summary(transcritos.pca)


# Vamos a determinar el numero optimo de dimensiones mediante el criterio del
# Scree Test
set.seed(1)
library(nFactors)
ev <- eigen(cor(transcritos_31))
ap <- parallel(subject=nrow(transcritos_31),var=ncol(transcritos_31),rep=100,cent=.05)
nS <- nScree(ev$values,ap$eigen$qevpea)
plotnScree(nS)


# Solucion: El Scree Test nos da como solucion mas probable 3 dimensiones. 
# Se observa en la grafica un codo pronunciado al pasar de 3 a 4 dimensiones.
# Con 3 dimensiones explicamos el 100% de la varianza total de las variables

# Ademas, el criterio de Kaiser dice que mantengamos las dimensiones con un
# eingenvalue > 1 (=puntos por encima de la linea de triangulos verdes), por lo 
# que nos quedamos con 3 dimensiones.


# c) Variabilidad retenida por cada una de las componentes principales retenidas

transcritos.pca$eig
View(transcritos.pca$eig[1:3,2])
transcritos.pca$eig[1:3,2]

View(transcritos.pca$svd$V)

# Solucion: La dimension 1 retiene el 42.545% de la varianza total; la dimension
# 2 retiene el 34.107% de la varianza total; y la dimension 3 retiene el 23.438% restante.



# d) Interpretacion de tales componentes

dimdesc(transcritos.pca, axes=c(1:3))



# Criterios de interpretacion del coeficiente de correlacion simple de Pearson
# estimado a partir de una muestra (r) (Zubcoff Vallejo, Jose Jacobo, 2013, PDI
# de la Universidad de Alicante):
# r=   0:          ninguna
# r=(0-0.25):       baja
# r=(0.25-0.50):    media
# r=(0.50-0.75):    alta
# r=(0.75-1):     muy alta
# r=   1:         perfecta



# Solucion:

# La dimension 1 esta MUY altamente relacionada directamente con los niveles de expresion 
# del transcrito HTA2-neg-47419880_st (pvalor<<0.05) y muy inversamente correlacionado con
# los niveles de expresion del transcrito HTA2-neg-47420781_st (pvalor~ 0.05, posible relacion causal)
# Esto significa que a mayores valores de la dimension 1 (=condiciones de deficiencia de antitrombina), 
# la expresion del transcrito HTA2-neg-47419880_st aumenta, mientras que la del transcrito HTA2-neg-47420781_st diminuye.


# La dimension 2 ???


# La dimension 3 ???



# e) Expresion matematica de las componentes de interes

# Puesto que cada dimension es una combinacion lineal de las variables
# preexistentes, la expresion matematica de cada dimension es el sumatorio de
# cada variable multiplicada por su coeficiente correspondiente, el cual se
# encuentra en adam.pca$svd$v.


colnames(transcritos.pca$svd$V) <- c("Dim 1", "Dim2", "Dim 3")
rownames(transcritos.pca$svd$V) <- lista_nombres
View(transcritos.pca$svd$V)

for (x in c(1:3)) {
  print(colnames(transcritos.pca$svd$V)[x])
  print (transcritos.pca$svd$V[,x])
}

# Solucion (variables ordenadas de mayor a menor loading):

# Dim1 = -0.036723202*HTA2-pos-2985916_st + ... -0.262597610*HTA2-neg-47420781_st

# Dim2 = ...

# Dim3 = ...




#Primero representamos el grafico de la expresion genica en funcion de las dimensiones
par(mfrow=c(1,1))
par(col="black")
plot.PCA(transcritos.pca,axes=c(1,2), choix="ind", graph.type = "ggplot")
??plot.PCA

# Interpretacion: 





#############################################################################
##############################################################################
###############################################################################
# Probamos con limma




# Mira este enlace https://bioconductor.org/packages/release/workflows/vignettes/maEndToEnd/inst/doc/MA-Workflow.html
library("limma")
limmaUsersGuide()



if (!require("BiocManager"))
  install.packages("BiocManager")
BiocManager::install("maEndToEnd", version = "devel")

suppressPackageStartupMessages({library("maEndToEnd")})


#General Bioconductor packages
library(Biobase)
library(oligoClasses)

#Annotation and data import packages
library(ArrayExpress)
library(pd.hugene.1.0.st.v1)
library(hugene10sttranscriptcluster.db)

#Quality control and pre-processing packages
library(oligo)
library(arrayQualityMetrics)

#Analysis and statistics packages
library(limma)
library(topGO)
library(ReactomePA)
library(clusterProfiler)

#Plotting and color options packages
library(gplots)
library(ggplot2)
library(geneplotter)
library(RColorBrewer)
library(pheatmap)

#Formatting/documentation packages
#library(rmarkdown)
#library(BiocStyle)
library(dplyr)
library(tidyr)

#Helpers:
library(stringr)
library(matrixStats)
library(genefilter)
library(openxlsx)
#library(devtools)

