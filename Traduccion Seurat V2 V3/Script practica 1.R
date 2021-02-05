# Práctica 1 en Seurat V3.2 y R 3.6.1

##################################################
#######        Encoded in UTF-8        ###########
##################################################



# Instalación monocle3 en R 3.6.1
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.10")

BiocManager::install(c('BiocGenerics', 'DelayedArray', 'DelayedMatrixStats',
                       'limma', 'S4Vectors', 'SingleCellExperiment',
                       'SummarizedExperiment', 'batchelor', 'Matrix.utils'))

install.packages("devtools")
devtools::install_github('cole-trapnell-lab/leidenbase')
devtools::install_github('cole-trapnell-lab/monocle3')


# Instalación Seurat V3 en R 3.6.1
remotes::install_version("Seurat", version = "3.2")


# Comprobamos versiones
packageVersion("Seurat")
packageVersion("monocle3")


##############################################



# Cargamos librerías
library("Seurat")
library("monocle3")

# Establecemos semilla de aleatoriedad
set.seed(1234567)


# Leemos matriz de conteos UMIs (contiene valores separados por tabuladores)
sc3 <- read.table("./Archivos accesorios/Codigo y datos de las Practicas 1-4/data/D3Ecounts.txt", sep = "\t", header = T)

head(sc3[1:10, 1:10])


# Formateamos la matriz para que quede más bonita y ordenada
rownames(sc3) <- sc3$Geneid
sc3 <- sc3[,2:ncol(sc3)]
head(sc3[1:10, 1:10])
