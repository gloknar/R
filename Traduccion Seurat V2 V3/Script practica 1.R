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








