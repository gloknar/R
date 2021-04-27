#################################################################################### 
# Ejercicio a entregar (Analisis de datos omicos 2019-2020)
#####################################################################################
setwd("/Users/pmb59/Desktop/umu")  # aqui debeis tener los archivos necesarios (los dos .csv)
require(data.table)
library(dplyr)

#leemos la tabla de expression genica normalizada 
sc = fread('SCPdata_seurat_normalized_expression_counts_subset_absorptiveAndCryptentero.csv')
# imprime las 5 primeras filas y 3 primeras columnas en la terminal
sc[1:5, 1:3]

#la tabla tiene expression de 22700 genes para una de las 11855 celulas
dim(sc)
#como veis, es similar a la tabla de la Practica 1 (pagina 8 del manual de practicas)

#vamos a convertirlo en data.frame (para usarlo en la funcion CreateSeuratObject)
sc_df <- as.data.frame(sc)
sc_df[1:5, 1:3]


#al igual que en la Practica 1, eliminamos la primera columna:
rownames(sc_df) <- sc_df$GENE
Nc <- ncol(sc_df) # numero de celulas
sc_df <- sc_df[,2:Nc]
sc_df[1:5, 1:3]


#ahora cargamos la libreria Seurat y creamos el objecto con nuestros datos
library(Seurat)
# min.cells = 0, min.features = 0 (no hacemos Quality-Control, ya lo hicieron los autores)
sc_seurat <- CreateSeuratObject(sc_df, project = "SeuratProject", assay = "RNA",
                                min.cells = 0, min.features = 0, names.field = 1,
                                names.delim = "_3p_", meta.data = NULL)

sc_seurat



# el metadata del objeto Seurat se puede ver haciendo
head(sc_seurat[[]] )
#utilizando el otro archivo CSV vamos a anadir metada que indica a que 
#cluster (segun el articulo) pertenece cada celula
metadata <- read.csv("SCPdata_20200311_tsne_epith_noninflamed.csv",header = TRUE)
head(metadata)
#eliminamos la primera fila ya que no contiene info valida
metadata <- metadata[-1,]
head(metadata)
#creamos un data.frame con el nombre de nuestras celulas
names_nuestras_celulas <- data.frame(NAME=rownames(sc_seurat[[]] ))
head(names_nuestras_celulas )
grupos_celulares <- merge(x=names_nuestras_celulas, y=metadata, by='NAME' , sort=FALSE)
head(grupos_celulares )
#Y anadimos nuevo metadata al objeto Seurat
sc_seurat$cluster<- grupos_celulares$cluster
head(sc_seurat[[]] )



#como se ha comentado en el video y el pdf de la clase, los datos ya estan normalizados. No es necesario hacer:
#sc_seurat <- NormalizeData(object = sc_seurat)

sc_seurat <- FindVariableFeatures(object = sc_seurat)
sc_seurat <- ScaleData(object = sc_seurat)
sc_seurat <- RunPCA(object = sc_seurat)
sc_seurat <- FindNeighbors(object = sc_seurat)
#sc_seurat <- FindClusters(object = sc_seurat)  # se puede hacer, pero no es necesario

# Visualiza el PCA
DimPlot(sc_seurat , reduction = "pca",group.by ="cluster")

#Crea UMAP (con 20 componentes) y visualizalo
sc_seurat <- RunUMAP(sc_seurat, dims = 1:20)
DimPlot(sc_seurat , reduction = "umap", group.by ="cluster")


# Ver la expression de ACE2 y TMPRSS2 en el UMAP
FeaturePlot(sc_seurat, features = c("ACE2", "TMPRSS2") )
# Otra forma de verlo usando un dotplot:
DotPlot(sc_seurat, features =  c("ACE2", "TMPRSS2"), cols = c("blue","red"), dot.scale = 8, group.by = "cluster" ) + RotatedAxis()


# que celulas expresan esos dos marcadores simultaneamente?
# Podemos poner un cut-off de expression normalizada = 1
doble_positivo <- which( sc_df["ACE2", ]> 1 & sc_df["TMPRSS2", ]> 1 )
#Vemos que son solamente 316 celulas:
length(doble_positivo )


#y anadimos esa informacion al objeto Seurat
sc_seurat$doblePositivo <- 'No'
sc_seurat$doblePositivo[doble_positivo] <- 'Yes'
head(sc_seurat[[]] )
# Y visualizamos el UMAP:
DimPlot(sc_seurat , reduction = "umap", group.by ="doblePositivo", cols =c('lightgray','red'))
#Y el PCA:
DimPlot(sc_seurat , reduction = "pca", group.by ="doblePositivo", cols =c('lightgray','red'))


#############################################################################################
#  CONTINUAR EL EJERCICIO:
#############################################################################################

# Ahora que sabeis que células expresan los dos marcadores (doblePositivo='Yes'), y las que no (doblePositivo='No'),
# teneis que encontrar que otros genes (al menos los 5 primeros) tienen co-expression con las celulas en 
# las doblePositivo='Yes', y visualizarlas utilizando las funciones FeaturePlot y DotPlot.


# si tuvieses que seleccionar solamente UNO de los genes encontrados para hacer mas experimentos, 
# cual escogeriais y por que?







