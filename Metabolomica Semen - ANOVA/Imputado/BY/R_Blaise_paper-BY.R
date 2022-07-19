# Fuente: Blaise paper  --- https://www.nature.com/articles/s41596-021-00579-1#code-availability

# 12. Importamos los datos preprocesados y normalizados. Los datos que usamos
# son datos de peak intensity de metabolitos (sacados de HPLC-MS), normalizados
# por masa de eyaculado y transformados por ln(), luego les añadimos la edad y
# el BMI y los normalizamos todos (metabs + edad y BMI) con center + scale de
# caret, que es casi identico al escalado por pareto. Finalmente eliminamos con
# caret::nzv() los metabolitos con varianza igual o cercana a 0

# Creamos carpeta de visualizacions si no la tenemos
if (!dir.exists("visualizaciones")) {
  dir.create("visualizaciones")
}


library(readxl)
dataset_semen <- readxl::read_excel("../MassNormImpDataR.xlsx", na = "Missing")
class(dataset_semen)
dataset_semen <- as.data.frame(dataset_semen)
dataset_semen[1:5,1:5]
str(dataset_semen[,1:10])

nombres_pacientes <- dataset_semen$Sample
rownames(dataset_semen) <-nombres_pacientes
dataset_semen$CENTRE <- as.factor(dataset_semen$CENTRE)
dataset_semen$DIAGNOSIS <- make.names(dataset_semen$DIAGNOSIS)
dataset_semen$DIAGNOSIS <- as.factor(dataset_semen$DIAGNOSIS)
dataset_semen$GROUP <- as.factor(dataset_semen$GROUP)
dataset_semen <- dataset_semen[,-1] # Eliminamos la columna de los nombres de los pacientes


# Eliminamos metabolitos con varianza = 0
metricas_nzv <- caret::nzv(dataset_semen,saveMetrics = T)
eliminar <- which(metricas_nzv[,"zeroVar"] == T)
dataset_semen <- dataset_semen[,-eliminar]


# Mirar correlaciones de pearson entre las covariables (ninguna sale con más de 85% de correlacion)
cor(dataset_semen$AGE, dataset_semen$BMI, use = "complete.obs", method = "pearson") # entre edad y BMI
matriz_cor <- cor(dataset_semen[,c(2:6)], use = "complete.obs", method = "pearson") # matriz de correlacion
png("visualizaciones/Corrplot_covariables", width = 720, height = 720)
corrplot::corrplot(matriz_cor, method = "number", type = "upper")
dev.off()


# Adicionalmente, mirar correlacion entre centro y BMI/edad tal
library(ltm)
png("visualizaciones/biserialCor_Centro_Edad", width = 720, height = 720)
correlation <- biserial.cor(dataset_semen$AGE, dataset_semen$CENTRE)
if (is.na(correlation)) {
  correlation <- 0
}
texto <-  paste0("biserial correlation = ", format(correlation, digits = 3))
boxplot(AGE ~ CENTRE, data = dataset_semen, col = c("green", "cyan"))
mtext(text = texto, side = 3, line = -2, outer = F, at = 1)
dev.off()


png("visualizaciones/biserialCor_Centro_BMI", width = 720, height = 720)
correlation <- biserial.cor(dataset_semen$BMI, dataset_semen$CENTRE)
if (is.na(correlation)) {
  correlation <- 0
}
texto <-  paste0("biserial correlation = ", format(correlation, digits = 3))
boxplot(BMI ~ CENTRE, data = dataset_semen, col = c("green", "cyan"))
mtext(text = texto, side = 3, line = -2, outer = F, at = 1)
dev.off()


png("visualizaciones/biserialCor_Centro_Concentracion", width = 720, height = 720)
correlation <- biserial.cor(dataset_semen$CONCENTRATION_MILLIONS_ML, dataset_semen$CENTRE)
if (is.na(correlation)) {
  correlation <- 0
}
texto <-  paste0("biserial correlation = ", format(correlation, digits = 3))
boxplot(CONCENTRATION_MILLIONS_ML ~ CENTRE, data = dataset_semen, col = c("green", "cyan"))
mtext(text = texto, side = 3, line = -2, outer = F, at = 1)
dev.off()


png("visualizaciones/biserialCor_Centro_Motilidad", width = 720, height = 720)
correlation <- biserial.cor(dataset_semen$PERCENT_PROGRESSIVE_MOVILITY, dataset_semen$CENTRE)
if (is.na(correlation)) {
  correlation <- 0
}
texto <-  paste0("biserial correlation = ", format(correlation, digits = 3))
boxplot(PERCENT_PROGRESSIVE_MOVILITY ~ CENTRE, data = dataset_semen, col = c("green", "cyan"))
mtext(text = texto, side = 3, line = -2, outer = F, at = 1)
dev.off()


# Center y edad estan muy correlaciones, eliminamos centro
dataset_semen <- dataset_semen[,-1]
View(dataset_semen)

# Preprocesamos con pareto (no afecta a las correlaciones, yuju!)
factors <- unlist(lapply(dataset_semen, is.factor))
factor_cols <- which(factors == T)

num_cols <- unlist(lapply(dataset_semen, is.numeric))
dataset_semen[,num_cols] <-  pcaMethods::prep(dataset_semen[,num_cols], scale = "pareto", center = T)



# Por último comprobamos NAs
NA_cols <- unlist(lapply(dataset_semen, anyNA))
colnames(dataset_semen[,NA_cols])
summary(dataset_semen[,NA_cols])

# Hay NAs en las covariables Volume, concentracion y motilidad progresiva, las imputamos
dataset_semen <- VIM::kNN(dataset_semen, variable = colnames(dataset_semen[,NA_cols]), imp_var = F) # Com imp_var = F evitamos que nos cree las columnas adicionales innecesarias
# NOTA: Al imputar conVIM::kNN, se pierden los nombres de las filas (por algún motivo), volvemos a ponerlas si es que no las pusimos antes
rownames(dataset_semen) <- nombres_pacientes




# Comprobamos si hay diferencias en las covariables de los grupos (El
# estadístico t y el p-valor son iguales tanto antes como despues del Pareto,
# yuju!!!). 
edad_control <- dataset_semen$AGE[dataset_semen$GROUP == "Control"]
edad_caso <- dataset_semen$AGE[dataset_semen$GROUP == "Caso"]


# Primero medimos normalidad
# El grupo más pequeño es n = 32, entonces usamos Shapiro-Wilk para ambas
# comprobaciones
shapiro.test(edad_control) # Significativo: no son normales
shapiro.test(edad_caso)   # Significativo: no son normales (como n > 30, podríamos asumir que son normales)


# Ahora comprobamos homocedasticidad
# Como los datos no son normales, tenemos que usar Levene
car::leveneTest(AGE ~ GROUP, data = dataset_semen)  # p-valor no significativo: hay homocedasticidad


# Como n de casos > 30, asumimos que se aproximan a una distribución normal, por lo que tocaría t.test
t.test(edad_control, edad_caso, alternative = "two.sided", var.equal = T)  # No hay diferencias, borramos
# t.test(AGE ~ GROUP, data = dataset_semen, alternative = "two.sided", var.equal = T)



# Ídem para BMI
bmi_control <- dataset_semen$BMI[dataset_semen$GROUP == "Control"]
bmi_caso <- dataset_semen$BMI[dataset_semen$GROUP == "Caso"]

# Normalidad
shapiro.test(bmi_control) # Significativo: no son normales
shapiro.test(bmi_caso)   # Significativo: no son normales (como n > 30, podríamos asumir que son normales)

# Homocedasticidad
car::leveneTest(BMI ~ GROUP, data = dataset_semen)  # p-valor no significativo: hay homocedasticidad

# Como n de casos > 30, asumimos que se aproximan a una distribución normal, por lo que tocaría t.test
t.test(BMI ~ GROUP, data = dataset_semen, alternative = "two.sided", var.equal = T) # No hay diferencias, eliminamos BMI y EDAD del anova



# 13. Notice that the PCA method outlined in this notebook provides built-in
# scaling functionality, so no data-scaling step is required prior to this.
# Select the scaling you require.
library(FactoMineR)
pca_semen <- PCA(X = dataset_semen, scale.unit = FALSE, quali.sup = c(factor_cols))
dev.off()

png("visualizaciones/pca_95CI_means", width = 720, height = 720)
plotellipses(pca_semen, means = T)
dev.off()

png("visualizaciones/pca_95CI_pointsCloud", width = 720, height = 720)
plotellipses(pca_semen, means = F)
dev.off()


png("visualizaciones/pca_varcor", width = 720, height = 720)
FactoMineR::plot.PCA(pca_semen, choix = "varcor", 
                     select = c("CONCENTRATION_MILLIONS_ML", "PERCENT_PROGRESSIVE_MOVILITY", "AGE", "BMI"), 
                     unselect = 1, label = "var", autoLab = "no")
dev.off()


# BIPLOT
library(factoextra)

# png("visualizaciones/biplot_center_names", width = 720, height = 720)
# fviz_pca_biplot(pca_semen,  geom = c("text"), habillage = factor_cols[1], col.var = "black",
#                 select.var = list(name = c("CONCENTRATION_MILLIONS_ML", "PERCENT_PROGRESSIVE_MOVILITY"))) + ggtitle("PCA Biplot (centro)") 
# dev.off()
# 
# 
# png("visualizaciones/biplot_center", width = 720, height = 720)
# fviz_pca_biplot(pca_semen,  geom = c("point"), habillage = factor_cols[1], col.var = "black",
#                 select.var = list(name = c("CONCENTRATION_MILLIONS_ML", "PERCENT_PROGRESSIVE_MOVILITY"))) + ggtitle("PCA Biplot (centro)") 
# dev.off()



png("visualizaciones/biplot_diagnosis_names", width = 720, height = 720)
fviz_pca_biplot(pca_semen,  geom = c("text"), habillage = factor_cols[1], col.var = "black",
                select.var = list(name = c("CONCENTRATION_MILLIONS_ML", "PERCENT_PROGRESSIVE_MOVILITY"))) + ggtitle("PCA Biplot (diagnosis)") 
dev.off()


png("visualizaciones/biplot_diagnosis", width = 720, height = 720)
fviz_pca_biplot(pca_semen,  geom = c("point"), habillage = factor_cols[1], col.var = "black",
                select.var = list(name = c("CONCENTRATION_MILLIONS_ML", "PERCENT_PROGRESSIVE_MOVILITY"))) + ggtitle("PCA Biplot (diagnosis)") 
dev.off()



png("visualizaciones/biplot_group_names", width = 720, height = 720)
fviz_pca_biplot(pca_semen,  geom = c("text"), habillage = factor_cols[2], col.var = "black",
                select.var = list(name = c("CONCENTRATION_MILLIONS_ML", "PERCENT_PROGRESSIVE_MOVILITY", "AGE", "BMI"))) + ggtitle("PCA Biplot (grupo)") 
dev.off()


png("visualizaciones/biplot_group", width = 720, height = 720)
fviz_pca_biplot(pca_semen,  geom = c("point"), habillage = factor_cols[2], col.var = "black",
                select.var = list(name = c("CONCENTRATION_MILLIONS_ML", "PERCENT_PROGRESSIVE_MOVILITY"))) + ggtitle("PCA Biplot (grupo)") 
dev.off()



################################### PRUEBAS ELIMINAR OUTLIERS, NO MEJORA EL PCA pero sí el PLS-DA

# pca_semen$ind$coord[,1] <- pca_semen$ind$coord[,1] *-1 
# clusters_pca <- kmeans(pca_semen$ind$coord, centers = 3)
# clusters_pca <- kmeans(dataset_semen[,num_cols], centers = 3, iter.max = 100, nstart = 100) # Con estos parámetros converge el algoritmo
# 
# plot(pca_semen)
# 
# fviz_cluster(clusters_pca, data = dataset_semen[,num_cols],
#              palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
#              geom = "text",
#              ellipse.type = "convex", 
#              ggtheme = theme_bw(), stand = F)
# 
# clusters_pca$cluster
# eliminar_outliers <- which(clusters_pca$cluster == 1)
# 
# dataset_semen_outliers <- dataset_semen[-eliminar_outliers,]
# 
# pca_2 <- PCA(X = dataset_semen_outliers, scale.unit = FALSE, quali.sup = c(factor_cols))
# 
# FactoMineR::plot.PCA(pca_2, choix = "varcor", 
#                      select = c("CONCENTRATION_MILLIONS_ML", "PERCENT_PROGRESSIVE_MOVILITY"), 
#                      unselect = 1, label = "var", autoLab = "no")
# 
# plotellipses(pca_2, means = F)
# 
# pca_semen2 <- pcaMethods::pca(dataset_semen_outliers, method = "svd", nPcs = 25, 
#                               center = FALSE, scale = "none", cv = "q2") # method = "svd" si no tienes NAs en los datos; method = "nipalsPca" si tienes pocos NAs, el resto de métodos por si tienes más NAs
# 
# summary(pca_semen2)
# plot(pca_semen2, main = "")
# 
# 
# pca_semen2 <- pcaMethods::pca(dataset_semen_outliers, method = "svd", nPcs = 2, 
#                               center = FALSE, scale = "none", cv = "q2") # method = "svd" si no tienes NAs en los datos; method = "nipalsPca" si tienes pocos NAs, el resto de métodos por si tienes más NAs
# 
# summary(pca_semen2)
# plot(pca_semen2, main = "")
# 
# 
# library(ropls)
# 
# dataset_semen_outliers[,-factor_cols]
# mdl <- ropls::opls(dataset_semen_outliers[, -factor_cols], dataset_semen_outliers$GROUP, crossvalI=10,
#                    predI = NA, permI = 1000, scaleC = "none")
# 
# plot(mdl)
# 
# ropls::getSummaryDF(mdl)
# cosa <- getVipVn(mdl)
# cosa[cosa > 1]
# range(cosa)
# usar <- which(cosa > 1)
################################################################################



# Cogemos las 25 primeras componentes para los análisis downstream porque
# explican el 80% de la varianza total de los datos. Por cierto, el R^2 es el
# porcentaje de varianza explicada!!! Compara los valores que te devuelve el
# prcomp() y el pcaMethods::pca(), son los mismos!
summary(prcomp(dataset_semen[,num_cols])) 



# 14. Fit a PCA model to your X matrix (with your chosen scaling method), and draw a
# bar plot of the R2 and Q2 values for the resulting model (Fig. 7a). Use the
# bar plot to select a the number of components to retain in the PCA model. This
# can be done by identifying the number of components where the increase in Q2X
# stabilizes (e.g., increase of <5%).
library(pcaMethods)
pca_semen2 <- pcaMethods::pca(dataset_semen, method = "svd", nPcs = 25, 
                              center = FALSE, scale = "none", cv = "q2") # method = "svd" si no tienes NAs en los datos; method = "nipalsPca" si tienes pocos NAs, el resto de métodos por si tienes más NAs

summary(pca_semen2) 

# Segun el gráfico, El q2 optimo sería para 2 componentes principales
png("visualizaciones/PCA_goodness-of-fit1", width = 720, height = 720)
plot(pca_semen2, main = "")
dev.off()




# 15. Run a further cross-validated PCA using the previously selected scaling
# and optimal number of components.
pca_semen3 <- pca(dataset_semen, method="svd", nPcs = 2, center = FALSE, 
                  scale = "none", cv = "q2")

png("visualizaciones/PCA_goodness-of-fit2", width = 720, height = 720)
plot(pca_semen3)
dev.off()



#### Identification of outliers

# 16. Plot the scores with a Hotelling’s T2 95% ellipse. In general, samples
# with scores that lie outside the ellipse are candidates for outliers

library(mdatools)
pca_semen4 <- mdatools::pca(dataset_semen[,num_cols], 2, scale = F)

png("visualizaciones/Hotteling-test", width = 720, height = 720)
p <- plotScores(pca_semen4)
# mdaplot(pca_semen4$res$cal$scores, type = "p", show.labels = TRUE, show.lines = c(0, 0),  
#         xlim = c(-8, 8), ylim = c(-8, 8), col = colores_visualizaciones)
mdaplot(pca_semen4$res$cal$scores, type = "p", show.labels = TRUE, show.lines = c(0, 0),  
        cgroup = dataset_semen$GROUP, show.colorbar = T)
plotHotellingEllipse(p, conf.lim = 0.95, col = "#a0a0a0", lty = 3)

dev.off()


# 17.(Optional) use the DmodX measure plot to detect samples for which there is
# a lot of residual variance unexplained by the model. These are also candidate
# outliers.



# 18. Investigate these outliers by plotting the raw data, loadings for the
# principal components pointing towards these potential outliers, the residual
# or the model reconstruction for these samples. Use the scores plot to find
# which loading vectors to inspect (PC1 and PC2 on Fig. 7b). Based on this
# investigation, consider the removal of the outliers from the dataset, and
# rerun the model (Fig. 7c,d with color coding based on the main factor of
# variance, i.e., age).

# Eliminamos los pacientes outliers C-007, G-010, C-049. Al hacerlo, el PCA no mejora,
# pero el PLS-DA sí
outliers <- which(rownames(dataset_semen) %in% c("C-007", "G-010", "C-049") ) 
dataset_semen_outliers <- dataset_semen[-outliers,]




#### Supervised analysis with PLS-DA

# 19. Open the notebook with the title ‘Multivariate Analysis – PLS-DA’.

# 20. Start by selecting the type of scaling, and run a PLS-DA model to see
# whether your samples can be discriminated with a supervised approach.
library(ropls)
set.seed(1) # Aunque cambies la semilla, como le puse muchas permutaciones, el algoritmo siempre converge independiente de la semilla
mdl <- ropls::opls(dataset_semen_outliers[, -factor_cols], dataset_semen_outliers$GROUP, crossvalI=10,
                   predI = NA, permI = 1000, scaleC = "none")  # El PLS-DA funciona sin outliers

dev.off()


png("visualizaciones/PLS-DA", width = 720, height = 720)
plot(mdl)
dev.off()



############## # Para sacar la curva ROC
library(mixOmics)
# plsda <- splsda(X, Y, ncomp = ncomp, scale = TRUE)

pls_da <- mixOmics::splsda(X = dataset_semen_outliers[,num_cols], dataset_semen_outliers$GROUP, scale = F, ncomp = 3)

perf <- perf(pls_da, validation = "Mfold",dist = "all", folds = 5, progressBar = T, nrepeat = 100, auc = TRUE, cpus = 4)
perf$auc
perf$auc.all$comp1
###############



######################## PRUEBAS sPLS-DA, NO EJECUTAR!!! 
cosa <- mixOmics::splsda(X = dataset_semen_outliers[,num_cols], dataset_semen_outliers$GROUP, scale = F, ncomp = 2)
library(mixOmics)
plotIndiv(cosa)
plotArrow(cosa)
cim(cosa)

plotVar(cosa)
plotLoadings(cosa)
network(cosa)
selectVar(cosa)
tune(cosa)
perf(cosa)
auc(cosa)


X <- dataset_semen_outliers[,num_cols]
Y <- dataset_semen_outliers$GROUP
tune= tune.splsda(X, Y, ncomp=1, nrepeat=100, logratio="none",
           test.keepX = c(5, 10, 15), folds=10, dist="max.dist", progressBar = TRUE, 
           scale = F, auc = T, cpus = 4)
plot(tune)
tune$measure
tune$auc
auc(tune)

#########################################3



# 21. As for PCA, use the goodness-of-fit bar plot to obtain a suggestion for the
# number of PLS components (Fig. 8a). This suggestion will be based on the
# stabilization of the Q2Y measure with increasing component number (increase
# compared with the previous component of 5% or less).


# Mirar AUC con pROC (sobreentrena porque se calcula en el set de entrenamiento,
# no en los pliegues de la CV. Es mas fiable el que calculo mas arriba de las
# pruebas del PLS-DA)
library(pROC)
library(caret)
predicciones <- predict(mdl, dataset_semen_outliers[,num_cols], type = "prob") # Columna 1 es la clase
matriz_conf <- confusionMatrix(predicciones, dataset_semen_outliers[,7], positive = "Caso")
matriz_conf

predicciones <-  ordered(predicciones, levels = c("Control", "Caso"))
# Con el argumento levels, ponemos los controles y los casos (aqui lo tengo bien, yujuuu!)
result_roc <- roc(dataset_semen_outliers$GROUP, predicciones, ci = T, levels = c("Control","Caso")) # generamos la curva ROC. El sentido de la comparación tiene que ser Control - Casos, i.e. Control >= Casos
# result_roc <- roc(controls = dataset_semen_outliers$GROUP[dataset_semen_outliers$GROUP == "Control"], cases = dataset_semen_outliers$GROUP[dataset_semen_outliers$GROUP == "Caso"], ci = T, direction = "auto") # generamos la curva ROC. El sentido de la comparación tiene que ser Control - Casos, i.e. Control >= Casos
AUC_test <- result_roc$auc


png("visualizaciones/PLS-DA_AUC", width = 720, height = 720)
plot(result_roc, print.thres="best", print.thres.best.method="closest.topleft", 
     print.auc = T, print.ci = T, auc.polygon = T)
dev.off()



# Paso 29: Calcula VIPs
imp_metabs <- getVipVn(mdl)
imp_metabs[imp_metabs > 1]
range(imp_metabs)
usar_metabs <- which(imp_metabs > 1)
# Quitamos las covariables
usar_metabs <- usar_metabs[-c(1:4)]
length(usar_metabs) # 83 metabolitos

#### Univariate

# 32. Open the ‘Univariate analysis’ notebook.



# 33.  Hacemos anova y corregiemos por Bejamini-Yekutieli. Nos quedamos con
# metabolitos con fdr < 0.05 Y VIP > 1

# Función para obtener el p-valor de un lm(). Fuente:
# https://gettinggeneticsdone.blogspot.com/2011/01/rstats-function-for-extracting-f-test-p.html
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}


# Iniciamos dataframe vacio
Resultados.anova = data.frame(matrix(ncol = 3, nrow = 0), stringsAsFactors = F)


# Iteramos por índices de las columnas (metabs) del dataset
for (i in usar_metabs) { 
  
  # Nombre del metab
  metabolito <- colnames(dataset_semen_outliers)[i]
  
  # De acuerdo a creo que Edu1, 
  # a esto: https://stackoverflow.com/questions/40823310/when-should-i-use-aov-and-when-anova
  # y a esto: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/ancova.lm.html ,
  # primero se ajustan los datos con un lm() o un aov() y luego se le hace un
  # anova() a eso, rollo lm1 = lm(datos); anova(lm1)
  # 
  # PD: con dataset_semen[,i] es la unica manera de que se lo trague el lm()
  lm_fit <- lm(dataset_semen_outliers[,i] ~ GROUP, data = dataset_semen_outliers)

  # Calculamos p-val de la regresión lineal
  p_val_lm <- lmp(lm_fit)
  
  # Calculamos diferencias con ANOVA y nos quedamos con los p-valores
  anova_metab <- anova(lm_fit)
  p_val_GROUP <- anova_metab[1,5]
  
  resultados_metab <- c(metabolito, p_val_lm, p_val_GROUP)
  Resultados.anova <- rbind(Resultados.anova, resultados_metab)
}

# Ponemos bien los nombres de las columnas y las filas en la tabla de resultados
colnames(Resultados.anova) <- c("Metabolito", "p_val_lm", "p_val_GROUP")
rownames(Resultados.anova) <- Resultados.anova$Metabolito 
Resultados.anova <- Resultados.anova[,-1]
View(Resultados.anova)

# Casteamos a floating comma (creo que al cargar los datos desde un excel, se
# desconfigura el tema del simbolo del decimal)
Resultados.anova$p_val_lm <- as.numeric(Resultados.anova$p_val_lm)
Resultados.anova$p_val_GROUP <- as.numeric(Resultados.anova$p_val_GROUP)


# Guardamos los p-valores sin corregir por BY
# data.table::fwrite(Resultados.anova, "Resultados_ANOVA_raw.csv", sep = ",", 
#                    nThread = 2, row.names = T)
xlsx::write.xlsx(Resultados.anova, "Resultados_ANOVA_raw.xls") 


# Corregimos por BY
Resultados.corregidos <- Resultados.anova

colnames(Resultados.corregidos) <- sub("p_val_", "fdr_", colnames(Resultados.corregidos))
Resultados.corregidos <- apply(Resultados.corregidos, 2, function(x) p.adjust(x, method = "BY"))

class(Resultados.corregidos)
Resultados.corregidos <- data.frame(Resultados.corregidos)
View(Resultados.corregidos)


# Guardamos los p-valores corregidos por BY
# data.table::fwrite(Resultados.corregidos, "Resultados_ANOVA_BY.csv", sep = ",",
#                    nThread = 2, row.names = T) # tiene pinta de que fwrite funciona con dat.frames, pero no con matrices 
xlsx::write.xlsx(Resultados.corregidos, "Resultados_ANOVA_BY.xls") 


# Sacamos los boxplots de los metabs con anovas significativos y mostramos en
# boxplots sus diferencias por EDAD (el anova encuentra diferencias
# significativas en esos dos factores). Nota: metabs significativos TRAS
# corregir por BY
metabs_significativos <- which(colnames(dataset_semen_outliers) %in% rownames(Resultados.corregidos[Resultados.corregidos$fdr_lm <= 0.05,]))
colnames(dataset_semen_outliers)[metabs_significativos]
Resultados.corregidos[metabs_significativos-7,]
# Recuerda que la tabla de metabolitos no incluye las 7 primeras columnas, que
# son las covariables de los pacientes, por lo tanto tienen un desajuste de 7
# números de índidce

par(mar = c(2, 3, 2, 3))


edades <- dataset_semen_outliers$AGE
edades <- ifelse(edades <= median(dataset_semen_outliers$AGE), "18-30 yo", "31-48yo")
edades <- as.factor(edades)

# Creamos carpeta de los boxplots si no la tenemos
if (!dir.exists("boxplots_metabs_BY_significativos")) {
  dir.create("boxplots_metabs_BY_significativos")
}


for (i in metabs_significativos) {
  
  # Boxplot de diferencias por grupo
  png(filename = file.path("boxplots_metabs_BY_significativos",paste0(colnames(dataset_semen_outliers)[i],"_boxplot_GROUP.png")), width = 658,
      height = 613)
  
  boxplot(dataset_semen_outliers[,i] ~ dataset_semen_outliers[,7], las = 1, ylab = "", xlab = "",
          main = colnames(dataset_semen_outliers)[i], col = c("cadetblue1", "brown2"))
  
  # Texto del p-val de la bondad del ajuste del modelo
  texto <-  paste0("p-val (Tukey) = ", format(as.numeric(Resultados.corregidos[i-7,1]), digits = 3))
  mtext(text = texto, side = 3, line = -1, outer = F, at = 1.5)
  
  # Texto del p-val de diferencias entre grupos
  texto2 <-  paste0("p-val (Group) = ", format(as.numeric(Resultados.corregidos[i-7,2]), digits = 3))
  mtext(text = texto2, side = 3, line = -2.5, outer = F, at = 1.5)
  
  dev.off()
  
  
  
  
  # Agrupamos por EDAD (diferencia significativa tras corregir por BY)
  png(filename = file.path("boxplots_metabs_BY_significativos",paste0(colnames(dataset_semen_outliers)[i],"_boxplot_AGE.png")), width = 658,
      height = 613)
  
  boxplot(dataset_semen_outliers[,i] ~ edades, las = 1, ylab = "", xlab = "",
          main = colnames(dataset_semen_outliers)[i], col = c("brown1", "cadetblue1"))
  
  # Texto del p-val de la bondad del ajuste del modelo
  texto <-  paste0("p-val (Tukey) = ", format(as.numeric(Resultados.corregidos[i-7,1]), digits = 3))
  mtext(text = texto, side = 3, line = -1, outer = F, at = 1.5)
  
  # Texto del p-val de diferencias entre edades
  texto2 <-  paste0("p-val (Age) = ", format(as.numeric(Resultados.corregidos[i-7,3]), digits = 3))
  mtext(text = texto2, side = 3, line = -2.5, outer = F, at = 1.5)
  
  dev.off()
}