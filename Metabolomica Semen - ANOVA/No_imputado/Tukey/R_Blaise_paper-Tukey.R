# Fuente: Blaise paper  --- https://www.nature.com/articles/s41596-021-00579-1#code-availability

# Los primeros 10 pasos ya están hechos por Metabolon, entonces empezamos con el
# paso 11, PCA. Para ilustrar, mostramos los primeros 10 pasos

#### Exploratory data analysis with PCA

# 11. Open the notebook called ‘Multivariate Analysis - PCA’.


# 12. Importamos los datos preprocesados y normalizados. Los datos que usamos
# son datos de peak intensity de metabolitos (sacados de HPLC-MS), normalizados
# por masa de eyaculado y transformados por ln(), luego les añadimos la edad y
# el BMI y los normalizamos todos (metabs + edad y BMI) con center + scale de
# caret, que es casi identico al escalado por pareto. Finalmente eliminamos con
# caret::nzv() los metabolitos con varianza igual o cercana a 0

if (!dir.exists("visualizaciones")) {
  dir.create("visualizaciones")
}


library(readxl)
dataset_semen <- readxl::read_excel("../MassNormDataR.xlsx", na = "Missing")
class(dataset_semen)
dataset_semen <- as.data.frame(dataset_semen)
dataset_semen[1:5,1:5]
str(dataset_semen[,1:10])

nombres_pacientes <- dataset_semen$Code
rownames(dataset_semen) <-nombres_pacientes
dataset_semen$CENTRE <- as.factor(dataset_semen$CENTRE)
dataset_semen$DIAGNOSIS <- make.names(dataset_semen$DIAGNOSIS)
dataset_semen$DIAGNOSIS <- as.factor(dataset_semen$DIAGNOSIS)
dataset_semen$GROUP <- as.factor(dataset_semen$GROUP)
dataset_semen <- dataset_semen[,-1] # Eliminamos la columna de los nombres de los pacientes


# Eliminamos metabolitos con varianza = 0
metricas_nzv <- caret::nzv(dataset_semen,saveMetrics = T)
eliminar <- which(metricas_nzv[,"zeroVar"] == T)
# dataset_semen <- dataset_semen[,-eliminar]


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
str(dataset_semen[,422])
anyNA(dataset_semen[,422]) # No hay NA Aqui
colnames(dataset_semen)[422]
dataset_semen[,422]

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




# Comprobamos si hay diferencias en las covariables de los grupos (El
# estadístico t y el p-valor son iguales tanto antes como despues del escalado
# por Pareto)
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
                     select = c("CONCENTRATION_MILLIONS_ML", "PERCENT_PROGRESSIVE_MOVILITY"), 
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
                select.var = list(name = c("CONCENTRATION_MILLIONS_ML", "PERCENT_PROGRESSIVE_MOVILITY"))) + ggtitle("PCA Biplot (grupo)") 
dev.off()


png("visualizaciones/biplot_group", width = 720, height = 720)
fviz_pca_biplot(pca_semen,  geom = c("point"), habillage = factor_cols[2], col.var = "black",
                select.var = list(name = c("CONCENTRATION_MILLIONS_ML", "PERCENT_PROGRESSIVE_MOVILITY"))) + ggtitle("PCA Biplot (grupo)") 
dev.off()



# Cogemos las 21 primeras componentes para los análisis downstream porque
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
pca_semen2 <- pcaMethods::pca(dataset_semen, method = "svd", nPcs = 21, 
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
dataset_semen_outliers$GROUP <- relevel(dataset_semen_outliers$GROUP, ref = "Control")




#### Supervised analysis with PLS-DA

# 19. Open the notebook with the title ‘Multivariate Analysis – PLS-DA’.

# 20. Start by selecting the type of scaling, and run a PLS-DA model to see
# whether your samples can be discriminated with a supervised approach.
library(ropls)
set.seed(1) # Aunque cambies la semilla, como le puse muchas permutaciones, el algoritmo siempre converge independiente de la semilla
mdl <- ropls::opls(dataset_semen_outliers[, -factor_cols], dataset_semen_outliers$GROUP, crossvalI=10,
                   predI = NA, permI = 1000, scaleC = "none")  # El PLS-DA funciona mejor sin outliers

dev.off()


png("visualizaciones/PLS-DA", width = 720, height = 720)
plot(mdl)
dev.off()

# 21. As for PCA, use the goodness-of-fit bar plot to obtain a suggestion for the
# number of PLS components (Fig. 8a). This suggestion will be based on the
# stabilization of the Q2Y measure with increasing component number (increase
# compared with the previous component of 5% or less).


############## # Para sacar la curva ROC
library(mixOmics)
# plsda <- splsda(X, Y, ncomp = ncomp, scale = TRUE)

pls_da <- mixOmics::splsda(X = dataset_semen_outliers[,num_cols], dataset_semen_outliers$GROUP, scale = F, ncomp = 3)

perf <- perf(pls_da, validation = "Mfold", dist = "all", folds = 5, progressBar = T, nrepeat = 100, auc = TRUE, cpus = 4)
perf$auc
# perf$auc.all$comp1
###############


?slice
# Mirar AUC con pROC (Es una estimación optimista porque se evalúa en el set de entrenamiento)
# library(pROC)
# library(caret)
# predicciones <- predict(mdl, dataset_semen_outliers[,num_cols], type = "prob") # Columna 1 es la clase
# matriz_conf <- confusionMatrix(predicciones, dataset_semen_outliers[,7], positive = "Caso")
# matriz_conf
# 
# predicciones <-  ordered(predicciones, levels = c("Control", "Caso"))
# # Con el argumento levels, ponemos los controles y los casos (aqui lo tengo bien, yujuuu!)
# result_roc <- roc(dataset_semen_outliers$GROUP, predicciones, ci = T, levels = c("Control","Caso")) # generamos la curva ROC. El sentido de la comparación tiene que ser Control - Casos, i.e. Control >= Casos
# # result_roc <- roc(controls = dataset_semen_outliers$GROUP[dataset_semen_outliers$GROUP == "Control"], cases = dataset_semen_outliers$GROUP[dataset_semen_outliers$GROUP == "Caso"], ci = T, direction = "auto") # generamos la curva ROC. El sentido de la comparación tiene que ser Control - Casos, i.e. Control >= Casos
# AUC_test <- result_roc$auc
# 
# 
# png("visualizaciones/PLS-DA_AUC", width = 720, height = 720)
# plot(result_roc, print.thres="best", print.thres.best.method="closest.topleft", 
#      print.auc = T, print.ci = T, auc.polygon = T)
# dev.off()


# Paso 29: Calcula VIPs
imp_metabs <- getVipVn(mdl)
imp_metabs[imp_metabs > 1]
range(imp_metabs)
usar_metabs <- which(imp_metabs > 1)
# Quitamos las covariables
usar_metabs <- usar_metabs[-c(1:3)]
length(usar_metabs) # 45 metabolitos

#### Univariate

# 32. Open the ‘Univariate analysis’ notebook.


# 33.  Hacemos anova y corregimos por Tukey (no le veo mucho el sentido si solo
# tenemos 2 grupos en el ANOVA, pero bueno). Nos quedamos con metabolitos con
# Tukey <= 0.05

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
library(multcomp) # Paquete para cargar la función de Tukey

for (i in usar_metabs) { 
  
  # Nombre del metab
  metabolito <- colnames(dataset_semen_outliers)[i]
  
  # De acuerdo a creo que Edu1, 
  # a esto: https://stackoverflow.com/questions/40823310/when-should-i-use-aov-and-when-anova
  # y a esto: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/anova.lm.html ,
  # primero se ajustan los datos con un lm() o un aov() y luego se le hace un
  # anova() a eso, rollo lm1 = lm(datos); anova(lm1)
  # 
  # PD: con Resultados.anova$p_val_lm <= 0.05[,i] es la unica manera de que se lo trague el lm()
  # lm_fit <- lm(Resultados.anova$p_val_lm <= 0.05[,i] ~ GROUP + CENTRE + AGE + BMI + AGE:BMI, data = Resultados.anova$p_val_lm <= 0.05)
  lm_fit <- lm(dataset_semen_outliers[,i] ~ GROUP, data = dataset_semen_outliers)

  # Calculamos p-val de la regresión lineal
  p_val_lm <- lmp(lm_fit)
  
  
  # Siguiendo la sugerencia de Edu1, vamos a usar TukeyHSD en vez de corregir
  # por BY. Para ello, después de calcular el modelo lineal, se usa el comando
  # TukeyHSD()
  # anova_metab <- anova(lm_fit)
  # TukeyHSD(lm_fit, conf.level = .95)
  tuk.lm <- glht(lm_fit, linfct = mcp(GROUP = "Tukey"))
  
  p_val_tukey <- summary(tuk.lm)$test$pvalues[[1]]
  
  
  resultados_metab <- c(metabolito, p_val_lm, p_val_tukey)
  Resultados.anova <- rbind(Resultados.anova, resultados_metab)
}

# Ponemos bien los nombres de las columnas y las filas en la tabla de resultados
colnames(Resultados.anova) <- c("Metabolito", "p_val_lm", "p_val_Tukey")
rownames(Resultados.anova) <- Resultados.anova$Metabolito 
Resultados.anova <- Resultados.anova[,-1]
View(Resultados.anova)


# Casteamos a floating comma (creo que al cargar los datos desde un excel, se
# desconfigura el tema del simbolo del decimal)
Resultados.anova$p_val_lm <- as.numeric(Resultados.anova$p_val_lm)
Resultados.anova$p_val_Tukey <- as.numeric(Resultados.anova$p_val_Tukey)


# Guardamos los p-valores corregidos por Tukey (Tukey controla el FWER)
# data.table::fwrite(Resultados.anova, "Resultados_ANOVA_Tukey.csv", sep = ",", 
#                    nThread = 2, row.names = T)
xlsx::write.xlsx(Resultados.anova, "Resultados_ANOVA_Tukey.xls")



# Sacamos los boxplots de los metabs con un p-valor de Tukey <= 0.05
par(mar = c(2, 3, 2, 2))
# metabs_significativos <- which(colnames(dataset_semen_outliers) %in% rownames(Resultados.anova[(Resultados.anova$p_val_lm <= 0.05) & (Resultados.anova$p_val_Tukey <= 0.05),]))
metabs_significativos <- which(colnames(dataset_semen_outliers) %in% rownames(Resultados.anova[Resultados.anova$p_val_Tukey <= 0.05,]))
colnames(dataset_semen_outliers)[metabs_significativos]
View(Resultados.anova[Resultados.anova$p_val_Tukey <= 0.05,])

# El indice del metab en la tabla de resultados, que luego lo necesitamos para graficar los boxplots
indice_metab_tabla <- which(rownames(Resultados.anova) %in% rownames(Resultados.anova[Resultados.anova$p_val_Tukey <= 0.05,]))

# Creamos carpeta de los boxplots si no la tenemos
if (!dir.exists("boxplots_metabs_Tukey_significativos")) {
  dir.create("boxplots_metabs_Tukey_significativos")
}




# Boxplot de diferencias por grupo
png(filename = file.path("boxplots_metabs_Tukey_significativos",paste0(colnames(dataset_semen_outliers)[metabs_significativos],"_boxplot_GROUP.png")), width = 658,
    height = 613)
  
boxplot(dataset_semen_outliers[,metabs_significativos] ~ dataset_semen_outliers[,7], las = 1, ylab = "", xlab = "",
        main = colnames(dataset_semen_outliers)[metabs_significativos], col = c("cadetblue1", "brown2"))
  
# Texto del p-val de la bondad del ajuste del modelo
texto <-  paste0("p-val (lm) = ", format(Resultados.anova[indice_metab_tabla,1], digits = 3))
mtext(text = texto, side = 3, line = -1, outer = F, at = 1.5)
  
# Texto del p-val de diferencias entre grupos (Tukey)
texto2 <-  paste0("p-val (Tukey) = ", format(Resultados.anova[indice_metab_tabla,2], digits = 3))
mtext(text = texto2, side = 3, line = -2.5, outer = F, at = 1.5)
  
dev.off()
  
  

# Sacamos los whiskerplots de los metabs con un p-valor en Tukey <= 0.05
# Hacemos el Tukey y lo graficamos
tuk.lm <- glht(lm(dataset_semen_outliers[,metabs_significativos] ~ GROUP, data = dataset_semen_outliers), linfct = mcp(GROUP = "Tukey"))
  
  
png(filename = file.path("boxplots_metabs_Tukey_significativos",paste0(colnames(dataset_semen_outliers)[metabs_significativos],"_whiskerplot.png")), width = 658,
    height = 613)
  
# Llamamos a par() después de llamar a png, porque si no png overrides par()
par(mar = c(2, 6.5, 2, 2))
  
plot(tuk.lm, 
     main = paste0("95%-family wise confidence level for ",colnames(dataset_semen_outliers)[metabs_significativos]),
     xlim = c(-1,1))
  
dev.off()


dataset_semen_outliers[,422]
colnames(dataset_semen_outliers)[422]

colnames(dataset_semen)[422]
dataset_semen[,422]