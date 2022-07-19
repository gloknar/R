# Para sacar gráficas de porcentajes de NAs en columnas
# Sacado de https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html



library("naniar")
naniar::riskfactors
vis_miss(airquality)