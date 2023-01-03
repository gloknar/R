# FUENTE: https://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/sql.html

# Paquetes de R para SQL:
# DBI
# RODBC
# dbConnect
# RSQLite
# RMySQL
# RPostgreSQL



# Paquete sqldf
library(sqldf)

# Ayuda del paquete
help(sqldf)
# vignette(package = "sqldf")  # Este paquete no tiene viñetas


# El paquete sqldf tiene muy pocos comandos, y el más usado es para hacer
# consultas en SQL:
sqldf()
?sqldf

sqldf(x, stringsAsFactors = FALSE,
      row.names = FALSE, envir = parent.frame(), 
      method = getOption("sqldf.method"),
      file.format = list(), dbname, drv = getOption("sqldf.driver"), 
      user, password = "", host = "localhost", port,
      dll = getOption("sqldf.dll"), connection = getOption("sqldf.connection"),
      verbose = isTRUE(getOption("sqldf.verbose")))  



# Por ejemplo, devuelve las columnas "age" y "circumference", localizadas en la
# tabla "Orange", y de esas columnas me devuelves las filas que tengan un valor
# de la variable Tree = 1 y me ordenas dichas filas (por defecto es ascendente)
# por su valor en la columna "circumference"
sqldf("SELECT age, circumference FROM Orange WHERE Tree = 1 ORDER BY circumference")



# Igual que la anterior, pero las tablas me las ordenas en orden descencente y
# me devuelves las 5 primeras filas
sqldf("SELECT age, circumference FROM Orange WHERE Tree = 1 ORDER BY circumference DESC LIMIT 5")



# Con esta query devuelve toda la tabla "iris". ¡Ojo, porque en SQL los nombres
# de las tablas son case sensitive! Para SQL, "iris" e "IRIS" son tablas
# distintas
sqldf("SELECT * FROM iris")
sqldf("SELECT * FROM IRIS")


# Curiosamente, puedes usar las palabras clave SQL en mayúsculas o minúsculas,
# aunque creo que es una buena práctica ponerlas en mayúsculas
sqldf("select * from iris")
sqldf("SELECT * FROM iris")



# Un ejemplo: cargo el dataframe BOD
data(BOD)
BOD
class(BOD)


sqldf("SELECT Time, demand from BOD ORDER BY demand ASC")

sqldf("SELECT Time, demand from BOD ORDER BY demand DESC")



sqldf("SELECT * FROM iris")
sqldf("JOIN * FROM iris")



sqldf('SELECT * FROM rock WHERE (peri > 5000 AND shape < .05) OR perm > 1000')
 

# Y para hacer joins, usa el siguiente comando de R:
merge(iris, BOD)



#### ORDER BY

# Para ordenar filas, se usa la sintaxis 
# ORDER BY var1 {ASC/DESC}, var2 {ASC/DESC}

# where the choice of ASC for ascending or DESC for descending is made per variable.

sqldf("SELECT * FROM Orange ORDER BY age ASC, circumference DESC LIMIT 5")

##   Tree age circumference
## 1    2 118            33
## 2    4 118            32
## 3    1 118            30
## 4    3 118            30
## 5    5 118            30

WHERE

Conditional statements can be added via WHERE:
  
  sqldf('SELECT demand FROM BOD WHERE Time < 3')

##   demand
## 1    8.3
## 2   10.3

Both AND and OR are valid, along with paranthese to affect order of operations.

sqldf('SELECT * FROM rock WHERE (peri > 5000 AND shape < .05) OR perm > 1000')

##   area     peri    shape perm
## 1 5048  941.543 0.328641 1300
## 2 1016  308.642 0.230081 1300
## 3 5605 1145.690 0.464125 1300
## 4 8793 2280.490 0.420477 1300

There are few more complicated ways to use WHERE:
  IN

WHERE IN is used similar to R’s %in%. It also supports NOT.

sqldf('SELECT * FROM BOD WHERE Time IN (1,7)')

##   Time demand
## 1    1    8.3
## 2    7   19.8

sqldf('SELECT * FROM BOD WHERE Time NOT IN (1,7)')

##   Time demand
## 1    2   10.3
## 2    3   19.0
## 3    4   16.0
## 4    5   15.6

LIKE

LIKE can be thought of as a weak regular expression command. It only allows the single wildcard % which matches any number of characters. For example, to extract the data where the feed ends with “bean”:
  
  sqldf('SELECT * FROM chickwts WHERE feed LIKE "%bean" LIMIT 5')

##   weight      feed
## 1    179 horsebean
## 2    160 horsebean
## 3    136 horsebean
## 4    227 horsebean
## 5    217 horsebean

sqldf('SELECT * FROM chickwts WHERE feed NOT LIKE "%bean" LIMIT 5')

##   weight    feed
## 1    309 linseed
## 2    229 linseed
## 3    181 linseed
## 4    141 linseed
## 5    260 linseed

Aggregated data

Select statements can create aggregated data using AVG, MEDIAN, MAX, MIN, and SUM as functions in the list of variables to select. The GROUP BY statement can be added to aggregate by groups. AS can name the

sqldf("SELECT AVG(circumference) FROM Orange")

##   AVG(circumference)
## 1           115.8571

sqldf("SELECT tree, AVG(circumference) AS meancirc FROM Orange GROUP BY tree")

##   Tree  meancirc
## 1    1  99.57143
## 2    2 135.28571
## 3    3  94.00000
## 4    4 139.28571
## 5    5 111.14286

Counting data

SELECT COUNT() returns the number of observations. Passing * or nothing returns total rows, passing a variable name returns the number of non-NA entries. AS works as well.

d <- data.frame(a = c(1,1,1), b = c(1,NA,NA))
d

##   a  b
## 1 1  1
## 2 1 NA
## 3 1 NA

sqldf("SELECT COUNT() as numrows FROM d")

##   numrows
## 1       3

sqldf("SELECT COUNT(b) FROM d")

##   COUNT(b)
## 1        1

