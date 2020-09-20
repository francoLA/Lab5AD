library("C50")
library("caret")

# Guardamos la URL de la base de datos
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data"

# Leemos directamente sobre los datos y se guarda en un data frame
# El separador de columas es una coma ","
cars <- read.table(url, sep = ",")

# Colocamos los nombres a las columnas
colnames(cars) <- c("buyingPrice", 
                    "maintenanceCost", 
                    "numberOfDoors", 
                    "numberOfPersons",
                    "sizeOfLuggageBoot",
                    "safety",
                    "decision")

# Transformamos todas las variables y la clase a factor
cars$buyingPrice <- as.factor(cars$buyingPrice)
cars$maintenanceCost <- as.factor(cars$maintenanceCost)
cars$numberOfDoors <- as.factor(cars$numberOfDoors)
cars$numberOfPersons <- as.factor(cars$numberOfPersons)
cars$sizeOfLuggageBoot <- as.factor(cars$sizeOfLuggageBoot)
cars$safety <- as.factor(cars$safety)
cars$decision <- as.factor(cars$decision)

#se crea el conjunto de datos para entrenamiento
training.index = createDataPartition(cars$decision, p = 0.7)$Resample1
training.set = cars[training.index, ]

#el resto de datos son de testing
test.set = cars[-training.index, ]

#creacion del arbol
tree = C5.0(decision ~ ., training.set)

# Obtenemos las reglas del arbol
tree.rules = C5.0(x = training.set[, -7], y = training.set$decision, rules = T)

# Las clases predecidas para el conjunto de prueba
tree.pred.class = predict(tree, test.set[,-7], type = "class")

# Las probabilidades para el conjunto de prueba
tree.pred.prob = predict(tree, test.set[,-7], type = "prob")

tree.pred.prob
tree.pred.class

# Matriz de confusion obtenida comparando las clases predichas y las originales
# del conjunto de prueba
conf.matrix.tree = confusionMatrix(table(test.set$decision, tree.pred.class))

# Obtencion de valores importantes
print(conf.matrix.tree)

# Graficando el arbol
plot(tree)

# Resumen del arbol
summary(tree)

# Resumen reglas del arbol
summary(tree.rules)



############## VARIACION #################

# Nuevo corte
training.index.var = createDataPartition(cars$decision, p = 0.8)$Resample1

# Conjunto de entrenamiento
training.set.var = cars[training.index.var, ]

#el resto de datos son de testing
test.set.var = cars[-training.index.var, ]

# Se obtiene el arbol
tree = C5.0(decision ~ safety +
                       buyingPrice +
                       maintenanceCost +
                       sizeOfLuggageBoot +
                       numberOfPersons, training.set.var)


# Se grfica el arbol
plot(tree)

# Se obtiene el resumen
summary(tree)

# Se obtenienen las clases predecidas
tree.pred.class.var = predict(tree, test.set.var[,-7], type = "class")

# Matriz de confusion obtenida comparando las clases predichas y las originales
# del conjunto de prueba
conf.matrix.tree.var = confusionMatrix(table(test.set.var$decision, tree.pred.class.var))

conf.matrix.tree.var

