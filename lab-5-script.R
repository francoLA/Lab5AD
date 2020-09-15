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
#cars$decision = factor(cars$decision, labels = c("unacc", "acc", "good", "vgood"))
###################################################################

#se crea el conjunto de datos para entrenamiento
training.index = createDataPartition(cars$decision, p = 0.7)$Resample1
training.set = cars[training.index, ]

#el resto de datos son de testing
test.set = cars[-training.index, ]

#creacion del arbol
tree = C5.0(decision ~ ., training.set)
tree.rules = C5.0(x = training.set[, -8], y = training.set$decision, rules = T)
tree.pred.class = predict(tree, test.set[,-8], type = "class")
tree.pred.prob = predict(tree, test.set[,-8], type = "prob")


conf.matrix.tree = confusionMatrix(table(test.set$decision, tree.pred.class))
print(conf.matrix.tree)