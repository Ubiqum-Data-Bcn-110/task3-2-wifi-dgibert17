## Buscamos los 2 WAPs que tienen mayor RSSI para cada captura en TRAINING.
```{r}
attributes = c("BESTWAP1", "RSSI_BESTWAP1",
               "BESTWAP2", "RSSI_BESTWAP2",
               "BESTWAP3", "RSSI_BESTWAP3"
)
df.bestWAPs = data.frame(matrix(ncol = 6), stringsAsFactors = TRUE)
colnames(x = df.bestWAPs) <- attributes

df.train[df.train == 100] = -105

for (i in 1:nrow(df.train)){
  vec = sort(df.train[i,1:last(grep(pattern = "WAP", names(df.train)))], decreasing = TRUE)
  df.bestWAPs[i,"BESTWAP1"] = names(vec)[1]
  df.bestWAPs[i,"RSSI_BESTWAP1"] = vec[1]
  df.bestWAPs[i,"BESTWAP2"] = names(vec)[2]
  df.bestWAPs[i,"RSSI_BESTWAP2"] = vec[2]
  df.bestWAPs[i,"BESTWAP3"] = names(vec)[3]
  df.bestWAPs[i,"RSSI_BESTWAP3"] = vec[3]
}

df.bestWAPs = cbind(df.bestWAPs, df.train[,c(which(names(df.train) == "LONGITUDE"):length(names(df.train)))])
df.bestWAPs$BESTWAP1 = as.factor(df.bestWAPs$BESTWAP1)
df.bestWAPs$BESTWAP2 = as.factor(df.bestWAPs$BESTWAP2)
df.bestWAPs$BESTWAP3 = as.factor(df.bestWAPs$BESTWAP3)
```

## Buscamos los 2 WAPs que tienen mayor RSSI para cada captura en VALIDATION
```{r}
attributes = c("BESTWAP1", "RSSI_BESTWAP1",
               "BESTWAP2", "RSSI_BESTWAP2",
               "BESTWAP3", "RSSI_BESTWAP3"
)
df.bestWAPs.val = data.frame(matrix(ncol = 6), stringsAsFactors = TRUE)
colnames(x = df.bestWAPs.val) <- attributes

df.val[df.val == 100] = -105

for (i in 1:nrow(df.val)){
  vec = sort(df.val[i,1:last(grep(pattern = "WAP", names(df.val)))], decreasing = TRUE)
  df.bestWAPs.val[i,"BESTWAP1"] = names(vec)[1]
  df.bestWAPs.val[i,"RSSI_BESTWAP1"] = vec[1]
  df.bestWAPs.val[i,"BESTWAP2"] = names(vec)[2]
  df.bestWAPs.val[i,"RSSI_BESTWAP2"] = vec[2]
  df.bestWAPs.val[i,"BESTWAP3"] = names(vec)[3]
  df.bestWAPs.val[i,"RSSI_BESTWAP3"] = vec[3]
}

df.bestWAPs.val = cbind(df.bestWAPs.val, df.val[,c(which(names(df.train) == "LONGITUDE"):length(names(df.train)))])
df.bestWAPs.val$BESTWAP1 = as.factor(df.bestWAPs.val$BESTWAP1)
df.bestWAPs.val$BESTWAP2 = as.factor(df.bestWAPs.val$BESTWAP2)
df.bestWAPs.val$BESTWAP3 = as.factor(df.bestWAPs.val$BESTWAP3)
```


## Probamos clasificar building y floor y ambas juntas con el nuevo DF de best waps
```{r warning=FALSE}
building.vec = createDataPartition(y = df.bestWAPs$BUILDINGID, p = 0.20)

# Training data
building.tr = df.bestWAPs[building.vec$Resample1, c(1,3,5, 9,10)]
# building.tr = dummy_cols(.data = building.tr, select_columns = "BUILDINGID")
# building.tr = select(building.tr, -"BUILDINGID")
building.tr$FLOOR = as.factor(building.tr$FLOOR)
building.tr$BUILDINGID = as.factor(building.tr$BUILDINGID)

# building.val = dummy_cols(.data = df.bestWAPs.val, select_columns = "BUILDINGID")
# building.val = select(building.val, -"BUILDINGID")
building.val = data.frame(df.bestWAPs.val)
building.val$FLOOR = as.factor(building.val$FLOOR)
building.val$BUILDINGID = as.factor(building.val$BUILDINGID)


# Test data
# VALIDATION DF

# Tabla de proporciones para ver que la proporcion de observaciones por BUILDING es la misma en el DF original que en el building.tr
prop.table(table(df.bestWAPs$BUILDINGID))
prop.table(table(building.tr$BUILDINGID))

ctrl = trainControl(method = "repeatedcv",
                    number = 3,
                    repeats = 10)

knn.Fit.Building = train(FLOOR~.,
                         method = "knn",
                         trControl = ctrl,
                         data = building.tr, allowParallel = TRUE)
knn.Fit.Building

knn.pred.Building = predict(knn.Fit.Building, building.val)
accuracy(actual = building.val$FLOOR, predicted = knn.pred.Building)

#Cuantas observaciones hay predichas por edificio y cuantas hay en realidad en total.
table(knn.pred.Building)
table(building.val$FLOOR)

#Cuantos dispositivos se han clasificado mal por cada edificio.
abs(table(knn.pred.Building) - table(building.val$FLOOR))
```
