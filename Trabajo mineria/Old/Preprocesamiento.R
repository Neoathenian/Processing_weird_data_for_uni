root.dir="E:/DeMesa/Master_BigData/Mineria_de_datos/Trabajo/Datasets_Raw/"
df_raw=read.csv(paste0(root.dir,"Bancarrota3years.csv"), row.names=1, na.strings="?")
df_raw$class=as.factor(df_raw$class)


#library(caret)

#Lo primero que haremos será mirar cuantos NAs hay.Hay 9888
sum(is.na(df_raw))

library(Hmisc)
missing_vals_description=describe(df_raw)
plot(missing_vals_description)

library(VIM)
#hepatitis.knn <- kNN(df_raw, variable = c("class"))

#Mirando en la siguiente gráfica, podemos ver que la variable con mayor cantidad
#de valores faltantes es el atributo 37, que de hecho tiene 4736 valores faltantes.
#Sabiendo que el dataset tiene 10503 datos, esto significa que aproximadamente 
#la mitad de sus valores son faltantes.
aggr(df_raw)
dim(df_raw)

#El attributo 37 es (activo circulante - existencias) / pasivo a largo plazo.
#Por tanto la primera idea que viene a la mente es ver si estas variables están 
#en otros atributos, y a poder ser, que esos atributos tengan pocos valores faltantes.
#Así, encontramos el Attributo 57 que tiene tanto activo circulante como existencias
#Y además, no tiene ningún valor nulo. Esto nos lleva a concluir a que estos valores faltantes
#Se deban exclusivamente al pasivo a largo plazo. 
#Aunque a priori esto parezca significar que esto se deba a que ese valor es faltante,
#Tenemos el atributo 59 que es: pasivo a largo plazo / fondos propios

#Vemos rápidamente, que todos los valores faltantes en Attr37 se deben a 
#que el pasivo a largo plazo es 0
Attr37_Nas=df_raw[is.na(df_raw$Attr37),]
sum(Attr37_Nas$Attr59!=0)

#Queremos imputar esos valores por infty, pero eso numéricamente se maneja mal
#Así que vamos a usar 1/Attr37, para lo que vamos a ver si Att37 tiene algún 0
#De hecho vemos que hay 3. Sorprendentemente estos coinciden con 0s del Attr59
#Esto lleva a pensar que estos 3 valores en Attr37 fueron imputados erróneamente por 0
df_raw[df_raw$Attr37==0 &!is.na(df_raw$Attr37),][,c("Attr37","Attr59")]


#Dada esta suposición, lo que haremos será invertir Attr37, y los valores NA imputar por 0
#Los 0s en Attr37 también los pasaremos a 0 como hemos comentado
df_raw[df_raw$Attr37==0 &!is.na(df_raw$Attr37),]$Attr37=NA
df_raw$Attr37=1/df_raw$Attr37
df_raw$Attr37=ifelse(is.na(df_raw$Attr37), 0, df_raw$Attr37)

#Los siguientes atributos muy grandes son:21,27,45,60. Estos son:
# Attr21 ventas (n) / ventas (n-1)
# Attr27 resultado de explotación / gastos financieros
# Attr45 beneficio neto / existencias
# Attr60 ventas / existencias
colSums(is.na(df_raw))[colSums(is.na(df_raw))>500]

#En el 21 parece claro que los NAs se deberán a que no hubo ventas anteriormente
#Haremos lo mismo que con el 37, lo invertiremos
#Nótese que en este caso hay 9 0s. Asumremos que si en n no hubo ventas
#Entonces en n-1 tampoco la habría, así que tras invertirla, seguirán siendo 0
#(Tomaremos 0/0->0)
sum(df_raw$Attr21==0,na.rm=T)
df_raw[df_raw$Attr21==0 &!is.na(df_raw$Attr21),]$Attr21=NA
df_raw$Attr21=1/df_raw$Attr21
df_raw$Attr21=ifelse(is.na(df_raw$Attr21), 0, df_raw$Attr21)

#En el atributo 27 parece ser que va a ser a que gastos financieros=0
#De hecho, este atributo tiene 722 que son 0s, por lo que no tiene sentido invertirlo
#En este caso sustituiremos simplemente por infinity
sum(df_raw$Attr27==0,na.rm=T)
df_raw$Attr27=ifelse(is.na(df_raw$Attr27), Inf, df_raw$Attr27)


#Podemos ver que todos los faltantes en el attributo 45 lo son en el 60.
#Es lógico por tanto pensar que esto se debe a que existencias=0
table(is.na(df_raw$Attr45),is.na(df_raw$Attr60))
#Por tanto, veamos si estos NAs coinciden con que
#la variable Attr20 (existencias * 365) / ventas, sea 0
Attr45_Nas=df_raw[is.na(df_raw$Attr45),]
#Vemos que para los que son en el attributo 20 no son NA, se verifica nuestra hipótesis
sum(Attr45_Nas$Attr20!=0,na.rm=T)

#Por tanto, lo que haremos será imputar estos valores por ..... (de momento 0 pero
#HABRA QUE CAMBIARLO)
df_raw$Attr45 <- ifelse(is.na(df_raw$Attr45) & (df_raw$Attr20==0), 0, df_raw$Attr45)
df_raw$Attr60 <- ifelse(is.na(df_raw$Attr60) & (df_raw$Attr20==0), 0, df_raw$Attr60)

#Veamos ahora los que tienen mas de 100 NAs. Vemos que 228 aparece repetido varias veces
#Por tanto no es sorprendente que todas tengan como denominador lo mismo (inmovilizado)
#Attr24 beneficio bruto (en 3 años) / activo total
#Attr28 capital circulante / inmovilizado
#Attr32 (pasivo corriente * 365) / coste de los productos vendidos
#Attr41 pasivo total / ((resultado de explotación + amortizaciones) * (12/365))
#Attr53 fondos propios / inmovilizado
#Attr54 capital constante / inmovilizado
#Attr64 ventas / inmovilizado

#Vemos que todas faltan a la vez (como habíamos hipotetizado)
sum(is.na(df_raw$Attr28)&is.na(df_raw$Attr53)&is.na(df_raw$Attr54)&is.na(df_raw$Attr64))
#Como inmovilizado no aparece en ninguna otra variable-> asumiremos
#que nuestra hipótesis es correcta
df_raw$Attr28=ifelse(is.na(df_raw$Attr28), Inf, df_raw$Attr28)
df_raw$Attr53=ifelse(is.na(df_raw$Attr53), Inf, df_raw$Attr53)
df_raw$Attr54=ifelse(is.na(df_raw$Attr54), Inf, df_raw$Attr54)
df_raw$Attr64=ifelse(is.na(df_raw$Attr64), Inf, df_raw$Attr64)

#Para el atributo 41 no tenemos ningún otro ejemplo con el mismo denominador
#Y mirando el heatmap de datos faltantes para las filas en las que falta 
#tampoco vemos un patrón claro
aggr(df_raw[(is.na(df_raw$Attr41))>0,])

library(VIM)
kNNimputation2441 <- VIM::kNN(df_raw, variable = c("Attr24","Attr41"))
#aggr(hepatitis.knn, delimiter = "_imp", numbers=TRUE, prop = FALSE,  cex.axis = 0.7)
df_raw$Attr24=kNNimputation2441$Attr24
df_raw$Attr41=kNNimputation2441$Attr41

#El atributo 1 no tiene NAs y también tiene activo total como denominador
#Nótese que Attr1 tiene valores no nulos para esas filas
#Por tanto de momento el attr24 no tiene una causa clara
df_raw[is.na(df_raw$Attr24),]$Attr1
#Mirando el heatmap de valores nulos para las filas con valores nulos en el Attr24
#No vemos ningún patrón que nos de una pista de su causa
aggr(df_raw[(is.na(df_raw$Attr24))>0,])


#El atributo 32 tiene el mismo denominador que el Attr47 y el Attr52
#Podemos ver que el attr47 y 52 tienen 85 NAs en común que coinciden con 
#NAs del attr32. Por tanto, estos 85 los asumiremos que son debido a división
#por 0. Quedarán por tanto 16 ya averiguaremos que hacer con ello mas tarde
table(is.na(df_raw$Attr47),is.na(df_raw$Attr52))
table(is.na(df_raw$Attr47)&is.na(df_raw$Attr52),is.na(df_raw$Attr32))

#Los hacemos inf:
Nas_positions=is.na(df_raw$Attr47)&is.na(df_raw$Attr52)
df_raw$Attr32=ifelse(Nas_positions, Inf, df_raw$Attr32)
df_raw$Attr47=ifelse(Nas_positions, Inf, df_raw$Attr47)
df_raw$Attr52=ifelse(Nas_positions, Inf, df_raw$Attr52)




#Attr13 (beneficio bruto + amortizaciones) / ventas
#Attr19 beneficio bruto / ventas
#Attr20 (existencias * 365) / ventas
#Attr23 beneficio neto / ventas
#Attr30 (pasivo total - tesorería) / ventas
#Attr31 (beneficio bruto + intereses) / ventas
#Attr39 beneficio sobre ventas / ventas
#Attr42 resultado de explotación / ventas
#Attr43 rotación de deudores + rotación de existencias en días
#Attr44 (créditos * 365) / ventas
#Attr49 EBITDA (resultado de las actividades de explotación - amortizaciones) / ventas
#####Attr52 (pasivo a corto plazo * 365) / coste de los productos vendidos)
#Attr56 (ventas - coste de los productos vendidos) / ventas
#Attr62 (pasivo a corto plazo *365) / ventas

colSums(is.na(df_raw))[colSums(is.na(df_raw))>40]
cols=c("Attr13","Attr19","Attr20","Attr23","Attr30","Attr31","Attr39",
  "Attr42","Attr43","Attr44","Attr49","Attr56","Attr62")
isna_col=is.na(df_raw[,cols[1]])

for(col in cols)
{
  isna_col=isna_col&is.na(df_raw[,col])
}
#Vemos que de hecho, todos tienen los NAs en el mismo sitio-> división por 0(ventas=0)
sum(isna_col)
#Por tanto los sustituiremos por Inf
for(col in cols)
{
  df_raw[,col]=ifelse(is.na(df_raw[,col]), Inf, df_raw[,col])
}

#Nos quedan 107 filas con NAs. Vemos en el heatmap que hay muchas filas al que les
#parece faltar los atributos: 4,8,12,(15),16,17,26,33,34,40,(45),46,50,(58),(60),63
#donde hemos puesto entre paréntesis los que no parecen faltar tanto
#En este caso, las columnas son (las sin parentesis)
#Attr4 activo circulante / pasivo a corto plazo
#Attr8 valor contable de los fondos propios / pasivo total
#Attr12 beneficio bruto / pasivo a corto plazo
#Attr16 (beneficio bruto + amortización) / total pasivo
#Attr17 total activo / total pasivo
#Attr26 (beneficio neto + amortizaciones) / total pasivo
#Attr33 gastos de explotación / pasivo a corto plazo
#Attr34 gastos de explotación / total pasivo
#Attr40 (activo circulante - existencias - deudores) / pasivo a corto plazo
#Attr46 (activo circulante - existencias) / pasivo a corto plazo
#Attr50 activo circulante / pasivo total
#Attr63 ventas / pasivo a corto plazo
#Podemos ver que todas tienen como denominador pasivo en algún sentido


cols=paste0("Attr",c(4,8,12,16,17,26,33,34,40,46,50,63))
colSums(is.na(df_raw[,cols]))
#Vemos que el 8,16,17,26,34,50 tienen 14 NAs, mientras que el resto 18
#Esto es porque pasivo a corto plazo<pasivo total
isna_col=is.na(df_raw[,cols[1]])

cols1=paste0("Attr",c(4,12,33,40,46,63))
cols2=paste0("Attr",c(8,16,17,26,34,50))
isna_col=is.na(df_raw[,cols1[1]])
for(col in cols1)
{
  isna_col=isna_col&is.na(df_raw[,col])
}
#Por tanto, podemos ver que estas columnas tienen las mismas 18 filas con valores faltantes
#lo que nos lleva a concluir que se debe a la división por 0
sum(isna_col)
isna_col=is.na(df_raw[,cols2[1]])
for(col in cols2)
{
  isna_col=isna_col&is.na(df_raw[,col])
}
#Vemos que estas columnas tienen 14 filas que son todas NA
sum(isna_col)

for(col in cols)
{
  df_raw[,col]=ifelse(is.na(df_raw[,col]), Inf, df_raw[,col])
}
#Por tanto, podemos ver que estas columnas tienen las mismas 18 filas con valores faltantes
#lo que nos lleva a concluir que se debe a la división por 0
sum(isna_col)

for(col in c(cols1,cols2))
{
  df_raw[,col]=ifelse(is.na(df_raw[,col]), Inf, df_raw[,col])
}

#Nos quedan 100 filas con NAs, y no parecen tener un patrón determinado
#Además, la fila a la que mas atributos le faltan, le faltan tan solo 4 variables
#Por tanto, para estas filas haremos imputación por kNN.
sum(!complete.cases(df_raw))
aggr(df_raw[rowSums(is.na(df_raw))>0,])

z=VIM::kNN(df_raw)
plot(z,delimiter="_imp")
aggr(z, delimiter = "_imp", numbers=TRUE, prop = FALSE,  cex.axis = 0.7)

dev.off()
aggr(z,delimiter = "_imp", numbers=TRUE, prop = FALSE,  cex.axis = 0.7)


df_raw=z[,colnames(df_raw)]


#####################Hemos terminado la eliminación de NANs################
#Vamos ahora a ver si hay variables muuuuuuuy correladas y eliminarlas

df2=data.frame(df_raw)
for (i in 1:(ncol(df2)-1))
{
  df2[,i] <- replace(df2[,i], is.infinite(df2[,i]), max(df2[!is.infinite(df2[,i]),i]))
}

#Hagamos un corrplot. Para ello, obtengamos la matriz de correlación
#y luego eliminemos todas las filas que no tengan una mínima correlación
#con alguna otra variable. En este caso, pediremos que tenga en 
#valor absoluto al menos un 0.75 de correlación.
library(corrplot)
library(correlation)

corr_matrix=cor(subset(df2,select = -class))
indexes=unique(which(corr_matrix >= 0.75 & row(corr_matrix) != col(corr_matrix), arr.ind = TRUE)[,c("row")])
corrplot(cor_sort(corr_matrix[indexes,indexes]))
corrplot(cor(subset(df2,select = -class)), method = "color")



replace(df2[,64], is.infinite(df2[,64]), max(df2[,64],na.rm = TRUE))
sum(is.infinite(df2[,64]))

sum(is.infinite(df2[,63]))
df[,64]


sum(is.na(df_raw))
aggr(df_raw)

sum(is.na(df_raw))
sum(is.na(z))



sum(colMeans(df_raw[,-ncol(df_raw)]))

str(df_raw[,-ncol(df_raw)])
