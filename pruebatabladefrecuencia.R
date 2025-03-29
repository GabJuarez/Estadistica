numeros <- c(1387, 1754, 1817, 1040, 1273, 1529, 3082, 1951, 2692, 1206, 1342, 443, 754, 1621, 870, 1174, 1412, 1809, 2415, 1546,
             2148, 2207, 2282, 1428, 1889, 1166, 1320, 2265, 1323, 1760, 1919, 2387, 2866, 732, 1464, 1626, 1762, 1915, 2119, 1766,
             2201, 996, 2813, 323, 352, 482, 1144, 1485, 1509, 1638, 1961, 2127, 2430, 1704, 1876, 2010, 2165, 2281, 2389, 335,
             968, 1298, 1410, 1553, 1648, 2071, 2116, 1500, 1549, 2348, 2498, 294, 1115, 1124, 1532, 1688, 1822, 1897, 2445, 2886,
             820, 1266, 1741, 1772, 1932, 2380, 2422, 2446, 369, 978, 1238, 1818, 1824, 1907, 1938, 1940, 2197, 2646, 1461, 1731,
             2230, 2341, 3292, 1108, 1295, 1344, 1906, 1952, 2070, 2454, 1606, 1680, 1827, 1915, 2084, 2639, 842, 1963, 2089, 2338,
             3043, 1059, 1674, 1807, 2086, 2236, 2928, 1269, 4717, 1797, 1955, 2199, 2482, 2701, 3210, 377, 1220, 1401, 2175, 1118,
             2584, 2666, 2991, 934, 2063, 2083, 2856, 2989, 910, 1536, 1957, 2240, 2695, 1325, 2280, 2279, 2626, 1501, 1752, 2088,
             2370, 2637, 1426, 2944, 2147, 1973, 2502, 783, 1538, 2339, 2700, 2222, 2597, 2742, 1837, 2842, 2434, 1640, 1821, 2487)

k <- ceiling(1 + log2(length(numeros)))

maximo <- max(numeros)
minimo <- min(numeros)

# Tamaño de los intervalos
amplitud <- (maximo - minimo) / k

#redondear a la centena más cercana
roundToNearestHundred <- function(x) {
  round(x / 100) * 100
}

amplitud <- roundToNearestHundred(amplitud)

# Creacion de intervalos y distribución de los datos en estos
intervalos <- cut(numeros, breaks = seq(200, maximo + 500, length.out = 10), right = FALSE)

tablaIntervalos <- table(intervalos)
totalDatos <- length(numeros)  # Total de datos

# inicializando listas para almacenar las frecuencias
frecuenciaAcumulada <- numeric(length(tablaIntervalos))
frecuenciaRelativa <- numeric(length(tablaIntervalos))

acumulado <- 0
for (i in 1:length(tablaIntervalos)) {
  frecuenciaRelativa[i] <- tablaIntervalos[i] / totalDatos
  acumulado <- acumulado + tablaIntervalos[i]
  frecuenciaAcumulada[i] <- acumulado
}

# creando un data frame
df <- data.frame(
  Intervalo = names(tablaIntervalos),
  Frecuencia = as.numeric(tablaIntervalos),
  FrecuenciaRelativa = frecuenciaRelativa,
  FrecuenciaAcumulada = frecuenciaAcumulada
)

print(df)

#HISTOGRAMA WWOOOW
hist(numeros, breaks = seq(200, maximo + 500, length.out = 10),
     col = "lightblue", border = "black",
     main = "Histograma de la Distribución",
     xlab = "Intervalos", ylab = "Frecuencia",
     xaxt = "n")  # Ocultar etiquetas automáticas del eje X

# puntos medios de los intervalos
intervalos <- seq(200, maximo + 500, length.out = 10)  #usando los mismos breaks de hist
puntos_medios <- intervalos[-length(intervalos)] + diff(intervalos) / 2  

# etiquetas
etiquetas <- paste0("[", head(intervalos, -1), ", ", tail(intervalos, -1), ")")
axis(1, at = puntos_medios, labels = etiquetas, las = 2)

