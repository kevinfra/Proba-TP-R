   # Ejercicio 1
  
  estimador_mom_b <- function(n,b,muestra_aleatoria) {
    if (missing(muestra_aleatoria) && !(missing(b) && missing(n))) muestra_aleatoria = runif(n, 0, b)
    return (2*mean(muestra_aleatoria))
  }
  
  estimador_mv_b <- function(n,b,muestra_aleatoria) {
    if (missing(muestra_aleatoria) && !(missing(b) && missing(n))) muestra_aleatoria = runif(n, 0, b)
    return (max(muestra_aleatoria))
  }
  
  # Ejercicio 2
  estimador_med_b <- function(n,b,muestra_aleatoria) {
    if (missing(muestra_aleatoria) && !(missing(b) && missing(n))) muestra_aleatoria = runif(n, 0, b)
    return (2*median(muestra_aleatoria))
  }
  
  # Ejercicio 3
  b = 1; n = 15 ; muestra_aleatoria = runif(n, 0, b)
  muestra_aleatoria = runif(n, 0, b)
  estimador_mom_b_ej3 = estimador_mom_b(n,b,muestra_aleatoria)
  estimador_mv_b_ej3 = estimador_mv_b(n,b,muestra_aleatoria)
  estimador_med_b_ej3 = estimador_med_b(n,b,muestra_aleatoria)
  print(estimador_mom_b_ej3) ; print(estimador_mv_b_ej3) ; print(estimador_med_b_ej3)
  
  error_estimador_mom_b_ej3 = abs(1 - estimador_mom_b_ej3)
  error_estimador_mv_b_ej3 = abs(1 - estimador_mv_b_ej3)
  error_estimador_med_b_ej3 = abs(1 - estimador_med_b_ej3)
  print(error_estimador_mom_b_ej3) ; print(error_estimador_mv_b_ej3) ; print(error_estimador_med_b_ej3)

# Ejercicio 4
# c)
lista_de_estimadores_mom = seq(1,1000,1)
lista_de_estimadores_mv = seq(1,1000,1)
lista_de_estimadores_med = seq(1,1000,1)
b = 1; n = 15
for(nrep in 1:1000) {
  muestra_aleatoria = runif(n, 0, b)
  lista_de_estimadores_mom[nrep] = 2*mean(muestra_aleatoria)
  lista_de_estimadores_mv[nrep] = max(muestra_aleatoria)
  lista_de_estimadores_med[nrep] = 2*median(muestra_aleatoria)
}

# d)
sesgo_mom =  mean(lista_de_estimadores_mom)-b
sesgo_mv = mean(lista_de_estimadores_mv)-b
sesgo_med = mean(lista_de_estimadores_med)-b

# e)
var_mom = var(lista_de_estimadores_mom)
var_mv = var(lista_de_estimadores_mv)
var_med = var(lista_de_estimadores_med)

# f)
ecm_mom = var_mom + sesgo_mom**2
ecm_mv = var_mv + sesgo_mv**2
ecm_med = var_med + sesgo_med**2


# Ejercicio 5

simulacion_mv = function(b, n){
  lista_de_estimadores_mv = seq(1,1000,1)
  for(nrep in 1:1000) {
    muestra_aleatoria = runif(n, 0, b)
    lista_de_estimadores_mv[nrep] = max(muestra_aleatoria)
  }
  sesgo_mv = mean(lista_de_estimadores_mv)-b
  var_mv = var(lista_de_estimadores_mv)
  return(var_mv + sesgo_mv**2)
}
simulacion_mom = function(b, n){
  lista_de_estimadores_mom = seq(1,1000,1)
  for(nrep in 1:1000) {
    muestra_aleatoria = runif(n, 0, b)
    lista_de_estimadores_mom[nrep] = 2*mean(muestra_aleatoria)
  }
  sesgo_mom =  mean(lista_de_estimadores_mom)-b
  var_mom = var(lista_de_estimadores_mom)
  return(var_mom + sesgo_mom**2)
}
simulacion_med = function(b, n){
  lista_de_estimadores_med = seq(1,1000,1)
  for(nrep in 1:1000) {
    muestra_aleatoria = runif(n, 0, b)
    lista_de_estimadores_med[nrep] = 2*median(muestra_aleatoria)
  }
  sesgo_med = mean(lista_de_estimadores_med)-b  
  var_med = var(lista_de_estimadores_med)
  return(var_med + sesgo_med**2)
}

# Ejercicio 6
n = 15; b = seq(0.5, 2, 0.01)
ecms_mom = 1:151
ecms_mv = 1:151
ecms_med = 1:151
step = 1
for(un_b in b) {
  ecms_mom[step] = simulacion_mom(un_b,n)
  ecms_mv[step] = simulacion_mv(un_b,n)
  ecms_med[step] = simulacion_med(un_b,n)
  step = step + 1
}
grafico_med = plot(b,ecms_med, col="blue",ylim=c(0,0.3),  ylab="ECM")
grafico_mom = points(b,ecms_mom, col="green")
grafico_mv = points(b,ecms_mv)



# Ejercicio 7
n = c(15,30,50,100,150,200); b = 1
ecms_mom = 1:6
ecms_mv = 1:6
ecms_med = 1:6
step = 1
for(un_n in n) {
  ecms_mom[step] = simulacion_mom(b,un_n)
  ecms_mv[step] = simulacion_mv(b,un_n)
  ecms_med[step] = simulacion_med(b,un_n)
  step = step + 1
}

grafico_mom = plot(n,ecms_mom, col="green", ylim=c(0,0.1), ylab="ECM")
grafico_mv = points(n,ecms_mv)
grafico_med = points(n,ecms_med, col="blue")

#Ej 8
muestra = c(0.917,0.247,0.384,0.530,0.798,0.912,0.096,0.684, 0.394, 20.1, 0.769, 0.137, 0.352, 0.332, 0.670)
n = 15
estimador_mom_b = 2*mean(muestra)
estimador_mv_b = max(muestra)
estimador_med_b = 2*median(muestra)
print(estimador_mom_b ) ; print(estimador_mv_b) ; print(estimador_med_b)


# Ejercicio 9
lista_de_estimadores_mom = seq(1,1000,1)
lista_de_estimadores_mv = seq(1,1000,1)
lista_de_estimadores_med = seq(1,1000,1)
b = 1; n = 15
for(nrep in 1:1000) {
  muestra_aleatoria = runif(n, 0, b)
  es_atipica = rbinom(1,1,0.05)
  if(es_atipica == 1) {
    muestra_aleatoria[1] = muestra_aleatoria[1] * 100
  }
  lista_de_estimadores_mom[nrep] = 2*mean(muestra_aleatoria)
  lista_de_estimadores_mv[nrep] = max(muestra_aleatoria)
  lista_de_estimadores_med[nrep] = 2*median(muestra_aleatoria)
}
sesgo_mv =mean(lista_de_estimadores_mv)-b  
var_mv = var(lista_de_estimadores_mv)
ecm_mv = var_mv + sesgo_mv**2
print(sesgo_mv) ; print(var_mv) ; print(ecm_mv)

sesgo_mom = mean(lista_de_estimadores_mom)-b 
var_mom = var(lista_de_estimadores_mom)
ecm_mom = var_mom + sesgo_mom**2
print(sesgo_mom) ; print(var_mom) ; print(ecm_mom)

sesgo_med = mean(lista_de_estimadores_med)-b 
var_med = var(lista_de_estimadores_med)
ecm_med = var_med + sesgo_med**2
print(sesgo_med) ; print(var_med) ; print(ecm_med)



