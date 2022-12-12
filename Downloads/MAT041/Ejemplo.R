setwd("/data/fcp-X79-02/ExamplesR")
datos <- read.table("Radiografias.txt", header = TRUE)

attach(datos)

hist(EDAD)
boxplot(EDAD)
summary(EDAD)
var(EDAD)
sd(EDAD)

sum( ( (EDAD - mean(EDAD))/sd(EDAD) )^3 )
sum( ( (EDAD - mean(EDAD))/sd(EDAD) )^4 ) - 3

hist(EDAD - mean(EDAD))

kurt.simm <- function(x){ 
  simm = sum( ( (x - mean(x))/sd(x) )^3 )
  kurt = sum( ( (x - mean(x))/sd(x) )^4 ) - 3
  return(c(simm, kurt))
}


kurt.simm(R3)

boxplot(R1 ~ SEXO)
boxplot(R1 ~ LATERALIDAD)
boxplot(R1 ~ LATERALIDAD + SEXO)

percentiles = quantile(R1, probs = c(0.25,0.5,0.75))
range(R1)

boxplot(R2 ~ SEXO)
boxplot(R2 ~ LATERALIDAD)
boxplot(R2 ~ LATERALIDAD + SEXO)


plot(table(LATERALIDAD))
