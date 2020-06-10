
# === importando librerías
library(tidyverse); library(ppcor)
options(scipen = 999) #desactivar notación científica

# importando datos
data <- read.csv("paradox_data.csv")

# descripción general de los datos
str(data)

# cambiar factores de cohortes de edad
data$Age.Cohort <- factor(data$Age.Cohort, levels(data$Age.Cohort)[c(1, 6, 3, 4, 5, 2)])

# agrupando por etnicidad y promedio de gastos
etnicidad <- data %>% 
  group_by(Ethnicity, Gender) %>%
  summarize(n= n(), media = mean(Expenditures))


# ploteando promedio por etnicidad
ggplot(etnicidad, aes(reorder(Ethnicity, -media), media, fill = Gender)) +
  theme_light() +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = "top") +
  labs(x="", y = "Media de gastos")

# graficando por boxplot
ggplot(data, aes(reorder(Ethnicity, -Expenditures), Expenditures, fill = Gender)) +
  theme_light() +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = "top") +
  labs(x="", y = "Media de gastos")


# ploteando cantidad por etnicidad
ggplot(etnicidad, aes(reorder(Ethnicity, -n), n, fill = Gender)) +
  theme_light() +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") +
  labs(x="", y = "Cantidad de personas")


# filtrar por grupos mayoritarios y replicar
data2 <- data %>% filter(Ethnicity=="Hispanic" | Ethnicity== "White not Hispanic")
# data2$Age.Cohort <- factor(data2$Age.Cohort, levels(data2$Age.Cohort)[c(1, 6, 3, 4, 5, 2)])

# agrupando por etnicidad y promedio de gastos de grupos mayoritarios
etnicidad2 <- data2 %>% 
  group_by(Ethnicity, Gender) %>%
  summarize(n= n(), media = mean(Expenditures))


# ploteando promedio de gasto por etnias mayoritarias
ggplot(etnicidad2, aes(reorder(Ethnicity, -media), media, fill = Gender)) +
  theme_light() +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0), 
        legend.position = "top") +
  labs(x="", y = "Media de gastos")


# graficando por boxplot de etnias mayoritarias
ggplot(data2, aes(reorder(Ethnicity, -Expenditures), Expenditures, fill = Gender)) +
  theme_light() +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = "top") +
  labs(x="", y = "Media de gastos")


# relación entre edad y gastos
ggplot(data2, aes(Age, Expenditures)) +
  theme_light() +
  geom_jitter() +
  geom_smooth(method = "lm") +
  labs(x= "Edad", y = "Gasto")


# relación entre edad, etnicidad y gastos
ggplot(data2, aes(Age.Cohort, Expenditures, colour = Ethnicity)) +
  theme_light() +
  geom_boxplot() +
  # stat_summary(fun = "mean", color="red", geom = "line", group = 1)
  theme(legend.position = "top") +
  labs(x= "Cohorte etario", y = "Gastos")

# grafiquemos lo mismo per con promedios
age.etn <- data2 %>% 
  group_by(Age.Cohort, Ethnicity) %>%
  summarize(n= n(), media = mean(Expenditures))

# aca ponemos la gráfica de la tabla de arriba
ggplot(age.etn, aes(Age.Cohort, media, fill = Ethnicity)) +
  theme_light() +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0), 
        legend.position = "top") +
  labs(x="", y = "Media de gastos")


# test de significancia estadística
# si testeamos, gasto no está normalmente distribuido, así que usaremos correlaciones
# no paramétricas

# creamos columna para correlación, discretizando etnicidad por 0 si es blanco
# 1 si es hispanico
data2$hispanico <- as.numeric(data2$Ethnicity == "Hispanic")

# corremos correlación
cor.test(data2$hispanico, data2$Expenditures, method = "spearman")

# ahora hagamos lo mismo, pero controlemos por edad
pcor.test(data2$hispanico, data2$Expenditures, data2$Age, method = "spearman")




