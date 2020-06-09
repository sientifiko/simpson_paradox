
# === importando librerías
library(tidyverse)




# importando datos

data <- read.csv("paradox_data.csv")

# descripción general de los datos
str(data)


# agrupando por etnicidad y promedio de gastos

etnicidad <- data %>% 
  group_by(Ethnicity) %>%
  summarize(n= n(), media = mean(Expenditures))


# ploteando promedio por etnicidad
ggplot(etnicidad, aes(reorder(Ethnicity, -media), media)) +
  theme_light() +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x="", y = "Media de gastos")

# ploteando cantidad por etnicidad
ggplot(etnicidad, aes(reorder(Ethnicity, -n), n)) +
  theme_light() +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x="", y = "Cantidad de personas")


# relación entre edad y gastos
ggplot(data, aes(Age, Expenditures)) +
  theme_light() +
  geom_jitter() +
  geom_smooth(method = "lm") +
  labs(x= "Edad", y = "Gasto")
  




