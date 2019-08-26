Elecciones locales Colombia 2019

Nuevo repositorio abierto con los datos disponibles para algunas elecciones locales en Colombia 2019

Si quiere ver cómo va la intención de voto para las 3s principales ciudades:
- Bogotá
- Medellín
- Cali

Si quiere analizar los datos, en las carpetas arriba puede encontrar las intenciones de votos consolidadas.

Si usa R (y debería), importe las encuestas en 3 pasos:

install.packages("RCurl") #Instale RCurl
library(RCurl) #Cargue RCurl
encuestas2018 <- read.csv(text=getURL("https://raw.githubusercontent.com/nelsonamayad/elecciones_localeslecciones-presidenciales-2018/master/Elecciones%202018/encuestas2018.csv"))
