# Estrategias de cobertura para comprador de divisa (USD)

El objetivo de este trabajo es comparar varias estrategias de cobertura para determinar 
la mejor para un comprador de la divisa.

## Estrategias a comparar

1. Estrategia tradicional: largo en call. 

La cobertura cambiaria con esta estrategia se realiza con comprar una opción call ATM. 

2. Estrategia Forward Participativo: largo call más corto put, mismo precio strike. 

Esta estrategia de cobertura consiste en un portafolio de opciones donde se compra una 
opción call y se vende una opción put, ambas opciones ATM con el mismo precio strike

3. Estrategia Collar o Bull Call Spread: largo call más corto put, diferentes 
precios strikes. 

Esta estrategia tiene una variación a la anterior, los precios strikes son diferentes, la 
posición corta en put tiene un precio strike más bajo (K1) y la posición larga en call tiene 
un precio strike más alto (K2). K1 es $50 menos que el precio actual del spot (S0) y K2 es 
igual al precio actual de spot (S0), es decir que la opción call es ATM.

