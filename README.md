# Practica 6


## Intervalos de confianza: 

- PDFs: [diapos](https://github.com/alejandro-isglobal/alejandro-isglobal.github.io/blob/master/slides/Practica7.pdf)

- Videos:  [Video 1](https://youtu.be/tYp_hXUm0-E), [Video 2](https://youtu.be/DBaid-WVCZ4), [Video 3](https://youtu.be/0kHW1qufx9M)

## Pruebas de hipótesis: 

- PDFs:  [diapos](https://github.com/alejandro-isglobal/alejandro-isglobal.github.io/blob/master/slides/practica8.pdf), 

- datos: [PLN_Number_SOC_Temp_StoragePeriod.csv](https://github.com/alejandro-isglobal/alejandro-isglobal.github.io/blob/master/data/PLN_Number_SOC_Temp_StoragePeriod.csv) 

- Videos:  [Video 1](https://youtu.be/5cbmYIq0Enc), [Video 2](https://youtu.be/2VrJAYzvRLM), [Video 3](https://youtu.be/kFKYAbrkp-s)


## Trabajo en clase

comandos:

- <code>z.test()</code>
- <code>t.test()</code>
- <code>prop.test()</code>
- <code>varTest()</code>


[Pregunta 1](Pregunta1.png)


# Ejercicio 1

La prueba de impacto de Charpy, también conocida como prueba de muesca en V de Charpy, es una prueba de velocidad de deformación estandarizada que determina la cantidad de energía absorbida por un material durante la fractura. Se ha puesto a prueba una muestra de 10 placas de acero A238 y se han obtenido las siguientes medidas de energía en Julios: $$67, 67.3, 67.8, 66.4, 67.5, 67.5, 66.6, 67.1, 66.5, 66.9$$ Asumiendo que la energía de impacto está normalmente distribuida, hallaremos el intervalo de confianza para la media de la energía deimpacto con un nivel de confianza del 95%.

- ¿Cuál es error estándar?

- ¿Cuál es el márgen de error?

- ¿Cuáles son los extremos del intervalo?


# Ejercicio 2

Un supervisor de control de calidad en una planta enlatadora sabe que la cantidad exacta en cada lata varía, pues hay ciertos factores imposibles de controlar que afectan la cantidad de llenado.

El llenado medio por lata es importante, pero igualmente importante es la variación de la cantidad de llenado. Si la varianza es grande, algunas latas contendrán muy poco contenido, y otras, demasiado.

A fin de estimar la variación del llenado en la enlatadora, el supervisor escoge al azar 9 latas y pesa el contenido de cada una de ellas y obtiene el siguiente pesaje (en onzas): $$7.96, 7.90, 7.98, 8.01, 7.97, 8.03, 8.02, 8.04, 8.02$$

Sabiendo que el peso se distribuye normalmente, establece un intervalo de confianza del 95% para la desviación estándar de la población.

# Ejercicio 3

10 análisis repetidos de la concentración de mercurio en una muestra de condensado de gas comercial proporcionaron los siguientes resultados (en ng/ml):

$$23.3,22.5,21.9,21.5,19.9,21.3,21.7,23.8,22.6,24.7$$

Suponiendo que la concentración de mercurio se distribuye según una distribución normal con una varianza de 1.96 (en ng/ml)2
 y que el valor medio de referencia que marca el límite de alta toxicidad por mercurio es 23 ng/ml. Realiza un contraste de hipótesis, con nivel de significación de 0.05, para decidir si los datos muestrales obtenidos permiten la comercialización del producto.


# Ejercicio 4

El consum, en litres per cada 100 km, d’un determinat model d’automòbil és una variable aleatòria normal. Segons les dades del fabricant, el consum mitjà d’aquest model és de 6.8 litres per 100 km. Una organització de defensa dels consumidors ha rebut nombroses queixes indicant que el consum mitjà és superior a l’indicat pel fabricant. Per decidir si aquestes queixes estan fonamentades, l’organització ha realitzat proves de consum en 5 unitats seleccionades a l’atzar. Els consums obtinguts (en litres per 100 km) són: $$7.3, 7.1, 7.9, 7.0, 7.2$$ 
Realitzeu un contrast de hipòtesis, amb nivell de significació de 1%, per decidir si les queixes dels consumidors estan fonamentades.
