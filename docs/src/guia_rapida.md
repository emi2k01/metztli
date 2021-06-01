# Guía rápida

## Tipos básicos

Metztli cuenta con los siguientes tipos integrados:

### Tipos numéricos enteros

El signo de un número se refiere al signo positivo o negativo. Un número sin
signo solo puede representar números positivos.

|           | 8 bits | 16 bits | 32 bits | 64 bits | 128 bits |
|-----------|--------|---------|---------|---------|----------|
| Con signo | ent8   | ent16   | ent32   | ent64   | ent128   |
| Sin signo | entp8  | entp16  | entp32  | entp64  | entp128  |

Adicionalmente:

- `ent` es un alias para `ent32`.

- `entp` es un alias para `entp32`.

#### Valores literales

- Literal en base 10 (decimal):

	`24`

- Literal en base 2 (binario):

	`0b1100`

- Literal en base 8 (octal):

	`0o30`

- Literal en base 16 (hexadecimal):

	`0x18`

También se pueden utilizar guiones bajos en las anteriores literales para
mejorar la legibilidad. Esto no cambia el el valor o la representación interna y solo
sirve como separador visual.

```metz
100_000_000

0b1110_0010_0010_1000

0o300_234_120

0xFF_E8_D1
```

### Tipos numéricos flotantes

Un número flotante es un número decimal fraccionario en el cual el punto
decimal cambia de posición dependiendo del valor, es decir, "flota".

Todos los números flotantes tienen signo.

| 32 bits   | 64 bits   |
|-----------|-----------|
| flota32   | flota64   |

Adicionalmente:

- `flota` es un alias para `flota32`.

#### Valores literales

Los valores literales de los tipos numéricos flotantes tienen la misma forma
que los valores literales de los tipos numéricos enteros, con el requerimiento
adicional de un punto para indicar el inicio de la parte fraccionaria. A
diferencia de otros lenguajes, la parte fraccionaria es obligatoria.

```metz
24.0

0b100_000.01

0o30.76

0x18.2f
```

Además se pueden escribir valores literales en forma exponencial (o notación científica).

`10E+3`

### Tipo cadena de texto

Las cadenas de texto son contenedores que almacenan una serie de carácteres Unicode UTF-8.

Las siguientes secuencias de escape son aceptadas:

**TODO**

#### Valores literales

- Cadena básica:

	`"Hola, mundo!"` => Hola, mundo!

- Cadena con secuencias de escape.

	`"Sepa\trado"` => Sepa &nbsp; &nbsp; &nbsp; &nbsp; rado

	`"El dijo \"Hola\" ayer"` => El dijo "Hola" ayer

	`"Diagonal invertida: \\"` => Diagonal invertida: \\

`caract`: Carácter unicode UTF-8.

### Tipo booleano

`bool`: Tipo booleano

Solo tiene dos posibles valores: `verdad` y `falso`.

---

## Variables

**Sintáxis:**

```metz
TIPO IDENTIFICADOR = EXPRESION;
```

El `IDENTIFICADOR` de una variable debe cumplir con las siguientes reglas:

- Debe comenzar con un carácter NO numérico.
- Solo puede contener carácteres del alfabeto español, carácteres numéricos y guiones bajos.
- Exceptuando la letra `ñ`, los carácteres no pueden llevar marcas gráficas (ej. á, ü, etc)

Los identificadores también son sensible a mayúsculas, es decir, `ent edad = 18;` y
`ent EdaD = 18;` crean variables con diferentes identificadores.

**Ejemplo 1:**

Descripción: Crear variable de tipo entero de 32 bits con el identificador `edad` y
asignarle el valor 18.

```metz
ent edad = 18;
```

**Ejemplo 2:**

Descripción: Crear variable de tipo cadena de texto con el identificador `apellido` y
asignarle el valor `González`.

```metz
cadena apellido = "González";
```

**Ejemplo 3:**

Descripción: Crear variable de tipo flotante de 64 bits con el identificador
`estatura` y asignarle el valor 1.83.

```metz
flota64 estatura = 1.83;
```

**Ejemplo 4:**

Descripción: Crear variable de tipo booleano con el identificador
`puerta_abierta` y asignarle un valor verdadero.

```metz
bool puerta_abierta = verdad;
```
