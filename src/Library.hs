module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

{- ---------------- Punto 1 -----------
Modelado
    a. Generar un data modelando la pizza.
    b. Crear la función constante grandeDeMuzza, que es una pizza que tiene “salsa”, “mozzarella” y
    “orégano”, tiene 8 porciones, y tiene 350 calorías.
-}

data Pizza = UnaPizza {
    ingredientes :: [String],
    tamanio :: Number,
    calorias :: Number
} deriving (Show, Eq)

grandeDeMuzza = UnaPizza ["salsa","mozzarela","oregano","peperoni","salamin"] 8 350
grandeDeMuzza2 = UnaPizza ["salsa","mozzarela","anana"] 8 350
grandeDeMuzza3 = UnaPizza ["salsa","palmito","anana"] 8 350

{- ---------------- Punto 2 -----------
Calcular nivel de satisfacción que da una Pizza:
    a. 0 si tiene palmito
    b. cantidad de ingredientes * 80, siempre y cuando tenga menos de 500 calorías, en caso
    contrario es la mitad del cálculo.
-}

satisfaccion :: Pizza -> Number
satisfaccion pizza
    | "palmito" `elem` ingredientes pizza = 0
    | calorias pizza < 500 = cantidadDeIngredientes pizza * 80
    | otherwise = (cantidadDeIngredientes pizza * 80) / 2

cantidadDeIngredientes :: Pizza -> Number
cantidadDeIngredientes pizza = length (ingredientes pizza)

{- ---------------- Punto 3 -----------
    Calcular el valor de una pizza que es 120 veces la cantidad de ingredientes, multiplicado por su tamaño
-}

valorDePizza :: Pizza -> Number
valorDePizza pizza = (cantidadDeIngredientes pizza * 120) * tamanio pizza


{- ---------------- Punto 4 -----------
    Implementar las siguientes funciones
-}
type NuevaPizza = Pizza -> Pizza

mapIngrediente :: [String] -> NuevaPizza
mapIngrediente nuevoIngrediente pizza = pizza {ingredientes = nuevoIngrediente}

mapTamanio :: Number -> NuevaPizza
mapTamanio nuevoTamanio pizza = pizza {tamanio = nuevoTamanio}

mapCalorias :: Number -> NuevaPizza
mapCalorias nuevaCantCalorias pizza = pizza {calorias = nuevaCantCalorias}

{- Punto 4 (A)
    nuevoIngrediente : Agrega un ingrediente a una pizza y agrega en calorías el doble de la cantidad de letras que tiene dicho ingrediente
-}

nuevoIngrediente :: String -> NuevaPizza
nuevoIngrediente nuevoIngrediente pizza =
    mapIngrediente (ingredientes pizza ++ [nuevoIngrediente]) .
    mapCalorias (length nuevoIngrediente * calorias pizza) $ pizza

{- Punto 4 (B)
    agrandar : agrega 2 porciones al tamaño. En el caso de ya tener el máximo de porciones, las mismas siguen siendo dicho máximo.
-}

agrandar :: NuevaPizza
agrandar pizza
    | tamanio pizza + 2 >= 10 = mapTamanio 10 pizza
    | otherwise = mapTamanio (tamanio pizza + 2) pizza

{- Punto 4 (C)
    mezcladita : es la combinación de 2 gustos de pizza, donde ocurre que la primera se le mezcla
    a la segunda, es decir, los ingredientes se le suman (sacando los repetidos) y de las calorías
    se le suma la mitad de la primera pizza a combinar. 
    Por ejemplo, si mezclamos una pizza chica
    de provolone con jamón con una gigante napolitana, queda una gigante napolitana con
    provolone y jamón. (Sí, este punto se pensó con hambre).
    Nota: No duplicar lógica
-}

mezcladita :: Pizza -> Pizza -> Pizza
mezcladita pizza1 pizza2 =
    mapCalorias (tamanio pizza2 + (tamanio pizza1 / 2)) .
    agregarIngredienteSinRepetir (ingredientes pizza1) $ pizza2

agregarIngredienteSinRepetir :: [String] -> Pizza -> Pizza
agregarIngredienteSinRepetir ingredientesEntrantes pizza2 =
    mapIngrediente (ingredientes pizza2 ++ filter (\ingreEntrante -> ingreEntrante `notElem` ingredientes pizza2) ingredientesEntrantes) pizza2

{- ---------------- Punto 5 -----------
    Calcular el nivel de satisfacción de un pedido, que es la sumatoria de la satisfacción que brinda cada pizza que compone el mismo. Nota: Usar composición.
-}

type Pedido = [Pizza]

satisfaccionDePedido :: Pedido -> Number
satisfaccionDePedido listaPizzas = sum (map satisfaccion listaPizzas) 

{- ---------------- Punto 6 -----------
    Modelar las siguientes pizzerias
-}
{- Punto 6 (A)
    pizzeriaLosHijosDePato : A cada pizza del pedido le agrega palmito
-}

pizzeriaLosHijosDePato :: Pedido -> Pedido
pizzeriaLosHijosDePato listaPizzas = map (agregarIngredienteSinRepetir ["palmito"]) listaPizzas

{- Punto 6 (B)
    pizzeriaElResumen : Dado un pedido, entrega las
    combinaciones de una pizza con la siguiente. Es
    decir, la primera con la segunda, la segunda con la
    tercera, etc. (y, por lo tanto, termina enviando un pedido que tiene una pizza menos que el
    pedido original, por el resultado de la combinación de pares de pizzas). Si el pedido tiene una
    sola pizza, no produce cambios. 
    Nota: En esta definición puede usarse recursividad, aunque
    no es necesario. pro-tip: función zip o zipWith.
-}

-- ! Este punto no lo entendi muy bien
pizzeriaElResumen :: Pedido -> [Pedido]
pizzeriaElResumen [] = []  
pizzeriaElResumen [_] = []  
pizzeriaElResumen (pizza1:pizza2:restoPizzas) = 
    [pizza1 `mezcladita` pizza2] : pizzeriaElResumen (pizza2:restoPizzas)


{- Punto 6 (C)
    pizzeriaEspecial : Una pizzería especial tiene un sabor predilecto de pizza y todas las pizzas
    del pedido las combina con esa.
    ■ La pizzeriaPescadito es un caso particular de este, donde su sabor predilecto es de
    anchoas básica: tiene salsa, anchoas, sólo posee 270 calorías y es de 8 porciones
-}

-- ! NO entendi si tengo que hacer 2 funciones separadas o pizzeriaPescadito es la pizza pescadito
pescadito = UnaPizza ["salsa","anchoas"] 8 270

pizzeriaEspecial :: Pedido -> Pedido
pizzeriaEspecial listaPizzas = map (\pizza -> pizza `mezcladita` pescadito) listaPizzas


{- Punto 6 (D)
    pizzeriaGourmet : Del pedido solo envía aquellas para las cuales el nivel de satisfacción
    supera el nivel de exquisitez de la pizzería... el resto no, las considera deplorables. Y, de
    regalo, a aquellas que manda las agranda a la siguiente escala, si esto es posible.
        ■ La pizzeriaLaJauja, es un clásico caso gourmet con un parámetro de exquisitez de
        399
-}

pizzeriaGourmet :: Pedido -> Pedido
pizzeriaGourmet listaPizzas = map (agrandar) $ filter (pizzeriaLaJauja) listaPizzas

pizzeriaLaJauja :: Pizza -> Bool
pizzeriaLaJauja pizza = (>399). satisfaccion $ pizza

{- ---------------- Punto 7 -----------
Pizzerías & Pedidos
    a. Implementar la función sonDignasDeCalleCorrientes que, dado un pedido y una lista de
    pizzerías, devuelve aquellas pizzerías que mejoran la satisfacción del pedido.
    b. Dado un pedido y una lista de pizzerías encontrar la pizzería que maximiza la satisfacción que
    otorga el pedido.
-}

-- ! NO funciona con pizzeriaElresumen
sonDignasDeCalleCorrientes :: Pedido -> [Pedido -> Pedido] -> [Pedido -> Pedido]
sonDignasDeCalleCorrientes pedido listaPizzerias = filter (\pizzeria -> satisfaccionDePedido pedido < satisfaccionDePedido (pizzeria pedido)) listaPizzerias


maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

-- ! Preguntar como hacer el b 
--pizzeriaConMayorSatisfaccion pedido listaPizzerias = maximoSegun () listaPizzerias
