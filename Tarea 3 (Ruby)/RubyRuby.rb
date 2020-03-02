#!/usr/bin/ruby

=begin
    Considere una clase Vector2, representando elementos de R2,
    con dos campos, x, y correspondiente a las coordenadas del vector.
=end

#   a) (0.25 pts) – Defina la clase en cuestión, con ambos campos.

=begin
    b) (0.25 pts) – Implemente setters y getters para ambos campos.
    Implemente un método initialize (constructor) que reciba dos números
    e inicialice las coordenadas x, y del vector.
=end

=begin
    c) (0.25 pts) – Implemente un método magnitud que calcule la magnitud
    de un vector. Use como norma la distancia euclı́dea entre las dos coordenadas
    del vector (la definición usual de norma).
=end

=begin
    d) (0.5 pts) – Implemente métodos para sumar, restar, igualdad
    y multiplicar (producto punto/escalar) dos Vector2. Debe implementarlos
    como operadores, usando +, −, == y ∗ respectivamente. Implemente además
    un método para − como operador unario, de manera que devuelva una copia
    del mismo vector con los signos de sus coordenadas invertidos.
=end

=begin
    e) (0.25 pts) – Implemente el método to s de manera que cualquier vector
    pueda ser representado en una cadena de caracteres como (x, y).
=end

=begin
    Los números complejos C tienen una interpretación geométrica en R2, tomando
    la parte real como la coordenada x y la parte imaginaria como la coordenada y.
    Considere una subclase Complejo de Vector2.
=end

=begin
    a) (0.25 pts) – Defina la subclase en cuestión.
    Debe redefinir el método initialize para que el valor
    de la parte imaginaria (y) sea 0 por defecto.
=end

=begin
    b) (0.75 pts) – Redefina el método de multiplicación (∗)
    para reflejar las reglas de multiplicación de complejos.
    Implemente además, usando ∼ como operador unario, un método
    que devuelva el conjugado de un número complejo (es decir,
    debe retornar una copia del mismo número, con el signo de
    la parte imaginaria invertido). Luego, implemente, usando el
    operador /, un método para la división de dos números complejos.
    En caso de que se divida por cero, debe atajar la excepción e
    imprimir ”No se puede dividir entre 0”.
=end

=begin
    c) (0.5 pts) – Implemente el método to s de manera que ahora 
    la representación en cadena de caracteres sea la usual para
    números complejos. Es decir:
        Si ambas partes son positivas: x + yi
        Si la parte real es negativa: −x + yi
        Si la parte imaginaria es negativa: x − yi
        Si ambas partes son negativas: −x − yi
        Si la parte imaginaria es cero: x
        Si la parte real es cero: yi
=end

=begin
    2. (2.5 pts) – Defina una clase Moneda con subclases
        Dolar,Yen,Euro,Bolivar y Bitcoin.
=end

=begin
    a) (0.5 pts) – Defina métodos dolares, yens, euros, bolivares
    y bitcoins sobre la clase Float que convierta el flotante en dólares,
    yens, euros, bolı́vares y bitcoins, respectivamente.
=end

=begin
    b) (1 pt) – Defina un método en sobre la clase Moneda (y sus subclases, por ende)
    que reciba un átomo entre :dolares, :yens, :euros, :bolivares y :bitcoins 
    y convierta la moneda en aquella representada por el átomo propuesto.
    Por ejemplo: 15.dolares.en(:euros) debe evaluar en 12.72 euros.
=end

=begin
    c) (1 pt) – Defina un método comparar sobre la clase Moneda, que
    reciba otra Moneda y las compare.
    Debe devolver :menor si la primera moneda es menor que el argumento.
    Debe devolver :igual si la primera moneda es igual que el argumento.
    Debe devolver :mayor si la primera moneda es mayor que el argumento.
    Por ejemplo: 100000.bolivares.comparar(2.dolares) debe evaluar en :menor.
    Nota: Use doble despacho para averiguar los tipos del argumento pasado.
    No pregunte por el tipo explı́citamente.
=end

#3. (1.5 pts) – Bloques e iteradores.
=begin
    Dadas dos colecciones (de tipos posiblemente diferentes),
    se desea calcular el producto cartesiano de los elementos
    generados para cada una de ellas.
    Por ejemplo: El producto cartesiano de [:a, :b, :c] y [4, 5] debe generar:
    [:a, 4]
    [:a, 5]
    [:b, 4]
    [:b, 5]
    [:c, 4]
    [:c, 5]
    Nota 1: No importa el orden en que se devuelvan los elementos,
    sino que todos los elementos aparezcan.
    Nota 2: El elemento [:a, 4] está en el resultado del ejemplo anterior,
    pero [4. :a] no. El orden interno de las tuplas es importante.
=end


def cartesianProd(a,b)
    return a.product(b)
end

cartesianProd([:a, :b, :c],[4, :a, 5]).each do |i|
    p i
end

class CartesianProd 
    def initialize( a, b ) 
         @a = a
         @b = b
    end

    def iterate
        @a.each do |i|
            @b.each do |j|
                yield( [i,j] )
            end
        end
    end
end

cartesiano = CartesianProd.new([:a, :b, :c], [4, 5])

cartesiano.iterate do |i|
    p i
end