/*
Ecrivez une fonction récursive qui prend une List[Int] en paramètre et qui somme tous les éléments de la liste,
sans utiliser la fonction définie Math.sum
*/

def ex1(list: List[Int]): Int = {
  list match {
    case Nil => 0
    case x :: xs => x + ex1(xs);
  }
}

println(ex1(List(1, 2, 3, 4, 5)))

/*
Ecrivez une fonction qui prend un Vector[String] et une List[Int] en paramètre.
Cette fonction retourne une Map[Int, String], ayant pour clés les éléments de la liste et pour valeurs les élé-
ments du vecteur, en se basant sur la collection la plus petite.
*/

def ex2(vector: Vector[String], list: List[Int]): Map[Int, String] = {
  list.zip(vector).map(x => (x._1, x._2)).toMap;
}

println(ex2(Vector("Hey", "Wadup", "oy"), List(1, 2, 3, 4)))


/*
Définissez un Vector[Int] contenant les valeurs de 1 à 1000.
Ecrivez une fonction qui prend un Int en paramètre.
— Si cet Int est un multiple de trois, retourne son cube.
— Si cet Int est un multiple de deux, retourne son carré.
— Si cet Int est impair, retourne -1.
Appliquez cette fonction à toutes les valeurs du vecteur.
*/
def ex3(i: Int): Int = {
  i match {
    case l if (i % 3 == 0) => i * i * i
    case l if (i % 2 == 0) => i * i
    case l if (i % 2 == 1) => -1
  }
}

(for (i <- 0 to 1000) yield i).map(x => println(ex3(x)))

/*
Définissez un Vector[Int] contenant les valeurs de 1 à 10.
Ecrivez une fonction qui prend un Int en paramètre, et retourne le factoriel de ce nombre, en utilisant une
implémentation récursive.
Ecrivez une fonction qui prend un Int en paramètre, et retourne le nombre de fibonacci correspondant, en utilisant
une implémentation récursive.
Appliquez les fonctions au vecteur comme suit :
— Si le nombre est pair, lui appliquer la fonction factoriel.
— Si le nombre est impair, lui appliquer la fonctione fibonacci.
*/

def factorial(i: Int): Int = {
  if (i > 0) i * factorial(i - 1) else 1
}

def fibonacci(n: Int): Int = {
  n match {
    case 0 => 0
    case 1 => 1
    case _ => fibonacci(n - 1) + fibonacci(n - 2)
  }
}

println((for (i <- 1 to 10) yield i).map(i => i match {
  case i if i % 2 == 0 => factorial(i)
  case _ => fibonacci(i)
}))

/*
Définissez un Vector[Int] contenant les valeurs de 1 à 5 et une List[Int] contenant les valeurs de 1 à 10.
Ecrivez une fonction qui prend le vecteur et la liste en paramètre, et qui retourne un Vector[List[Int]], où
chaque liste du vecteur est la liste initiale multipliée par une des composantes du vecteur.
*/

def ex5(vector: Vector[Int], list: List[Int]): Vector[List[Int]] = {
  vector.map(i => list.map(j => i * j))
}

println(ex5(Vector(1, 2, 3), List(1, 1, 1)))


/*
Définissez une List[Int] contenant les valeurs (1, 5, 7, 2, 8, 4, 3, 6, 9).
Ecrivez la fonction insert, qui permet d’ordonner la liste dans un ordre croissant.
Appelez la fonction customSort avec la liste définie.
*/

val list = List(1, 5, 7, 2, 8, 4, 3, 6, 9)

def customSort(list: List[Int]): List[Int] = list match {
  case Nil => Nil
  case x :: xs => insert(x, customSort(xs))
}

def insert(value: Int, list: List[Int]): List[Int] = {
  list match {
    case Nil => List(value)
    case x :: xs => if (value < x) value :: list else x :: insert(value, xs)
  }
}
println("customSort")
println(customSort(list))

/*
Ecrivez une fonction prenant une List[Int] et une condition Int => Boolean en paramètre.
Cette fonction permet de filtrer la liste passée en paramètre selon la condition définie, en ne conservant que les
éléments de la liste validant cette condition.
Vous pouvez vérifier votre implémentation en utilisant la fonction filter prédéfinie pour les collections, et
vérifier, par exemple, que la liste ne conserve que les nombres impairs.
*/

def ex7(list: List[Int], condition: Int => Boolean): List[Int] = {
  list.filter(condition)
}
def cond(value: Int): Boolean = {
  value % 2 == 1
}
println("Filter: " + ex7(list, cond))