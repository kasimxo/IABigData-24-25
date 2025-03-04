# Ejercicios de Python
# 1. Verifique las versiones de python 2 y 3 instaladas en el laboratorio.
!python3 --version
# 2. Instale y ejecute JupyterLab.
pip install jupyterlab # En local

# 3. Genere un script “Hola Mundo” en python. Ejecute el script de las dos formas vistas en clase. Comentarios y Variables
# 4. Incorpore comentarios por línea y multi-línea al script Hola Mundo.
# 5. Asigne a una variable valores enteros, reales y strings verificado el tipo de datos con la función type. Tipos de Datos
# 6. ¿Cuántos tipos de datos numéricos incluye python por defecto?
# 7. ¿Cuáles son los valores posibles del tipo bool ? ¿ true es igual que True?
# 8. ¿A qué número se corresponde el valor False ?
# 9. ¿Cuál es el valor máximo que entra en un entero?
# 10. Defina el valor entero 12 en decimal, binario, hexadecimal y octal.
# 11. ¿Cómo se define un número real (punto flotante) ?
# 12. ¿Qué precisión tiene su equipo usando números reales?
# 13. ¿Cómo se define un número complejo?
# 14. ¿Cómo se define un string?
# 15. ¿Qué significa el valor None? ¿Cómo se usa?
# 16. Suponiendo (nombre = "DevOps"), ¿Qué devuelve el nombre[1]?
# 17. ¿Qué pasa con el nombre[-2]?
# 18. ¿Qué pasa con el nombre[1: -1]?
# 19. ¿Cómo obtener la longitud de nombre?
# 20. ¿Qué retorna nombre.title ()?
# 21. Si nombre="Programación para DevOps". ¿Cómo podemos verificar si el nombre contiene "DevOps" con el método find? Listas y Tuplas
# 22. ¿Cómo se define una lista? ¿Cómo se define una tupla? ¿Cuál es la diferencia?
# 23. Teniendo en cuanta que a = [10,20,30,40,50,60,70,80,90]. ¿Qué retorna: len(a), min(a), max(a)?
# 24. ¿Cómo se puede agregar el valor 100?
# 25. Investigue como usando el operador + se puede añadir una lista con los valore 110 120 130.
# 26. Borre el último valor.
# 27. Muestre la posición 1 a 5.
# 28. Borre los últimos 3 valores en una sola línea.
# 29. En una sola linea modifique los primeros tres posiciones para tener 1, 2 y 3 en lugar de 10, 20 y 30.
# 30. ¿Qué pasa si hacemos a[:] = [] ?
# 31. a =[1, 2,3] ¿Cuál es el resultado de hacer a=a*3?
# 32. a =[1, 2,3,4,5] ¿Cuál es el resultado de hacer a[::-1]?
# 33. ¿Cómo obtenemos [5,3,1]?
# 34. Si tenemos a = [10,20,[30,40,50],60]. ¿Qué retorna a[2][1]?
# 35. Si tenemos la lista a=[1,2,3] ¿Qué hacen los siguientes métodos?
# 36. a.count(2)
# 37. a.append(4)
# 38. a.append([5,6])
# 39. a.extend([7,8,9])
# 40. a.pop()
# 41. a.pop(2)
# 42. a.remove(20)
# 43. a.insert(1,20)
# 44. a.reverse()
# 45. a.sort()
# 46. a.sort(reverse=True)
# 47. Si tenemos la tupla a=(10,20,30,40,50). ¿Que retorna lo siguiente?
# 48. len(a)
# 49. min(a)
# 50. max(a)
# 51. a.count(50)
# 52. a.index(50)
# 53. Si tenemos a=(10,20,30,40,50); b = (60, 70) ¿Qué retorna a+b y a*2? Diccionarios
# 54. ¿Cómo se genera un diccionario? ¿Es modificable?
# 55. Aunque los diccionarios se suelen crear con los {}, también es posible crear un diccionario usando la clase dict. Verifique que a y b contiene lo mismo: a = {'joe' : 85 , 'peter' : 88 , 'jack' : 90} b = dict (joe = 90 , peter = 85 , jack = 88)
# 56. ¿Qué retorna len(a)?
# 57. ¿Cómo agregamos un valor? ¿Y cómo lo borramos?
# 58. Verifique que es posible determinar si una clave está presente o no en un diccionario usando in // not in. Ejemplos: 'peter' in a 'Carlos' not in a
# 59. Si tenemos a = {'Carlos' : 85 , 'Pablo' : 88 , 'Daniel' : 90} ¿Qué retornan los siguientes métodos:?
# 60. a.get('Carlos', 0)
# 61. a.get('Pepe', 0)
# 62. a.keys()
# 63. a.values()
# 64. list(a.values())
# 65. a.items()
# 66. Teniendo notas = {'Carlos': [80,85,90], 'Pablo' : [85,90,95]} ¿Que retorna notas['Pablo'][0]? Conjuntos
# 67. ¿Cómo se genera un conjunto?
# 68. Aunque es habitual crear los conjuntos a partir de una instancia de la clase set, se pueden crear usando las llaves {}, separando cada elemento por comas. Se diferencia de los diccionarios ya que no llevan los dos puntos. Verifique que es equivalente: s = {1, 2, 3, 4} s = set([1,2,3,4])
# 69. Verifique que al igual que otras colecciones en python, en los conjuntos sus elementos pueden ser de diversos tipos: s = {True, 3.14, None, False, "Hola mundo", (1, 2)}
# 70. Verifique que un conjunto no puede incluir objetos mutables como listas, diccionarios, e incluso otros conjuntos. ¿Qué pasa con s = {[1, 2]}?
# 71. Un set puede ser convertido a una lista y viceversa. En este último caso, los elementos duplicados son unificados. Pruebe con el siguiente ejemplo: list({1, 2, 3, 4}) set([1, 2, 2, 3, 4])
# 72. Los conjuntos son objetos mutables. Vía los métodos add() y discard() podemos añadir y remover un elemento indicándolo como argumento. Verifique que hace: s = {1, 2, 3, 4} s.add(5) s.discard(2)
# 73. Para determinar si un elemento pertenece a un conjunto, utilizamos la palabra reservada in o not in. ¿Qué retorna: ? 2 in {1, 2, 3} 4 in {1, 2, 3}
# 74. Genera la unión, la intersección, y la diferencia entre los siguientes conjuntos: a = {1, 2, 3, 4} b = {3, 4, 5, 6}
# 75. ¿Cómo se genera un conjunto inmutable ? Bytes, Betearray
# 76. ¿Cómo se genera una cadena de texto byte en python? ¿Es modificable?
# 77. ¿Qué pasa si intentamos crear la cadena de texto byte de la siguiente manera pais = b"España"?
# 78. ¿Qué pasa si hacemos: pais = bytes("España", "utf-8") ?
# 79. Crear un bytearray desde un bytes, un string y una lista de entero