
def integracion(f: Double => Double, a: Double, b: Double): Double = {
    val valX = (a + b) / 2.0
    val aprox = (b - a) / 6.0 * (f(a) + 4.0 * f(valX) + f(b))
    aprox
  }
  def margenError(valorEsperado: Double,valorObtenido :Double): Double = Math.abs(valorEsperado-valorObtenido)
 
  
 //Se definen Las funciones con las ecuaciones del las formulas propuestas
  
  
    def ec1(x: Double): Double = { -Math.pow(x, 2) + 8 * x - 12 }
    def ec2(x: Double): Double = { 3 * Math.pow(x, 2) }
    def ec3(x: Double): Double = { x + 2 * Math.pow(x, 2) - Math.pow(x, 3) + 5 * Math.pow(x, 4) }
    def ec4(x: Double): Double = { ((2*x)+1)/((x*x)+x)} 
    def ec5(x: Double): Double = { Math.pow(Math.E, x)}
    def ec6(x: Double): Double = { 1 / Math.sqrt(x - 1)}
    def ec7(x: Double): Double = {1 /(1+Math.pow(x, 2)) }
    

   // Ejemplo de uso para la función de aproximación de 3 & 5
  val aprox1 = integracion(ec1, 3, 5)
  println(s"El resultado de la integración es: ${integracion(ec1, 3, 5)} " +
    s"y su margen de error es ${margenError(7.33, aprox1)}")

   val aprox2 = integracion(ec2, 0, 2)
  println(s"El resultado de la integración es: ${integracion(ec2, 3, 5)} " +
    s"y su margen de error es ${margenError(8.0, aprox2)}")

   val aprox3 = integracion(ec3, -1, 1)
  println(s"El resultado de la integración es: ${integracion(ec3, 3, 5)} " +
    s"y su margen de error es ${margenError(3.333, aprox3)}")

   val aprox4 = integracion(ec4, 1, 2)
  println(s"El resultado de la integración es: ${integracion(ec4, 3, 5)} " +
    s"y su margen de error es ${margenError(1.0986, aprox4)}")

   val aprox5 = integracion(ec5, 0, 1)
  println(s"El resultado de la integración es: ${integracion(ec5, 3, 5)} " +
    s"y su margen de error es ${margenError(1.718228, aprox5)}")

   val aprox6 = integracion(ec6, 2, 3)
  println(s"El resultado de la integración es: ${integracion(ec6, 3, 5)} " +
    s"y su margen de error es ${margenError(0.822842, aprox6)}")

   val aprox7 = integracion(ec7, 0, 1)
  println(s"El resultado de la integración es: ${integracion(ec7, 3, 5)} " +
    s"Margen de error es ${margenError(0.785398, aprox7)}")
