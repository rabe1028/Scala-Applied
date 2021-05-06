class Container[+T](n: T) {
  def put[E >: T](a: E): Container[E] = new Container(a)
  def get(): T = n
}
