package module2

object higher_kinded_types{

  def tuple[A, B](a: List[A], b: List[B]): List[(A, B)] =
    a.flatMap{ a => b.map((a, _))}

  def tuple[A, B](a: Option[A], b: Option[B]): Option[(A, B)] =
    a.flatMap{ a => b.map((a, _))}

  def tuple[E, A, B](a: Either[E, A], b: Either[E, B]): Either[E, (A, B)] =
    a.flatMap{ a => b.map((a, _))}


  def tuplef[F[_], A, B](fa: F[A], fb: F[B]): F[(A, B)] = ???


  trait Bindable[F[_], A] {
    def map[B](f: A => B): F[B]
    def flatMap[B](f: A => F[B]): F[B]
  }



  def tupleBindable[F[_], A, B](fa: Bindable[F, A], fb: Bindable[F, B]): F[(A, B)] = 
    fa.flatMap{ a => fb.map((a, _))}


  def listBindable[A](list: List[A]): Bindable[List, A] = new Bindable[List, A] {
    def map[B](f: A => B): List[B] = list.map(f)
    
    def flatMap[B](f: A => List[B]): List[B] = list.flatMap(f)
    
  }
  
  def optBindable[A](opt: Option[A]): Bindable[Option, A] = ???


  val optA = Some(1)
  val optB = Some(2)

  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)

  println(tupleBindable(optBindable(optA), optBindable(optB)))
  println(tupleBindable(listBindable(list1), listBindable(list2)))


}