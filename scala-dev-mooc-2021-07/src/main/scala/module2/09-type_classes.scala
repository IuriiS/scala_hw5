package module2
import module2.type_classes.JsValue.{JsNull, JsNumber, JsString}

object type_classes{


  sealed trait JsValue
  object JsValue{
    final case class JsObject(get: Map[String, JsValue]) extends JsValue
    final case class JsString(get: String) extends JsValue
    final case class JsNumber(get: Double) extends JsValue
    final case object JsNull extends JsValue
  }


  // 1
  trait JsonWriter[T]{
      def write(v: T): JsValue
  }

  //2
  object JsonWriter{
      
      implicit val intJson = new JsonWriter[Int]{
          def write(v: Int): JsValue = JsNumber(v)
      }

      implicit val strToJson = new JsonWriter[String]{
          def write(v: String): JsValue = JsString(v)
      }

      implicit def optInstance[A](implicit ev: JsonWriter[A]) = new JsonWriter[Option[A]]{
          def write(v: Option[A]): JsValue = v match {
              case Some(value) => ev.write(value)
              case None => JsNull
          }
      }
  }


  //3
  def toJson[T](v: T)(implicit ev: JsonWriter[T]): JsValue = 
    ev.write(v)

  //4

  implicit class JsonSyntax[T](v: T){
        def toJson(implicit ev: JsonWriter[T]) = ev.write(v)
  }


    toJson("hello")
    toJson(13)
    toJson[Option[String]](Some("abc"))
    toJson[Option[Int]](Some(13))

    "hello".toJson
    13.toJson


  // Bindable

   // 1
   trait Bindable[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  

  // 3
//   def tupleF[F[_], A, B](a: F[A], b: F[B])(implicit ev: Bindable[F]): F[(A, B)] = 
//     ev.flatMap(a)(a => ev.map(b)(b => (a, b)))

  def tupleF[F[_] : Bindable, A, B](a: F[A], b: F[B]): F[(A, B)] = {
        Bindable[F].flatMap(a)(a => Bindable[F].map(b)(b => (a, b)))
  }


  object Bindable{
    // 2

    def apply[F[_]](implicit ev: Bindable[F]) = ev

    implicit val listBindable: Bindable[List] = new Bindable[List] {
        def map[A, B](fa: List[A])(f: A => B) = ???
        
        def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = ???
        
    }

    implicit val optBindable: Bindable[Option] = new Bindable[Option] {
        def map[A, B](fa: Option[A])(f: A => B): Option[B] = ???
        
        def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = ???
        
    }
  } 



  val optA: Option[Int] = Some(1)
  val optB: Option[Int] = Some(2)

  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)

  println(tupleF(optA, optB))
  println(tupleF(list1, list2))


  // Context Bounds, Ordering example


}