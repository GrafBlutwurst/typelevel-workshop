package workshop

import workshop.adts._
import workshop.model.rawText
import simulacrum.typeclass
import scala.concurrent.Future
import workshop.typeclasses.Show.ops._
import workshop.typeclasses.Eq.ops._
import workshop.typeclasses.Monoid.ops._
import workshop.typeclasses.Functor.ops._
import scala.concurrent.ExecutionContext.Implicits.global

object typeclasses {

  //Show

  @typeclass trait Show[A] {
    def show(a: A): String
  }

  implicit def showInt: Show[Int] = new Show[Int] {
    def show(a: Int): String = a.toString
  }

  implicit def showString: Show[String] = new Show[String] {
    def show(a: String): String = a
  }

  implicit def showChessPiece: Show[ChessPiece] = cp => cp.toString

  implicit def showOption[A: Show]: Show[Option[A]] = opt => opt.fold("None")(x => Show[A].show(x))

  implicit def showTpl[A: Show, B: Show]: Show[(A,B)] = tpl => s"(${tpl._1.show},${tpl._2.show})"


  implicit def showList[A :Show]:Show[List[A]] = lst => lst.map(Show[A].show).mkString("[",",","]")





  //Eq

  @typeclass trait Eq[A] {
    def eqv(x: A, y: A): Boolean
    def ===(x: A)(y: A): Boolean = eqv(x, y)
  }

  implicit def eqAll[A]:Eq[A] = (a1, a2) => a1 == a2

  implicit def eqInt: Eq[Int] = new Eq[Int] {
    def eqv(x: Int, y: Int): Boolean = x == y
  }

  implicit def eqString: Eq[String] = new Eq[String] {
    def eqv(x: String, y: String): Boolean = x.equals(y)
  }

  implicit def eqOption[A: Eq]: Eq[Option[A]] = ???

  implicit def eqEither[A: Eq, B: Eq]: Eq[Either[A, B]] = ???







  //Monoid

  @typeclass trait Monoid[A] {
    def empty: A
    def combine(x: A, y: A): A

    def |+|(x: A)(y: A): A = combine(x, y)
  }

  implicit def intMonoid: Monoid[Int] = new Monoid[Int] {
    def empty: Int = 0

    def combine(x: Int, y: Int): Int = x + y
  }

  implicit def stringMonoid: Monoid[String] = new Monoid[String]{
    def empty: String = ""
    def combine(x: String, y: String): String = x + y
  }

  implicit def timespanMonoid: Monoid[TimeSpan] = ???

  implicit def listMonoid[A]: Monoid[List[A]] = ???

  // The intMonoid further up uses `addition` as its method of combination, but that is not the only monoid for `Int`!
  // We can also define one for multiplication, though if we just define it for `Int` again the compiler won't know which to pick
  // What we can do instead is define a small wrapper that allows us to multiply
  case class Mult(value: Int)

  implicit def multMonoid: Monoid[Mult] = ???

  def combineAll[A: Monoid](list: List[A]): A = foldMap(list)(identity)

  def foldMap[A, B: Monoid](list: List[A])(f: A => B): B = list.foldRight(Monoid[B].empty)( (a,b) => Monoid[B].combine(f(a),b))

  implicit def tupleMonoid[A: Monoid, B: Monoid]: Monoid[(A, B)] = ???

  implicit def tuple3Monoid[A: Monoid, B: Monoid, C: Monoid]: Monoid[(A, B, C)] = ???

  implicit def mapMonoid[A, B: Monoid]: Monoid[Map[A, B]] = new Monoid[Map[A,B]] {
    def empty: Map[A,B] = Map.empty[A,B]

    def combine(x: Map[A,B], y: Map[A,B]): Map[A,B] = x.toList.foldRight(y)(
      (elem, map) => map + (elem._1 -> map.get(elem._1).fold(elem._2)(elem._2 |+| _ ))
    )
  }

  implicit def futureMonoid[A: Monoid]: Monoid[Future[A]] = new Monoid[ Future[A]] {
    def empty: Future[A] = Future.successful(Monoid[A].empty)

    /*def combine(x: Future[A], y: Future[A]): Future[A] = for {
      a <- x
      b <- y
    } yield a |+| b*/

    def combine(x: Future[A], y: Future[A]): Future[A] = x.zip(y).map( tpl => tpl._1 |+| tpl._2)

  }

  //Monoid word count
  //Use foldMap with a Monoid to count the number of words, the number of characters and the number of occurences of each word
  //Tip: the Map and Tuple3 Monoid can help

  val words: List[String] = rawText.split(" ").toList


  val wc = foldMap(words)(w => (1, w.length, Map(w -> 1)))





  //Now that you have the word count let's extend it with the ability to get the longest word of the text.
  //Tip: Define a Maximum Monoid to do so
  final case class MaxLenSimple(s:String)

  implicit def mlsMonoid = new Monoid[MaxLenSimple] {
    def empty: MaxLenSimple = MaxLenSimple("")

    def combine(x: MaxLenSimple, y: MaxLenSimple): MaxLenSimple = if (x.s.length() < y.s.length()) y else x
  }

  final case class MaxLen(i:Int, s:Set[String])

  implicit def mlMonoid = new Monoid[MaxLen] {
    def empty: MaxLen = MaxLen(0, Set.empty[String])

    def combine(x: MaxLen, y: MaxLen): MaxLen = (x.i - y.i) match {
      case diff if diff > 0  => x
      case diff if diff < 0  => y
      case diff if diff == 0 => MaxLen(x.i, x.s union y.s)
    }
  }





  //Functor

  @typeclass trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
    //you can implement fmap directly here in the trait. It is just very handy to build an intuition about "Functors move functions into an effect"
    def fmap[A,B](f: A => B):F[A] => F[B] = fa => map(fa)(f)
  }

  implicit def optionFunctor: Functor[Option] = ???

  implicit def listFunctor: Functor[List] = ???



  sealed trait Tree[+A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A) extends Tree[A]

  implicit def treeFunctor: Functor[Tree] = ???






  //Cardinality

  @typeclass trait Cardinality[A] {
    def cardinality: BigInt
  }

  implicit def cardinalityUnit: Cardinality[Unit] = ???

  implicit def cardinalityBoolean: Cardinality[Boolean] = ???

  implicit def cardinalityByte: Cardinality[Byte] = ???

  implicit def cardinalityShort: Cardinality[Short] = ???

  implicit def cardinalityInt: Cardinality[Int] = ???

  implicit def cardinalityTuple[A: Cardinality, B: Cardinality]: Cardinality[(A, B)] = ???

  implicit def cardinalityEither[A: Cardinality, B: Cardinality]: Cardinality[Either[A, B]] = ???

  implicit def cardinalitySize: Cardinality[Size] = ???

  implicit def cardinalityNothing: Cardinality[Nothing]= ???

  implicit def cardinalityFunction[A: Cardinality, B: Cardinality]: Cardinality[A => B] = ???



}
