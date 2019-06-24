package workshop

import workshop.typeclasses._
import workshop.model._
import simulacrum.typeclass
import scala.concurrent.Future
import abstractions.Monoidal.ops._
import abstractions.Traverse.ops._
import scala.concurrent.ExecutionContext.Implicits.global
import adts.Iso

object abstractions {


  //Multiplicative Monoidal Functors

  @typeclass trait Monoidal[F[_]] extends Functor[F] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]

    def unit: F[Unit]

    //technically pure is NOT a member of Monoidal but it's just way to handy to have. Pure is a member of the formulation through applicative but as you can see we can express it easily enough
    def pure[A](a: A): F[A] = map(unit)(_ => a)

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      map(product(fa, fb)) { case (a, b) => f(a, b) }
  }

  //An alternative (and in practice more common formulation of Multiplicative Monoidal Functors is called Applicative but they're the same as shown bellow
  //The reason that Applicative is more often chosen as the cannonical representation are not known to me but I suspect it's the case because it's what Haskell does and it's shorter.
  //However most of the things that we use in Scala with Applicative really rely more on its nature as a product monoidal rather than its capability of `ap`
  @typeclass trait Applicative[F[_]] extends Functor[F] {
    override def map[A,B](fa:F[A])(f:A=>B):F[B]= ap(pure(f))(fa)
    def pure[A](a:A):F[A]
    def ap[A,B](fab:F[A=>B])(fa:F[A]):F[B]
  }

  def applicativeIsSameAsMonoidal[F[_]]:Iso[Monoidal[F],Applicative[F]] = Iso(
    (m:Monoidal[F]) => new Applicative[F]{
      override def pure[A](a:A):F[A] = m.pure(a)
      override def ap[A,B](fab:F[A=>B])(fa:F[A]):F[B] = map(m.product(fab, fa))(tpl => tpl._1(tpl._2))
    }
  )(
    (a:Applicative[F]) => new Monoidal[F]{
      override def map[A, B](fa: F[A])(f: A => B): F[B] = a.map(fa)(f)
      override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = a.ap(map(fa)( a => ((b:B) => (a,b)) ))(fb)
      override def unit: F[Unit] = a.pure(())
    }
  )

  implicit def optionMonoidal: Monoidal[Option] = ???

  implicit def futureMonoidal: Monoidal[Future] = ???

  // There are two possible solutions here, can you figure out which?
  implicit def listMonoidal: Monoidal[List] = ???


  def bundle[F[_]: Monoidal, A](x: F[A], y: F[A]): F[List[A]] = ???

  def appendM[F[_]: Monoidal, A](x: F[A], y: F[List[A]]): F[List[A]] = ???

  def sequence[F[_]: Monoidal, A](list: List[F[A]]): F[List[A]] = ???

  def traverse[F[_]: Monoidal, A, B](list: List[A])(f: A => F[B]): F[List[B]] = ???

  def ap[F[_]: Monoidal, A, B](ff: F[A => B], fa: F[A]): F[B] = ???

  //Given two Option[Int] multiply the int values if they exist or leave them unchanged if one of them doesn't
  def combineOptions(x: Option[Int], y: Option[Int]): Option[Int] = ???




  //Foldable

  @typeclass trait Foldable[F[_]] {
    //implementations of foldMap should only go through F ONCE
    def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B

    def combineAll[A: Monoid](fa: F[A]): A = foldMap(fa)(identity)
  }

  implicit def optionFoldable: Foldable[Option] = ???

  implicit def listFoldable: Foldable[List] = ???

  implicit def setFoldable: Foldable[Set] = ???

  // Turn this foldable into a List
  def fromFoldable[F[_]: Foldable, A](fa: F[A]): List[A] = ???

  // Find the first element that matches the predicate
  // Hint: YOu might need to defne a new type with a new monoid
  def find[F[_]: Foldable, A](fa: F[A], f: A => Boolean): Option[A] = ???





  //Traversable
  @typeclass trait Traverse[F[_]]  {
    def traverse[G[_]: Monoidal, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

    def sequence[G[_]: Monoidal, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)
  }

  implicit def listTraversable: Traverse[List] = ???

  implicit def optionTraversable: Traverse[Option] = ???

  implicit def eitherTraversable[E]: Traverse[Either[E, ?]] = ???



  //Validated

  sealed trait Validated[+E, +A]
  case class Valid[+A](a: A) extends Validated[Nothing, A]
  case class Invalid[+E](e: E) extends Validated[E, Nothing]

  type ValidatedList[+E, +A] = Validated[List[E], A]

  def toEither[E, A](v: Validated[E, A]): Either[E, A] = ???

  def toValidated[E, A](e: Either[E, A]): Validated[E, A] = ???

  implicit def validatedMonoidal[E: Monoid]: Monoidal[Validated[E, ?]] = ???

  implicit def validatedTraversable[E]: Traverse[Validated[E, ?]] = ???


  // Validation exercise
  // Use `Validated` and everything you've learned so far to solve this one
  // In the `model` object you can find two lists of unvalidated strings representing users
  // Your job is to check all of them whether they are valid or not.
  // To do so, you should use the `User.validate` function.
  // Once your done, you can check the difference to Either
  def allUsers = ???





  // Next we want to write a function that takes a String representing a user
  // and return the UserReport for that user using the `User.fetchReport` function
  def reportForUser(u: String): Future[ValidatedList[String, UserReport]] = ???




  // Hard: Now get all reports for all the users
  def allReports = ???










  // Nested Monoidals

  case class Nested[F[_], G[_], A](value: F[G[A]])

  implicit def nestedMonoidal[F[_]: Monoidal, G[_]: Monoidal]: Monoidal[Nested[F, G, ?]] = ???


  // Try implementing `allReports` using `Nested`, it should be much easier this way
  def allReportsUsingNested: Future[ValidatedList[String, List[UserReport]]] = ???





  @typeclass trait ContravariantFunctor[F[_]] {
    def contramap[A, B](fa: F[A])(f: B => A): F[B]
  }

  case class Predicate[A](run: A => Boolean)

  case class StringEncoder[A](run: A => String)

  implicit def predicateContravariant: ContravariantFunctor[Predicate] = ???

  implicit def stringEncoderContravariant: ContravariantFunctor[StringEncoder] = ???


  //THESE TWO INSTANCES ARE NOT POSSIBLE TO WRITE but looking at them can help form a better intuition about this. So if you're still a bit shaky try to implement them and see if you understand why it doesn't work
  implicit def predFunctor:Functor[Predicate] = ???

  implicit def optContravar:ContravariantFunctor[Option] = ???
}
