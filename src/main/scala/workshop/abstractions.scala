package workshop

import workshop.typeclasses._
import workshop.model._
import simulacrum.typeclass
import scala.concurrent.Future
import abstractions.Monoidal.ops._
import typeclasses.Monoid.ops._
import abstractions.Traverse.ops._
import scala.concurrent.ExecutionContext.Implicits.global
import adts.Iso

object abstractions {


  //Multiplicative Monoidal Functors

  @typeclass trait Monoidal[F[_]] extends Functor[F] { self =>
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]

    def unit: F[Unit]

    //technically pure is NOT a member of Monoidal but it's just way to handy to have. Pure is a member of the formulation through applicative but as you can see we can express it easily enough
    def pure[A](a: A): F[A] = map(unit)(_ => a)

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      map(product(fa, fb)) { case (a, b) => f(a, b) }

    def compose[G[_]:Monoidal] : Monoidal[λ[X => F[G[X]]]] = new Monoidal[λ[X => F[G[X]]]]{
      def unit: F[G[Unit]] = self.pure(Monoidal[G].unit)
      def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] = self.map(fa)(Monoidal[G].fmap(f))
      def product[A, B](fa: F[G[A]], fb: F[G[B]]): F[G[(A, B)]] = self.map2(fa, fb)(Monoidal[G].product)
    }
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

  implicit def optionMonoidal: Monoidal[Option] = new Monoidal[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

    def unit: Option[Unit] = Some(())

    def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = (fa, fb) match {
      case (Some(a), Some(b)) => Some((a,b))
      case _ => None
    }
  }

  implicit def futureMonoidal: Monoidal[Future] = new Monoidal[Future] {
    def unit: Future[Unit] = Future.successful(())
    def product[A, B](fa: Future[A], fb: Future[B]): Future[(A, B)] = fa.zip(fb)
    def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
  }

  implicit def listMonoidal: Monoidal[List] = new Monoidal[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = Functor[List].map(fa)(f)

    def unit: List[Unit] = List(())

    def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa match {
      case a::as => fb.map(b => (a,b)) ::: product(as, fb)
      case Nil => Nil
    }
  }




  def bundle[F[_]: Monoidal, A](x: F[A], y: F[A]): F[List[A]] = Monoidal[F].map2(x,y)( (a1, a2) => List(a1,a2))

  def appendM[F[_]: Monoidal, A](x: F[A], y: F[List[A]]): F[List[A]] = Monoidal[F].map2(x,y)(_ :: _)

  def sequence[F[_]: Monoidal, A](list: List[F[A]]): F[List[A]] = list.foldRight(Monoidal[F].pure(List.empty[A]))(appendM)

  def traverse[F[_]: Monoidal, A, B](list: List[A])(f: A => F[B]): F[List[B]] = list.foldRight(Monoidal[F].pure(List.empty[B]))((a, bs) => appendM(f(a),bs))

  def sequence2[F[_]: Monoidal, A](list: List[F[A]]): F[List[A]] = traverse(list)(identity)

  def ap[F[_]: Monoidal, A, B](ff: F[A => B], fa: F[A]): F[B] = Monoidal[F].map2(ff, fa)(_(_))

  //Given two Option[Int] multiply the int values if they exist or leave them  unchanged if one of them doesn't
  def combineOptions(x: Option[Int], y: Option[Int]): Option[Int] = ???



  //Foldable

  @typeclass trait Foldable[F[_]] {
    //implementations of foldMap should only go through F ONCE
    def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B

    def combineAll[A: Monoid](fa: F[A]): A = foldMap(fa)(identity)
  }

  implicit def optionFoldable: Foldable[Option] = new Foldable[Option] {
    def foldMap[A, B: Monoid](fa: Option[A])(f: A => B): B = fa.fold(Monoid[B].empty)(a => f(a))
  }

  implicit def listFoldable: Foldable[List] = new Foldable[List] {
    def foldMap[A, B: Monoid](fa: List[A])(f: A => B): B = fa.foldRight(Monoid[B].empty)( (a,b) => f(a) |+| b )
  }

  implicit def setFoldable: Foldable[Set] = new Foldable[Set] {
    def foldMap[A, B: Monoid](fa: Set[A])(f: A => B): B = fa.foldRight(Monoid[B].empty)( (a,b) => f(a) |+| b )
  }

  // Turn this foldable into a List
  def fromFoldable[F[_]: Foldable, A](fa: F[A]): List[A] = Foldable[F].foldMap(fa)(a => List(a))

  // Find the first element that matches the predicate
  // Hint: YOu might need to defne a new type with a new monoid


  def find[F[_]: Foldable, A](fa: F[A], f: A => Boolean): Option[A] = Foldable[F].foldMap(fa)(
    a => if (f(a)) Some(a) else None
  )(
    new Monoid[Option[A]]{
      def empty: Option[A] = None
      def combine(x: Option[A], y: Option[A]): Option[A] = x.orElse(y)
    }
  )





  //Traversable
  @typeclass trait Traverse[F[_]]  { self =>
    def traverse[G[_]: Monoidal, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

    def sequence[G[_]: Monoidal, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)

    def compose[G[_]: Traverse]: Traverse[λ[X => F[G[X]]]] = new Traverse[λ[X => F[G[X]]]] {
      def traverse[H[_]: Monoidal, A, B](fa: F[G[A]])(f: A => H[B]): H[F[G[B]]] = self.traverse(fa)(ga => Traverse[G].traverse(ga)(f))
    }
  }

  /*object Traverse{
    def apply[F[_]](implicit instance:Traverse[F]):Traverse[F] = instance
  }

  implicit class TraverseSyntax[F[_]: Traverse, A](fa:F[A]) {
    def traverse[G[_]: Monoidal, B](f: A => G[B]):G[F[B]] = Traverse[F].traverse(fa)(f)
  }

  implicit class StringSyntax(s:String) {
    def appendFoo:String = s + "foo"
  }*/

  implicit def listTraversable: Traverse[List] = new Traverse[List] {
    def traverse[G[_]: Monoidal, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = 
      fa.foldRight(Monoidal[G].pure(List.empty[B]))( (a, gbs) => Monoidal[G].map2(f(a),gbs)(_::_) )
  }

  implicit def optionTraversable: Traverse[Option] = new Traverse[Option] {
    def traverse[G[_]: Monoidal, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = 
      fa.fold(Monoidal[G].pure(Option.empty[B]))(a => f(a).map(Option.apply))
  }

  implicit def eitherTraversable[E]: Traverse[Either[E, ?]] = new Traverse[Either[E, ?]]{
    def traverse[G[_]: Monoidal, A, B](fa: Either[E,A])(f: A => G[B]): G[Either[E,B]] = fa.fold(
      e => Monoidal[G].pure(Left(e)),
      a => f(a).map(Right.apply)
    )
  }



  //Validated

  sealed trait Validated[+E, +A]
  case class Valid[+A](a: A) extends Validated[Nothing, A]
  case class Invalid[+E](e: E) extends Validated[E, Nothing]

  type ValidatedList[+E, +A] = Validated[List[E], A]

  def toEither[E, A](v: Validated[E, A]): Either[E, A] = v match {
    case Valid(a) => Right(a)
    case Invalid(e) => Left(e)
  }

  def toValidated[E, A](e: Either[E, A]): Validated[E, A] = e.fold(Invalid.apply, Valid.apply)

  implicit def validatedMonoidal[E: Monoid]: Monoidal[Validated[E, ?]] = new Monoidal[Validated[E, ?]] {
    def map[A, B](fa: Validated[E,A])(f: A => B): Validated[E,B] = fa match {
      case Valid(a) => Valid(f(a))
      case Invalid(e) => Invalid(e)
    }
    def unit: Validated[E,Unit] = Valid(())

    def product[A, B](fa: Validated[E,A], fb: Validated[E,B]): Validated[E,(A, B)] = (fa, fb) match {
      case (Valid(a), Valid(b)) => Valid((a,b))
      case (Valid(a), Invalid(e2)) => Invalid(e2)
      case (Invalid(e1), Valid(b)) => Invalid(e1)
      case (Invalid(e1), Invalid(e2)) => Invalid(e1 |+| e2)
    }
  }

  implicit def validatedTraversable[E]: Traverse[Validated[E, ?]] = new Traverse[Validated[E, ?]] {
    def traverse[G[_]: Monoidal, A, B](fa: Validated[E,A])(f: A => G[B]): G[Validated[E,B]] = fa match {
      case Invalid(e) => Monoidal[G].pure(Invalid(e))
      case Valid(a) => f(a).map(Valid.apply)
    }
  }


  // Validation exercise
  // Use `Validated` and everything you've learned so far to solve this one
  // In the `model` object you can find two lists of unvalidated strings representing users
  // Your job is to check all of them whether they are valid or not.
  // To do so, you should use the `User.validate` function.
  // Once your done, you can check the difference to Either
  def allUsers(ul:List[String]):ValidatedList[String, List[User]] = ul.traverse(User.validate)





  // Next we want to write a function that takes a String representing a user
  // and return the UserReport for that user using the `User.fetchReport` function
  def reportForUser(u: String): Future[ValidatedList[String, UserReport]] = User.validate(u).traverse(User.fetchReport)




  // Hard: Now get all reports for all the users
  def allReports(ul:List[String]): Future[ValidatedList[String, List[UserReport]]] = ul.traverse(reportForUser).map(_.sequence)










  // Nested Monoidals

  case class Nested[F[_], G[_], A](value: F[G[A]])

  implicit def nestedMonoidal[F[_]: Monoidal, G[_]: Monoidal]: Monoidal[Nested[F, G, ?]] = new Monoidal[Nested[F, G, ?]] {
    val composit = Monoidal[F] compose Monoidal[G]

    def product[A, B](fa: Nested[F,G,A], fb: Nested[F,G,B]): Nested[F,G,(A, B)] = Nested(composit.product(fa.value, fb.value))

    def unit: Nested[F,G,Unit] = Nested(composit.unit)

    def map[A, B](fa: Nested[F,G,A])(f: A => B): Nested[F,G,B] = Nested(composit.map(fa.value)(f))
  }


  // Try implementing `allReports` using `Nested`, it should be much easier this way
  def allReportsUsingNested(ul:List[String]): Future[ValidatedList[String, List[UserReport]]] = ul.traverse( u => Nested(reportForUser(u))).value

  def allReportsUsingTL(ul:List[String]): Future[ValidatedList[String, List[UserReport]]] =
    ul.traverse[
      λ[X => Future[ValidatedList[String, X]]], 
      UserReport
    ](
      reportForUser
    )(
      Monoidal[Future].compose[ValidatedList[String, ?]]
    )






  @typeclass trait ContravariantFunctor[F[_]] {
    def contramap[A, B](fa: F[A])(f: B => A): F[B]
  }

  case class Predicate[A](run: A => Boolean)

  case class StringEncoder[A](run: A => String)

  implicit def predicateContravariant: ContravariantFunctor[Predicate] = new ContravariantFunctor[Predicate] {
    def contramap[A, B](fa: Predicate[A])(f: B => A): Predicate[B] = Predicate( fa.run compose f)
  }

  implicit def stringEncoderContravariant: ContravariantFunctor[StringEncoder] = ???


  //THESE TWO INSTANCES ARE NOT POSSIBLE TO WRITE but looking at them can help form a better intuition about this. So if you're still a bit shaky try to implement them and see if you understand why it doesn't work
  implicit def predFunctor:Functor[Predicate] = ???

  implicit def optContravar:ContravariantFunctor[Option] = ???
}
