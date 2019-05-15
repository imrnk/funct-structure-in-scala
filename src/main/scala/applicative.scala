import simulacrum._

@typeclass trait Applicative[F[_]] extends Functor[F]{ //self =>

  def pure[A](a : A) : F[A]

  def apply[A,B](fa : F[A])(ff : F[A => B]) : F[B]

  def apply2[A,B,Z](fa : F[A], fb : F[B])(ff : F[(A,B) => Z]) : F[Z] =
    apply(fa)(apply(fb)(map(ff)(f => b => a => f(a,b))))

  override def map[A,B](fa : F[A])(f : A => B) : F[B] = apply(fa)(pure(f))

  def map2[A,B,Z](fa : F[A], fb : F[B])(f :(A,B) => Z) : F[Z] =
    apply(fa)(map(fb)(b => f(_, b)))  //f(_, b) is a partial function of type A => Z,
                                      // passed to apply so it becomes F[A => Z]

  def map3[A,B,C,Z] (fa : F[A], fb : F[B], fc : F[C])(f : (A,B,C) => Z) : F[Z] =
    apply(fa)(map2(fb,fc)((b,c) => a => f(a,b,c))) // a => f(a,b,c) is a function of type A => Z

  def tuple2[A,B](fa : F[A], fb : F[B]) : F[(A,B)] =
    map2(fa,fb)((a,b) => (a,b))

  def map4[A,B,C,D,Z] (fa : F[A], fb : F[B], fc : F[C], fd : F[D])(f : (A,B,C,D) => Z) : F[Z] =
    map2(tuple2(fa,fb), tuple2(fc,fd)){ case ((a,b), (c,d)) => f(a,b,c,d) }

  // This function is not compiling
  // def flip[A,B](ff : F[A => B]) : F[A] => F[B] = apply(_)(ff)

  /*def compose[G[_]](implicit G : Applicative[G]) : Applicative[Lambda[X => F[G[X]]]] =
    new Applicative[Lambda[X => F[G[X]]]] {
       def pure[A](a: A): F[G[A]] = self.pure(G.pure(a))

      override def apply[A, B](fga: F[G[A]])(ff: F[G[A => B]]): F[G[B]] = {
        val x : F[G[A] => G[B]] = self.map(ff)(gab => G.flip(gab))
        self.apply(fga)(x)
      }
    }*/
}

trait ApplicativeLaws[F[_]] {

  //import Applicative.ops

  implicit def F : Applicative[F]

  //If F[A] is applied with lifted identity ( A => A) to F[A => A] then we get back original F[A]
  def applicativeIdentity[A](fa : F[A]) = F.apply(fa)(F.pure((a:A) => a)) == fa

  //lift A to F[A] and then lift function A => B to F[A =>B] and apply it
  //is equivalent to apply the function A => B and then lift to F[B]
  def applicativeHomomorphism[A,B](a :A, f : A => B) = F.apply(F.pure(a))(F.pure(f)) == F.pure(f(a))


  def applicativeInterchange[A,B](a : A, ff : F[A => B]) = F.apply(F.pure(a))(ff) == F.apply(ff)(F.pure((f: A => B) => f(a)))

  //mapping a F[A] over a A => B is equivalent to apply on F[A] with lifting the function A => B to F[A => B]
  def appliativeMap[A,B](fa : F[A], f : A => B) = F.map(fa)(f) == F.apply(fa)(F.pure(f))
}

object ApplicativeLaws {
  def apply[F[_]](implicit F0 : Applicative[F]): ApplicativeLaws[F] = new ApplicativeLaws[F]{
    def F = F0
  }
}

object Applicative {

  implicit val optionApplicative = new Applicative[Option] {
    override def pure[A](a: A): Option[A] = Some(a)

    override def apply[A, B](fa: Option[A])(ff: Option[A => B]): Option[B] = (fa, ff) match {
      case (None, _) => None
      case (Some(a), Some(f)) => Some(f(a))
      case (_, None) => None
    }
  }

  implicit val listApplicative = new Applicative[List] {
    override def pure[A](a: A): List[A] = List(a)

    override def apply[A, B](fa: List[A])(ff: List[A => B]): List[B] =
      fa flatMap {a => ff.map {_(a)}}
      //(fa zip ff) map {case (a, f) => f(a)}
  }
}