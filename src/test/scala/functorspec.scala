import org.scalatest.{FlatSpec, Matchers}

class functorspec extends FlatSpec with Matchers{

  "functor" should "be able to map over a list of integers" in {
    val FL  = implicitly[Functor[List]]

    FL.map(List(1,2,3))(_+1) should be (List(2,3,4))
  }

  it should "be able to map over a function that takes Int => String and String => String" in {
    val FUNCL = implicitly[Functor[Int => ?]]

    val fcompose = FUNCL.map(String.valueOf(_))(str => if(str == "5") "five" else "don't know")

    fcompose(5) should be ("five")
    fcompose(6) should be ("don't know")
  }

  it should "compose two functors of List of Options" in {
    val listOptFunc = Functor[List] compose Functor[Option]
    val lo = List(Some(1), None, Some(2))
    listOptFunc.map(lo)(_ + 1) should be (List(Some(2), None, Some(3)))
  }
}
