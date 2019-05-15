import org.scalatest.{FlatSpec, Matchers}

class applicativespec extends FlatSpec with Matchers{

  "applicative" should "apply a function of doubling on an Option[Int]" in {
    Applicative[Option].map(Some(2))(_ *2) should be (Some(4))
  }

  it should "return None as result while applying a function on None" in {
    Applicative[Option].map(None: Option[Int])(_ * 2) should be (None)
  }

  it should "apply a function to each element of a list" in {
    Applicative[List].map(List(1,2,3))(_ * 2) should be (List(2,4,6))
  }

  it should "apply each function in a list of function to each element of a list" in {
    //TODO
  }

  it should "apply the function on each of the Option value" in {
    Applicative[Option].map2(Some(1), Some(2))(_ + _) should be (Some(3))
  }

  it should "apply the function on each of element of the List" in {
    Applicative[List].map2(List(1,2), List(3,4))(_ + _) should be (List(4,5,5,6))
  }

  it should "return a tuple of Option if passed two Options" in {
    Applicative[Option].tuple2(Some(1), Some(2)) should be (Some((1,2)))
  }

  it should "return none if passed a None in tuple2 function" in {
    Applicative[Option].tuple2(Some(1), None) should be (None)
  }

  /*it should "compose two list of options and apply the function" in {
    val lopt = Applicative[List] compose Applicative[Option]
    lopt.map2(List(Some(1), None, Some(2)), List(Some(3)))(_ + _) should be (List(Some(4), None, Some(5)))
  }*/
}
