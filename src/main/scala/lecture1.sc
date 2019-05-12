
val listFunc: Functor[List] = Functor.listFunctor

listFunc(List(1,2,3).map(_ + 2))