
import scala.concurrent.duration.TimeUnit

type Par[A] = ExecutorService => Future[A]

object Par {
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => unit(f(a(es).get, b(es).get))(es)
    // 위 처럼 구현하면 아래의 구현과 달라지는지
//    (es: ExecutorService) => {
//      val as = a(es)
//      val bs = b(es)
//      UnitFuture(f(as.get, bs.get))
//    }


  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  // exercise 7.3(hard)
  def map2_[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???

  // exercise 7.4
  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = {
    map(parList)(_.sorted)
  }

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit())((a, _) => f(a))

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }
  // exercise 7.5(hard)
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
  es => {
    unit(ps.map(_(es).get))(es)
  }
}

class ExecutorService {
  def submit[A](a: Callable[A]): Future[A] = ???
}
trait Callable[A] { def call: A }
trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean
}


// #2
def sum(ints: IndexedSeq[Int]): Par[Int] =
  if(ints.size <= 1)
    Par.unit(ints.headOption getOrElse 0)
  else {
    val (l, r) = ints.splitAt(ints.length/2)
    Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
  }
println("fin.")
