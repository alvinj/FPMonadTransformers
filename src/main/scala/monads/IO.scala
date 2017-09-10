package monads

class IO[A] private (constructorCodeBlock: => A) {

    def run = constructorCodeBlock

    def flatMapOrig[B](f: A => IO[B]): IO[B] = IO(f(run).run)

    def flatMap[B](customFmapAlgorithm: A => IO[B]): IO[B] = {
        val res1: IO[B] = customFmapAlgorithm(run)
        val res2: B = res1.run
        IO(res2)
    }

    def map[B](f: A => B): IO[B] = flatMap(a => IO(f(a)))

}

object IO {
    def apply[A](a: => A): IO[A] = new IO(a)
}

