package v3_loop_with_quit

import monads.{IO, Monad, StateT}

object LoopWithQuitNoDebug extends App {

    // the `IO` functions i used previously
    def getLine(): IO[String] = IO(scala.io.StdIn.readLine())
    def putStr(s: String): IO[Unit] = IO(print(s))

    def toInt(s: String): Int = {
        try {
            s.toInt
        } catch {
            case e: NumberFormatException => 0
        }
    }

    // a class to track the sum of the ints that are given
    case class SumState(sum: Int)

    // an implementation of the `Monad` trait for the `IO` type.
    implicit val IOMonad = new Monad[IO] {
        def lift[A](a: => A): IO[A] = {
            IO(a)
        }
        def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] = ma.flatMap(f)
    }

    /**
      * given the int `i`, add it to the previous `sum` from the given SumState `s`;
      * then return a new state `newState`, created with the new sum;
      * at the end of the function, wrap `newState` in an `IO`;
      * the anonymous function creates a `StateT` wrapped around that `IO`.
      */
    def doSumWithStateT(newValue: Int): StateT[IO, SumState, Int] = StateT { (oldState: SumState) =>

        // create a new sum from `i` and the previous sum from `s`
        val newSum = newValue + oldState.sum

        // create a new SumState
        val newState: SumState = oldState.copy(sum = newSum)

        // return the new state and the new sum, wrapped in an IO
        IO(newState, newSum)
    }

    /**
      * the purpose of this function is to “lift” an IO action into the StateT monad.
      * given an IO instance named `io` as input, the anonymous function transforms
      * the `IO[A]` into an `IO[(SumState, A)]`;
      * that result is then wrapped in a `StateT`.
      */
    def liftIoIntoStateT[A](io: IO[A]): StateT[IO, SumState, A] = StateT { s =>
        io.map(a => (s, a))  // yields the type`[IO(SumState, A)]`
    }

    // new versions of the i/o functions that uses StateT
    def getLineAsStateT():         StateT[IO, SumState, String] = liftIoIntoStateT(getLine)
    def putStrAsStateT(s: String): StateT[IO, SumState, Unit]   = liftIoIntoStateT(putStr(s))

    /**
      * this loop stops when you type 'q' at the command line
      */
    def sumLoop: StateT[IO, SumState, Unit] = for {
        _     <- putStrAsStateT("\ngive me an int, or 'q' to quit: ")
        input <- getLineAsStateT
        _     <- if (input == "q") {
                     liftIoIntoStateT(IO(Unit))
                 } else for {
                     i <- liftIoIntoStateT(IO(toInt(input)))
                     _ <- doSumWithStateT(i)
                     _ <- sumLoop
                 } yield Unit
    } yield Unit

    val result = sumLoop.run(SumState(0)).run
    println(s"Final SumState: ${result}")

}
