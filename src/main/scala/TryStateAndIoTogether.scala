import monads.{IO, State}

object TryStateAndIoTogether extends App {

    /**
      * State code
      */
    type Stack = List[String]

    def push(x: String): State[Stack, Unit] = State[Stack, Unit] {
        xs => (x :: xs, ())
    }

    /**
      * `IO` functions
      */
    def getLine: IO[String] = IO(scala.io.StdIn.readLine())
    def putStrLn(s: String): IO[Unit] = IO(println(s))

    /**
      * main loop: prompt a user for some input, then push that input
      * onto a stack. you'll get errors when you try to run this
      * code, so it's commented-out.
      */
//    val res = for {
//        _     <- putStrLn("Type anything:")
//        input <- getLine
//        _     <- push(input)
//        _     <- putStrLn(s"Input: $input")
//    } yield ()

}





