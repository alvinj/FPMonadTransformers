package monads

case class State[S, A](run: S => (S,A)) {

    // this code is like: `xs.filter { i : Int => i < 3 }`
    def flatMap[B](g: A => State[S, B]): State[S, B] = State { (s0: S) =>

        // create a new (state,value) by applying `run` to the initial state, `s0`.
        // as shown above, `run` transforms an S to an (S,A).
        val (s1, a) = run(s0)   //(S,A) <~~ the type `S` here, not a State

        // create a new State by applying `g` to `a`. as shown above,
        // `g` transforms an A to a `State[S,B]`.
        val s2 = g(a)           //State[S,B]

        // create a final result by applying s2.run to s1.
        // once again, `run` transforms an S to an (S,A).
        val rez = s2.run(s1)    //(S,B)

        // yield this value; it is the param that's passed to State's
        // constructor, so this function yields a State[S,B] as its
        // final result
        rez

        //old
        //g(a).run(s1)
    }

    def map[B](f: A => B): State[S, B] = flatMap(a => State.point(f(a)))
}

object State {
    def point[S, A](v: A): State[S, A] = State(run = s => (s, v))
}



