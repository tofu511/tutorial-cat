package com.danielasfregola.tutorial.cat.functor

import com.danielasfregola.tutorial.cat
import com.danielasfregola.tutorial.cat._

// See solution at https://gist.github.com/DanielaSfregola/ddf48f6c5638f6284b563798c55d5ebd

object FunctorInstances {

  implicit val maybeFunctor: Functor[Maybe] = new Functor[Maybe] {
    override def map[A, B](boxA: Maybe[A])(f: A => B): Maybe[B] = boxA match {
      case Empty => Empty
      case Just(a) => Just(f(a))
    }
  }

  implicit val zeroOrMoreFunctor: Functor[ZeroOrMore] = new Functor[ZeroOrMore] {
    override def map[A, B](boxA: ZeroOrMore[A])(f: A => B): ZeroOrMore[B] = boxA match {
      case Zero => Zero
      case OneOrMore(head, tail) => OneOrMore(f(head), map(tail)(f))
    }
  }

}
