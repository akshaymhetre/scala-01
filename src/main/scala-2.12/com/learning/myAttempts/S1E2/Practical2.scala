package com.learning.myAttempts.S1E2

import scala.collection.mutable

/**
  * Created by Akshay Mhetre on 3/15/2017.
  */
object SumOfNumWithShortFun {

  def sumR(fun: Int => Int, a: Int, b:Int):Int =
    if(a > b) 0 else fun(a) + sumR(fun, a+1,b)

  def prodR(fun: Int => Int, a: Int, b:Int):Int =
    if(a > b) 1 else fun(a) * prodR(fun, a+1,b)

  def mapReduce(a: Int, b: Int,
                map: Int=>Int,
                reduce: (Int, Int) => Int,
                pivot: Int):Int = if(a> b) pivot
                              else
                              reduce(map(a),mapReduce(a+1,b, map, reduce, pivot))


  def sumCubes(a: Int, b:Int) =  sumR(a => a*a*a, a, b)
  def sumIds(a: Int, b: Int) = sumR(a => a, a, b)

  def fact(a:Int):Int =if(a < 2) 1 else a*fact(a-1)
  def sumFact(a: Int, b:Int):Int =  sumR(fact, a, b)

  /*PritnMessage function
* */
  val myUpperCase: String =>String = str => {
    println("Inside my uppercase function")
    str toUpperCase
  }

  def printWelcomeMsg1(str1: String, str2: String) = println(myUpperCase(str1)+" "+str2)
  val withHello1: (String) => Unit = printWelcomeMsg1("hello", _)


  def printWelcomeMsg2(str1: String)(str2: String) = println(myUpperCase(str1)+" "+str2)
  val withHello2: (String) => Unit = printWelcomeMsg2("hello")

  val add: (Int, Int) => Int = (a, b) => a+b
  def addDef(a: Int)(b: Int) = a+b
  val addC = (a:Int) => ((b:Int) => a+b)
  val addThree = (a:Int) => (b:Int) => (c: Int) => a+b+c

  val printWelcomeMsg3: (String) => (String) => Unit = (str1: String) => {
      val upper = myUpperCase(str1) // Heavy computation
    (str2: String) => println(upper+ " "+ str2)
  }

  val withHello3 : String => Unit = printWelcomeMsg3("hello")



}


object Practical2 extends App{
  //println(SumOfNumWithShortFun.mapReduce(1, 10, a=> a*a*a, (a,b) => a+b, 0))
  //println(SumOfNumWithShortFun.sumCubes(1, 10))

  //SumOfNumWithShortFun.withHello3("akshay")
  //SumOfNumWithShortFun.withHello3("gautam")
  import Combinators._

  println(mapInt(List(1,2,3,4), x => x*2))
  println(mapG(List(1,2,3,4), (x:Int) => x*2))
  println(mapFL(List(1,2,3,4))(_*2))
}

object Combinators{

  def mapInt(lst: List[Int], f : Int => Int): List[Int] = {
    val buf = mutable.Buffer.empty[Int]
    val it = lst.iterator
    while(it.hasNext){
      val a = it.next()
      buf.append(f(a))
    }
    buf.toList
  }

  def mapG[A, B](lst: List[A], f : A => B): List[B] = {
    val buf = mutable.Buffer.empty[B]
    val it = lst.iterator
    while(it.hasNext){
      val a = it.next()
      buf.append(f(a))
    }
    buf.toList
  }

  def mapGC[A, B](lst: List[A])(f : A => B): List[B] = {
    val buf = mutable.Buffer.empty[B]
    val it = lst.iterator
    while(it.hasNext){
      val a = it.next()
      buf.append(f(a))
    }
    buf.toList
  }


  def mapL[A, B](lst: List[A])(f : A => B): List[B] = {
    var buf = List.empty[B]
    val it = lst.iterator
    while(it.hasNext){
      val a = it.next()
      buf = f(a) :: buf
    }
    buf.reverse
  }

  def mapRL[A, B](lst: List[A])(f : A => B): List[B] = {
      if(lst.isEmpty) List.empty[B]
      else {
        f(lst.head) :: mapRL(lst.tail)(f)
      }
  }

  def mapFL[A, B](lst: List[A])(f : A => B): List[B] = {
    lst.foldLeft(List.empty[B])((newLst, ele) => f(ele) :: newLst).reverse
  }


}