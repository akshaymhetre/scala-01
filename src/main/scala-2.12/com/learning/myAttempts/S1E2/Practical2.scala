package com.learning.myAttempts.S1E2

/**
  * Created by Akshay Mhetre on 3/15/2017.
  */
object SumOfNumWithShortFun {

  def sumR(fun: Int => Int, a: Int, b:Int):Int = if(a > b) 0 else fun(a) + sumR(fun, a+1,b)

  def sumCubes(a: Int, b:Int) =  sumR(a => a*a*a, a, b)
  def sumIds(a: Int, b: Int) = sumR(a => a, a, b)

  def fact(a:Int):Int =if(a < 2) 1 else a*fact(a-1)
  def sumFact(a: Int, b:Int):Int =  sumR(fact, a, b)
}


object Practical2 {

}
