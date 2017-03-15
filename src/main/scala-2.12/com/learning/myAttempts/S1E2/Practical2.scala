package com.learning.myAttempts.S1E2

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
}


object Practical2 extends App{
  println(SumOfNumWithShortFun.mapReduce(1, 10, a=> a*a*a, (a,b) => a+b, 0))
  println(SumOfNumWithShortFun.sumCubes(1, 10))
}
