package com.learning.myAttempts.S1E1

import scala.annotation.tailrec

/**
  * Created by Akshay Mhetre on 3/6/2017.
  */
object Practical1 extends App{
  println(SumOfNum.sumL(1,3))
}

object SumOfNum {
  //Loop : Traditional way
  def sumL(a: Int, b:Int) = {
    if(a > b) 0
    else {
      var sum = 0
      var _a = a
      while(_a <= b) { //for(i <- a to b){
        sum += _a  // sum += i
        _a += 1
      }
      sum
    }
  }

  //Recursive way
  def sumR(a: Int, b:Int):Int = if(a > b) 0 else a + sumR(a+1,b)

  def sumTR(a: Int, b:Int) = {
    //Tail Recursive way
    @tailrec
    def _sumTR(a: Int, b:Int, acc: Int):Int = {
      if(a > b) acc
      else _sumTR(a+1, b, acc+a)
    }
    _sumTR(a, b, 0)
  }


  def cube(a:Int) = a*a*a
  def sumCubes(a: Int, b:Int):Int =  if(a > b) 0 else cube(a) + sumCubes(a+1,b)

  def fact(a:Int):Int =if(a < 2) 1 else a*fact(a-1)
  def sumFact(a: Int, b:Int):Int =  if(a > b) 0 else fact(a) + sumFact(a+1,b)

}


object SumOfNumWithFun {

  def sumR(fun: Int => Int, a: Int, b:Int):Int = if(a > b) 0 else fun(a) + sumR(fun, a+1,b)

  def cube(a:Int) = a*a*a
  def sumCubes(a: Int, b:Int) =  sumR(cube, a, b)

  def fact(a:Int):Int =if(a < 2) 1 else a*fact(a-1)
  def sumFact(a: Int, b:Int):Int =  sumR(fact, a, b)

}
