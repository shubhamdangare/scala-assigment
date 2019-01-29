package com.knoldus.queue

import scala.collection.mutable.ArrayBuffer

object queue extends App {

  val queOne = new DoubleQueue
  queOne.enqueue(10)
  queOne.enqueue(20)
  queOne.enqueue(30)
  println(queOne.dequeue)
  println(queOne.dequeue)
  val queSecond  = new SquareQueue
  queSecond.enqueue(40)
  queSecond.enqueue(50)
  println(queSecond.dequeue)

}
trait Queue{
  def enqueue(num:Int)
  def dequeue():Int
}

class DoubleQueue extends Queue{
  val stack  = new ArrayBuffer[Int](10)
  def enqueue(num:Int):Unit = stack += (num+num)

  def dequeue(): Int = stack.remove(0)
}

class SquareQueue extends Queue{
  val stack  = new ArrayBuffer[Int](10)
  override def enqueue(num:Int):Unit = {
    stack += num*2
  }
  override def dequeue():Int = stack.remove(0)
}
