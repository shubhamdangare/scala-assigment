package com.knoldus

class PrintList{
  val firstNumber  = 10
  val secondNumber = 20
  val thirdNumber  = 30
  val fourthNumber = 40

  val listOfNumbers = List(firstNumber,secondNumber,thirdNumber,fourthNumber)

  def Print():Unit = {
    for(i <- listOfNumbers.indices){
      println( i + " = " + listOfNumbers(i))
    }
  }
}



/////////////////////////////////////////////////////////////////////////////////////////////////////////////////


class Maximum{

  def findMax(xs: List[Int]): Int = {
    require(xs.nonEmpty, "list must not be empty")
    val head = xs.head
    val tail = xs.tail
    if (tail.isEmpty)
      head
    else {
      val large = findMax(tail)
      if (head >= large)
        head
      else
        large
    }
  }
}



/////////////////////////////////////////////////////////////////////////////////////////////////////////////////



class SumAndProductOfDigit{

   private def getFactorial(num:Int):Int = {
    if(num <= 1)
      1
    else
      num * getFactorial(num-1)
  }

  private def getSumOfDigit(num:Int):Int = num match{
    case 0 => 0
    case _ => num % 10 + getSumOfDigit(num / 10)
  }

  def printResult(num:Int):Unit= {
    println(getSumOfDigit(getFactorial(num)))

  }
}



/////////////////////////////////////////////////////////////////////////////////////////////////////////////////


class Bank(name:String,balance:Long){

  val totalBalance:Long = balance
  def deposit(Amt:Long):Bank = {
    new Bank(name, Amt + totalBalance)
  }
  def withdraw(Amt:Long):Bank = {
    new Bank(name,Amt - balance)
  }

  def print  = println(balance)
}



/////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  class Series{

    def fibonacciSeries(index: Int): Int = {
      if (index <= 0)
        0
      else {

        if (index == 1)
          1
        else
          fibonacciSeries(index - 1) + fibonacciSeries(index - 2)
      }
    }

    def print():Unit = {

      for(i <- 0 to 7)
        println(fibonacciSeries(i))
    }

  }


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  class Shape{

    def area(shapes:String,first:Int,second:Int,f:(Int,Int) => Int):Int = {

      val areaOfShape:String = shapes
      areaOfShape match {
        case "rectangle" => f(first,second)
        case _ => -1
      }
    }

    def getRectArea(length: Int, width:Int):Int={
          length * width
    }

    def print():Unit = {
      println( area("rectangle",2,3,getRectArea))
    }
  }



/////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  class SumDiffShape{

    def sum(f:(Int,Int)=>Int,numberOne:Int,numberSecond:Int):Int = {
      f(numberOne,numberSecond)
    }

    def square(firstNumber:Int, secondNumber:Int):Int= {
      firstNumber*secondNumber  + firstNumber * secondNumber
    }

    def additionOfNumber(firstNumber:Int,secondNumber:Int):Int = {
          firstNumber  + secondNumber
    }

    def print():Unit= {
      println(sum(square, 5, 4))
    }

  }




/////////////////////////////////////////////////////////////////////////////////////////////////////////////////


object MainClass extends App{

  // print elemet from list
  // question 2 maximum elemets of list
  // println(max(List(2,4,9,3,4)))
  // question 3 print sum of digit of product of number
/*
  question3(5)
  def question3(num:Integer) ={
      var sum:Long = 1
      var value:Integer = num
        while(value > 0 ){
            sum *= value
            value =  value - 1
        }
      var total:Long = 0
      while(sum!=0){
         total += sum % 10
        sum = sum/10
      }
     total
  }
   println(question3(5))
*/


  val customer = new Bank("shubham",5000)
  println(customer.totalBalance)
  val customerAmt =  customer.deposit(1000)
  println(customerAmt.totalBalance)

  val firstPerson = new Person("he",25)
  val secondPerson = new Person("h",25)
  firstPerson < secondPerson


  val name =  FirstName("shubham")
  val lastName =  LastName("Dangare")
  val age =  Age(22)


  def displayDetails(firstName:FirstName,lastName:LastName,age:Age) = {
     firstName.name + " " + lastName.name + " is of " + age.age
  }

  println(displayDetails(name,lastName,age))

}


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////


trait Ordered[Person]{

  def <(that:Person)

}

class Person(val name:String,val age:Int) extends Ordered[Person]{

  override def <(that:Person) = {
    if(this.name == that.name) {
      if (this.age == that.age)
        println("true")
    }

    else {
      if (this.name != that.name) {
        if (this.name.length == that.name.length)
          println("true")
        else
          println("false")
      }
    }
  }
}

case class FirstName(name:String) extends  AnyVal
case class LastName(name:String) extends  AnyVal
case class Age(age:Int) extends  AnyVal
