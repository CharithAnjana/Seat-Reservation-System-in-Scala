object seat extends App {

  //to make seat list
  def makelist():Array[Array[String]] = {
    var seatlist = Array ofDim[String](4,5)
    for(i <- 0 to 3 ; j <- 0 to 4)
    {
      seatlist(i)(j)  = "0000"
    }
    printseatlist(seatlist)
    return seatlist
  }


  //to display seat list
  def printseatlist(arr: Array[Array[String]]){
    println("\n\tSeat List\n")
    var x = 1
    print("    A B C D E\n")
    for(it <- arr)
    {
      print(x)
      print(" - ")
      for(i <- it)
      {
        print(i(0))
        print(" ")
      }
      x = x+1
       print("\n")
    }
    print("\n")
    print("Airports { 1 -> Colombo, 2 -> India, 3 -> Dubai, 4 -> USA }\n")
    print("\n")
  }


  //to get relevent column number
  def getCnum(c: String):Int = c match{
    case "A" => 1
    case "B" => 2
    case "C" => 3
    case "D" => 4
    case "E" => 5
    case _ => -1
  }


//to reserve the seat
def reserveseat(seatlist: Array[Array[String]], details:Array[Int]){
  
  var s = seatlist(details(0))(details(1))
  if(s(0) == '0')
  {
    var d:Array[String] = Array("x", "0", "0", "0")
    for(x <- details(2) to details(3)-1)
    {
      d(x) = "1"
    }
    var seat:String = d(0) + d(1) + d(2) + d(3)
    seatlist(details(0))(details(1)) = seat
    println(seat)
    println("Your seat reserved Successfuly..!")
  }
  
  else if( s(0) == 'x' && ( s(1) == '0' || s(2) == '0' || s(3) == '0') )
  {
    var ch:Array[String] = Array("x", "0", "0", "0")
    for(x <- details(2) to details(3)-1)
    {
      ch(x) = "1"
    }
    var fl = 0
    for(i <- 0 to 3)
    {
      if(ch(i)== "1" && (ch(i) == s(i).toString))
      {
        fl = 1
      }
    }
    if(fl == 0)
    {
      for(i <- 1 to 3)
      {
        if(s(i) == '1')
        {
          ch(i) = s(i).toString
        }
      }
      var seat:String = ch(0) + ch(1) + ch(2) + ch(3)
      seatlist(details(0))(details(1)) = seat
      println(seat)
      println("Your seat reserved Successfuly..!\n")
    }
    else
    {
      println(s)
      println("Seat is already reserverd..!\n")
    }
  }
  else
  {
    println(s)
    println("Seat is already reserverd..!\n")
  }
}


//to get seat number
def getseat(seatlist: Array[Array[String]]){
  var details = Array(-1, -1, -1, -1)
  //to get seat numbers
  print("\nenter row number : ")
  var rn = scala.io.StdIn.readInt()
  print("enter column Letter : ")
  var c = scala.io.StdIn.readLine()

  //to get destination
  print("enter number for the starting Airport : ")
  var s = scala.io.StdIn.readInt()
  print("enter number for the destination Airport : ")
  var d = scala.io.StdIn.readInt()

  var cn = getCnum(c)
  if(cn == -1 || rn<1 || rn>4)
  {
    println("Invalid Seat Number\n")
  }
  else
  {
    if(s<1 || s>4 || d<1 || d>4)
    {
      println("Invalid Airport\n")
    }
    else
    {
      details(0) = rn-1
      details(1) = cn-1
      details(2) = s
      details(3) = d

      reserveseat(seatlist,details)
    }
  }
}




  var seatlist: Array[Array[String]] = makelist()
  var stloop = false
  while(!stloop)
  {
    println("\n------Welcome!------")
    println("1. Reserve a seat")
    println("2. Get the seat list")
    println("3. exit")
    print("Select the Number: ")

    var op = scala.io.StdIn.readInt()

    op match{
      case 1 => getseat(seatlist)
      case 2 => printseatlist(seatlist)
      case 3 => stloop = true
      case _ => println("Invalid Number..")
    }
  }


}
