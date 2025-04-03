
class Node (var item : String, var next : Node) {

}

class LinkedList () {
  var head : Node = null

  def addToStart(s : String) : Unit = head = new Node(s, head)

  def getSize() : Int = {
    var result : Int = 0
    if (head != null)  {
      var temp = head
      result += 1
      while (temp.next != null) {
        result += 1
        temp = temp.next
      }
    }
    result
  }

  override def toString(): String = {
    var result = s"List content : (size ${getSize()}) : "
    if (head == null) result += "null"
    else {
      var temp = head
      result += s"${temp.item} "
      while (temp.next != null) {
        temp = temp.next
        result +=  s"-> ${temp.item}"
      }
      result += "-> null"
    }
    result
  }
}

object Task3 extends App{

  var flightList: LinkedList = new LinkedList()
  println(flightList)
  flightList.addToStart("Rome")
  println(flightList)
  flightList.addToStart("Paris")
  println(flightList)
  flightList.addToStart("Tokyo")
  println(flightList)

}
