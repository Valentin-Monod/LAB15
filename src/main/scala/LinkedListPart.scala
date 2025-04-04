
class Node (var item : String, var next : Node) {
  override def toString: String = s"Item : $item, Next : ${next.item}"
}

class LinkedList () {
  var head : Node = null

  def addToStart(element : String) : Unit = head = new Node(element, head)

  def addToEnd(element:String) : Unit = {
    getLastElement().next = new Node(element, null)
  }

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

  def isPresent(element: String): Boolean = {
    var present: Boolean = false
    if (getSize() != 0) {
      var temp = head
      for (i <- 0 until getSize()) {
        if (temp.item == element) present = true
        temp = temp.next
      }
    }
    present
  }

  def getLastElement(): Node = {
    var last : Node = null
    if (getSize() == 0) return null
    else {
      var temp = head
      for (i <- 0 until getSize()) {
        if (i == getSize()-1) last = temp
        temp = temp.next
      }
    }
    last
  }

  def findElement(element : String) : Node = {
    var first_occurence: Node = null
    var get_first = false
    if (getSize() != 0) {
      var temp = head
      for (i <- 0 until getSize() if !get_first) {
        if (temp.item == element) {
          first_occurence = temp
          get_first = true
        }
        temp = temp.next
      }
    }
    first_occurence
  }

  def swapElements(element1: String, element2: String) : Unit = {
    require(isPresent(element1))
    require(isPresent(element2))
    findElement(element1).item = element2
    findElement(element2).item = element1
  }

  def getPosition(element: String): Int = {
    var result = -1
    var temp = head
    for (i <- 0 until getSize()) {
      if (temp.item == element) result = i
      temp = temp.next
    }
    result
  }

  def removeFirstElement() : Unit = head = head.next

  def removeLastElement() : Unit = {
    require(getSize() != 0)
    if (getSize() == 1) head = null
    else {
      var temp = head
      var removed = false
      for (i <- 0 until getSize() if !removed) {
        if (i == getSize()-2) {
          temp.next = null
          removed = true
        }
        temp = temp.next
      }
    }

  }

  def removeElement(element : String) : Unit = {
    require(isPresent(element))

    if (getPosition(element) == getSize()-1) removeLastElement()
    else if (getPosition(element) == 0) removeFirstElement()
    else {
      var current = head
      var previous : Node = null
      for (i <- 0 until getSize()-1) {
        if (i == getPosition(element)) previous.next = current.next
        previous = current
        current = current.next
      }
    }

  }

  def clear() : Unit = head = null

  def insertAfter(before: String, elementToInsert: String): Unit = {
    require(isPresent(before))

    if (getPosition(before) == getSize()-1) addToEnd(elementToInsert)
    else if (getPosition(before) == 0) {
      var temp = head
      removeFirstElement()
      addToStart(elementToInsert)
      addToStart(temp.item)
    }
    else {
      var current = head
      var after = current.next
      for (i <- 0 until getSize()-1) {
        if (i == getPosition(before)) current.next = new Node(elementToInsert, after)
        after = after.next
        current = current.next
      }
    }

  }

  override def toString(): String = {
    var result = s"List content : (size ${getSize()}) : "
    if (head == null) result += "null"
    else {
      var temp = head
      result += s"${temp.item} "
      while (temp.next != null) {
        temp = temp.next
        result +=  s"-> ${temp.item} "
      }
      result += "-> null"
    }
    result
  }
}

object LinkedListPart extends App{

  var flightList: LinkedList = new LinkedList()

}
