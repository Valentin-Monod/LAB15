
class Node (var item : String, var next : Node) {
  override def toString: String = s"Item : $item, Next : ${next.item}"
}

class LinkedList () {
  var head : Node = null

  def clear() : Unit = head = null

  def addToStart(element : String) : Unit = head = new Node(element, head)

  def addToEnd(element:String) : Unit = {
    if (head == null) head = new Node(element, head)
    else getLastElement().next = new Node(element, null)
  }

  def getSize(): Int = {

    // Previous version without recursivity
    //    var result : Int = 0
    //    if (head != null)  {
    //      var temp = head
    //      result += 1
    //      while (temp.next != null) {
    //        result += 1
    //        temp = temp.next
    //      }
    //    }
    //    result

    def sizeHelper(current: Node): Int = {
      if (current == null) 0
      else 1 + sizeHelper(current.next)
    }
    sizeHelper(head)

  }

  def isPresent(element: String): Boolean = {

    // Previous version without recursivity
    //    var present: Boolean = false
    //    if (getSize() != 0) {
    //      var temp = head
    //      for (i <- 0 until getSize()) {
    //        if (temp.item == element) present = true
    //        temp = temp.next
    //      }
    //    }
    //    present

    def isPresentHelper(current : Node) : Boolean = {
      if (current == null) return false
      else if(current.item == element) return true
      else isPresentHelper(current.next)
    }
    isPresentHelper(head)

  }

  def getLastElement(): Node = {

    // Previous version without recursivity
    //      var last : Node = null
    //      if (getSize() == 0) return null
    //      else {
    //        var temp = head
    //        for (i <- 0 until getSize()) {
    //          if (i == getSize()-1) last = temp
    //          temp = temp.next
    //        }
    //      }
    //      last

    def getLastHelper (current : Node) : Node = {
      if (current == null) return current
      else if (current.next == null) return current
      else getLastHelper(current.next)
    }
    getLastHelper(head)

  }

  def findElement(element: String): Node = {

    // Previous version without recursivity
    //        var first_occurence: Node = null
    //        var get_first = false
    //        if (getSize() != 0) {
    //          var temp = head
    //          for (i <- 0 until getSize() if !get_first) {
    //            if (temp.item == element) {
    //              first_occurence = temp
    //              get_first = true
    //            }
    //            temp = temp.next
    //          }
    //        }
    //        first_occurence

    def findElementHelper(current: Node): Node = {
      if (current == null) return current
      else if (current.item == element) return current
      else findElementHelper(current.next)
    }
    findElementHelper(head)

  }

  def change_item(position : Int, item : String) : Unit = {

    // Previous version without recursivity
    //    var temp = head
    //    for (i <- 0 until getSize()) {
    //      if (i == position) temp.item = item
    //      temp = temp.next
    //    }

    def changeItemHelper (current : Node, pos : Int = 0) : Unit = {
      if (current == null) return
      else if (pos == position) current.item = item
      else changeItemHelper(current.next, pos + 1)
    }
    changeItemHelper(head)

  }

  def swapElements(element1: String, element2: String) : Unit = {
    var pos1 = getPosition(element1)
    var pos2 = getPosition(element2)
    change_item(pos1, element2)
    change_item(pos2, element1)
  }

  def getPosition(element: String): Int = {

    // Previous version without recursivity
    //    var result = -1
    //    var temp = head
    //    for (i <- 0 until getSize()) {
    //      if (temp.item == element) result = i
    //      temp = temp.next
    //    }
    //    result

    def getPositionHelper(current: Node, index: Int = 0): Int = {
      if (current == null) -1
      else if (current.item == element) index
      else getPositionHelper(current.next, index + 1)
    }
    getPositionHelper(head)

  }

  def removeFirstElement() : Unit = {
    if (head != null) head = head.next
  }

  def removeLastElement() : Unit = {

    // Previous version without recursivity
    //    if (getSize() <= 1) head = null
    //    else {
    //      var temp = head
    //      var removed = false
    //      for (i <- 0 until getSize() if !removed) {
    //        if (i == getSize()-2) {
    //          temp.next = null
    //          removed = true
    //        }
    //        temp = temp.next
    //      }
    //    }

   def removeLastElementHelper (current : Node, size : Int = getSize()) : Unit = {
     if(size <=1) clear()
     else if (size == 2) current.next = null
     else removeLastElementHelper(current.next, size - 1)
   }
    removeLastElementHelper(head)

  }

  def removeElement(element: String): Unit = {

    // Previous version without recursivity
    //        if (getSize() <= 1) clear()
    //        else if (getPosition(element) == getSize()-1) removeLastElement()
    //        else if (getPosition(element) == 0) removeFirstElement()
    //        else {
    //          var current = head
    //          var previous : Node = null
    //          for (i <- 0 until getSize()-1) {
    //            if (current.item == element) previous.next = current.next
    //            previous = current
    //            current = current.next
    //          }
    //        }

    def removeElementHelper(current: Node, previous: Node = null): Unit = {
      if (current == null) return
      else {
        if (current.item == element) previous.next = current.next
        removeElementHelper(current.next, current)
      }
    }

    if (getSize() <= 1) clear()
    else if (getPosition(element) == 0) removeFirstElement()
    else if (getPosition(element) == getSize() - 1) removeLastElement()
    else removeElementHelper(head)

  }

  def insertAfter(before: String, elementToInsert: String): Unit = {

    // Previous version without recursivity
    //    if (getPosition(before) == getSize()-1) addToEnd(elementToInsert)
    //    else if (getPosition(before) == 0) {
    //      var temp = head
    //      removeFirstElement()
    //      addToStart(elementToInsert)
    //      addToStart(temp.item)
    //    }
    //    else {
    //      var current = head
    //      var after = current.next
    //      for (i <- 0 until getSize()-1) {
    //        if (i == getPosition(before)) current.next = new Node(elementToInsert, after)
    //        after = after.next
    //        current = current.next
    //      }
    //    }

    def insertAfterHelper (current : Node) : Unit = {
      if (current == null) return
      else if (current.item == before) {
        var saved = current.next
        current.next = new Node (elementToInsert, saved)
      }
      else insertAfterHelper(current.next)
    }
    insertAfterHelper(head)

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

  // test what you want here

}
