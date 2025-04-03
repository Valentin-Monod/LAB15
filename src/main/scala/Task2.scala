object Task2 extends App {

  var names = List("Adam", "Paul", "Manou", "Gonçalo")
  println(names.mkString(","))

  if (names.contains("Paul")) println("Paul is in the seq")

  var new_names = names.filterNot(_ == "Paul")
  println(new_names.mkString(","))

}
