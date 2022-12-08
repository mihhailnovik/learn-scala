import scala.collection.mutable
import scala.collection.mutable.Map

class Day7 extends AoCPuzzle {
  override def dayNr: Int = 7

  override def active(): Boolean = true

  private val commands = getData().tail

  case class Node(id: String, children: Map[String, Node], parent: Node, var size: Int, isDirectory: Boolean) {
    def gidChild(name: String) = children(name)

    def addChild(name: String, child: Node): Unit = children(name) = child

    def getRoot(): Node = if (parent == null) this else parent.getRoot()

    def folders(): List[Node] = if (isDirectory) List(this).appendedAll(children.flatMap(_._2.folders())) else List.empty[Node]
  }

  val root = Node("/", mutable.Map.empty[String, Node], null, 0, true)

  def processCommands(commands: List[String], node: Node): Node = {
    if (commands.isEmpty) node else {
      val command = commands.head
      if (command.startsWith("$")) {
        if (command == "$ ls") {
          processCommands(commands.tail, node) // we do not care ls
        } else if (command.startsWith("$ cd")) {
          val destinationDir = command.split(" ")(2)
          if (destinationDir == "..") {
            processCommands(commands.tail, node.parent)
          } else {
            val currentNode = node.gidChild(destinationDir)
            processCommands(commands.tail, currentNode)
          }

        } else {
          println("unknown command " + command)
          processCommands(commands.tail, node) // we do not care ls
        }
      } else {
        if (command.startsWith("dir")) {
          val dirName = command.split(" ")(1)
          val newDir = Node(dirName, mutable.Map.empty[String, Node], node, -1, true)
          node.addChild(dirName, newDir)
          processCommands(commands.tail, node)
        } else {
          val fileNode = Node(command.split(" ")(1), mutable.Map.empty[String, Node], node, command.split(" ")(0).toInt, false)
          node.addChild(command.split(" ")(1), fileNode)
          processCommands(commands.tail, node)
        }
      }
    }
  }

  def size(node: Node): Int = {
    if (node.isDirectory) {
      node.children.values.map(size).sum
    } else {
      node.size
    }
  }

  def traverse(node: Node, maxSize: Int, acc: List[Int]): List[Int] = {
    val satisfy = node.children.values.map(size).filter(_ < maxSize).toList
    node.children.flatMap(n => traverse(n._2, maxSize, satisfy.appendedAll(acc))).toList
  }


  override def part1Answer(): String = {
    val localRoot = processCommands(commands, root).getRoot()
    localRoot.folders().map(size).filter(_ < 100000).sum.toString
  }

  override def part2Answer(): String = "findMarker(14).toString"
}
