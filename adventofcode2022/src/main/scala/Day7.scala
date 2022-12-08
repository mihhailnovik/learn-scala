import scala.collection.mutable
import scala.collection.mutable.Map

class Day7 extends AoCPuzzle {
  override def dayNr: Int = 7
  override def active(): Boolean = false

  private val commands = getData().tail

  case class Node(children: Map[String, Node], parent: Node, size: Int) {
    def gidChild(name: String) = children(name)
    def addChild(name: String, child: Node): Unit = children(name) = child

    def getRoot(): Node = if (parent == null) this else parent.getRoot()

    def print(): Unit = {
      println(getRoot().children.keys)
    }
  }
  val root = Node(mutable.Map.empty[String, Node], null, -1)

  def processCommands(commands: List[String], node: Node): Node = {
    if (commands.isEmpty) node else {
      val command = commands.head
      if (command.startsWith("$")) {
        if (command == "$ ls") {
          processCommands(commands.tail, node) // we do not care ls
        } else
        if (command.startsWith("$ cd")) {
          val destinationDir = command.split(" ")(2)
          if (destinationDir == "..") {
            processCommands(commands.tail, node.parent)
          } else {
            val currentNode = node.gidChild(destinationDir)
            processCommands(commands.tail, currentNode)
          }

        } else {
          println("unknown command "+command)
          processCommands(commands.tail, node) // we do not care ls
        }
      } else  {
        if (command.startsWith("dir")) {
          val dir = command.split(" ")(1)
          println("new dir "+ dir)
          val newDir = Node(mutable.Map.empty[String, Node], node, -1)
          node.addChild(dir, newDir)
          processCommands(commands.tail, node)
        } else {
          println("new File "+ command)
          val fileNode = Node(mutable.Map.empty[String, Node], node, command.split(" ")(0).toInt)
          node.addChild(command.split(" ")(1), fileNode)
          processCommands(commands.tail, node)
        }
      }
    }
  }

  override def part1Answer(): String = {
    val tree = processCommands(commands, root)
    tree.print()
    "???"
  }

  override def part2Answer(): String = "findMarker(14).toString"
}
