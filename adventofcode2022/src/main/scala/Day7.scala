import scala.collection.mutable
import scala.collection.mutable.Map

class Day7 extends AoCPuzzle {
  override def dayNr: Int = 7

  private val commands = getData().tail
  private val root = RootDirectory("/", mutable.Map.empty[String, Data])

  private def readCommands(commands: List[String], node: Data): Data = {
    if (commands.isEmpty) node else {
      val command = commands.head
      val data = processCommand(command, node)
      readCommands(commands.tail, data)
    }
  }

  private def processCommand(command: String, node: Data): Data = {
    val cd = "\\$ cd (\\D+)".r
    val dir = "dir ([a-z]+)".r
    val file = "([0-9]+) (\\D+)".r
    command match
      case "$ ls" => node // just ignore ls
      case cd("..") => getParent(node)
      case cd(newDir) => child(node, newDir)
      case dir(name) =>
        val directory = Directory(name, mutable.Map.empty[String, Data], node)
        addChild(node, directory)
        node
      case file(size, name) =>
        val fileData = File(name, size.toInt, node)
        addChild(node, fileData)
        node
  }

  def size(node: Data): Int = {
    node match
      case RootDirectory(_, children) => children.values.map(size).sum
      case File(_, size, _) => size
      case Directory(_, children, _) => children.values.map(size).sum
  }

  def traverse(node: Data, maxSize: Int, acc: List[Int]): List[Int] = {
    val satisfy = children(node).values.map(size).filter(_ < maxSize).toList
    children(node).flatMap(n => traverse(n._2, maxSize, satisfy.appendedAll(acc))).toList
  }


  override def part1Answer(): String = {
    val localRoot = root(readCommands(commands, root))
    getAllfolders(localRoot).map(size).filter(_ < 100000).sum.toString
  }

  override def part2Answer(): String = {
    val localRoot = root(readCommands(commands, root))
    val toDelete = 30000000 - (70000000 - size(localRoot))
    getAllfolders(localRoot).map(size).filter(_ > toDelete).min.toString
  }

  sealed trait Data(val id: String)

  case class RootDirectory(override val id: String, children: Map[String, Data]) extends Data(id)

  case class File(override val id: String, size: Int, parent: Data) extends Data(id)

  case class Directory(override val id: String, children: Map[String, Data], parent: Data) extends Data(id)

  private def getParent(data: Data): Data = {
    data match
      case RootDirectory(_, _) => data
      case File(_, _, parent) => parent
      case Directory(_, _, parent) => parent
  }

  private def root(data: Data) = {
    val parent = getParent(data)
    parent match
      case RootDirectory(_, _) => parent
      case File(_, _, parent) => getParent(parent)
      case Directory(_, _, parent) => getParent(parent)
  }

  private def addChild(parent: Data, child: Data): Unit = {
    parent match
      case RootDirectory(_, children) => children(child.id) = child
      case Directory(_, children, _) => children(child.id) = child
      case File(_, _, _) => ()
  }

  private def children(data: Data): Map[String, Data] = {
    data match
      case RootDirectory(_, children) => children
      case File(_, _, _) => Map.empty[String, Data]
      case Directory(_, children, _) => children
  }

  private def getAllfolders(data: Data): List[Data] = {
    data match
      case RootDirectory(_, children) => List(data).appendedAll(children.flatMap( child => getAllfolders(child._2)))
      case File(_, _, _) =>  List.empty[Data]
      case Directory(_, children, _) => List(data).appendedAll(children.flatMap( child => getAllfolders(child._2)))
  }

  private def child(data: Data, id: String): Data = {
    data match
      case RootDirectory(_, children) => children(id)
      case File(_, _, _) => data
      case Directory(_, children, _) => children(id)
  }

}
