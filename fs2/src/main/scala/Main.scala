import cats.effect.{ContextShift, IO}
import fs2.{Pipe, Stream}
object Main extends App {

  implicit val ioContextShift: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)

  def repeat[F[_],O]: Pipe[F,O,O] = in => in ++ in.through(repeat)

  def drain[F[_],O]: Pipe[F,O,O] =  _.flatMap(_ => Stream.empty)

  def attempt[F[_],O]: Pipe[F,O,Either[Throwable, O]] = _.map(Right(_)).handleErrorWith(e => Stream.emit(Left(e)))

//  println(Stream(1,2,3).merge(Stream.eval(IO { Thread.sleep(200); 4 })).compile.toVector.unsafeRunSync())


  println((Stream(1,2) ++ Stream.raiseError[IO](new RuntimeException)).attempt.compile.toList.unsafeRunSync())
}
