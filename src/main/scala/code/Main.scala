package code

object Main extends App {
  println("Hello world!")
}

object Parser {

  val Throws = """([0-9])([0-9/])""".r
  val FinalThrow = """([0-9X])([0-9/X])([0-9X])""".r


  def parse(input: String): List[Frame] = {

    val initialString: List[String] = input.replaceAll("-","0").split(" ").toList

    initialString.map(x => {
      x match {
        case "X" => Frame(10,None)
        case Throws(throw1,"/") => {
          val score = throw1.toInt
          Frame(score,Some(10 - score))
        }
        case Throws(throw1,throw2) => {
          val score = throw1.toInt
          val score2 = throw2.toInt
          Frame(score,Some(score2))
        }
        case FinalThrow("X", "X", "X") => Frame(10, Some(10), Some(10))
        case FinalThrow("X", "X", throw3) => Frame(10, Some(10), Some(throw3.toInt))
        case FinalThrow("X", throw2, throw3) => Frame(10, Some(throw2.toInt), Some(throw3.toInt))
        case FinalThrow(throw1, "/", throw3) => Frame(throw1.toInt, Some(10 - throw1.toInt), Some(throw3.toInt))
      }
    })
  }
}

case class Frame(throw1: Int, throw2: Option[Int], throw3: Option[Int] = None){
  val frameScoreNotFinal: Int = throw1 + throw2.getOrElse(0)
  val frameScoreFinal: Int = throw1 + throw2.getOrElse(0) +throw3.getOrElse(0)

}

object Scorer {

  def run(list: List[Frame]): Int = list match {
    case Nil => 0
    case h :: Nil => returnFinalFrameScore(h)
    case h :: tail :: Nil => returnNinthScore(h, tail) + run(List(tail))
    case h :: m :: end :: _ => returnScore(h, m, end) + run(list.tail)
  }

  def returnScore(frame1: Frame, frame2: Frame, frame3: Frame): Int = {
    (frame1 ,frame2) match {
      case (Frame(10, _, _) ,Frame(10,_,_)) => 20 + frame3.throw1
      case (Frame(10, _, _) , _) => 10 + frame2.frameScoreNotFinal
      case (Frame(t1, t2, _) , _) if t1 + t2.get == 10 => 10 + frame2.throw1
      case (_,_) => frame1.frameScoreNotFinal
    }
  }

  def returnNinthScore(frame1: Frame, frame2: Frame): Int = {
    (frame1) match {
      case Frame(10, _, _) => 10 + frame2.frameScoreNotFinal
      case Frame(t1, t2, _) if t1 + t2.get == 10 => 10 + frame2.throw1
      case _ => frame1.frameScoreNotFinal
    }
  }

  def returnFinalFrameScore(frame: Frame): Int = frame.frameScoreFinal
}
