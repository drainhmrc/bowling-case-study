package code

import org.scalatest._

class MainSpec extends WordSpec with Matchers {
  "Parser" should {
    "Correctly parse all strikes" in {
      val testString = "X X X X X X X X X XXX"
      val expected = List.fill(9)(Frame(10, None)) ::: Frame(10, Some(10), Some(10)) :: Nil

      Parser.parse(testString) shouldBe expected
    }
    "Correctly parse all 9-" in {
      val testString = "9- 9- 9- 9- 9- 9- 9- 9- 9- 9-"
      val expected = List.fill(10)(Frame(9, Some(0)))

      Parser.parse(testString) shouldBe expected
    }
    "Correctly parse all spares with final 5" in {
      val testString = "5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/5"
      val expected: List[Frame] = List.fill(9)(Frame(5, Some(5))) ::: Frame(5, Some(5), Some(5)) :: Nil

      Parser.parse(testString) shouldBe expected
    }
    "Correctly parse all spares with final X55" in {
      val testString = "5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ X55"
      val expected: List[Frame] = List.fill(9)(Frame(5, Some(5))) ::: Frame(10, Some(5), Some(5)) :: Nil

      Parser.parse(testString) shouldBe expected
    }

  }

  "Scorer" should {
    "total scores all strikes" in {
      val testList = List.fill(9)(Frame(10, None)) ::: Frame(10, Some(10), Some(10)) :: Nil
      val expected = 300

      Scorer.run(testList) shouldBe expected
    }
    "total scores all spares" in {
      val testList = List.fill(9)(Frame(5, Some(5))) ::: Frame(5, Some(5), Some(5)) :: Nil
      val expected = 150

      Scorer.run(testList) shouldBe expected
    }
    "total scores all 9s" in {
      val testList = List.fill(10)(Frame(9, Some(0)))
      val expected = 90

      Scorer.run(testList) shouldBe expected
    }
  }

}
