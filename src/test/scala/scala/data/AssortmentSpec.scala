package scala.data

import org.scalatest._

class AssortmentSpec extends FlatSpec with Matchers {
  "Assortment" should "retain insertion order with each element that's added" in {
    val col = Assortment[String, Int]()

    // add set entry
    val col0 = col + "test"
    col0.set shouldBe Set("test")
    col0.map shouldBe Map()
    col0.seq shouldBe Seq()
    col0.insertOrder shouldBe List(Assortment.Entry("test"))

    // add mapping
    val col1 = col0 + ("test2", 1)
    col1.set shouldBe Set("test")
    col1.map shouldBe Map(("test2", 1))
    col1.seq shouldBe Seq()
    col1.insertOrder shouldBe List(
        Assortment.Mapping("test2", 1),
        Assortment.Entry("test"))

    // replace set entry with mapping
    val col2 = col1 + ("test", 1)
    col2.set shouldBe Set()
    col2.map shouldBe Map(("test2", 1), ("test", 1))
    col2.seq shouldBe Seq()
    col2.insertOrder shouldBe List(
        Assortment.Mapping("test2", 1),
        Assortment.Mapping("test", 1))

    // replace mapping with set entry
    val col3 = col2 + ("test2")
    col3.set shouldBe Set("test2")
    col3.map shouldBe Map(("test", 1))
    col3.seq shouldBe Seq()
    col3.insertOrder shouldBe List(
        Assortment.Entry("test2"),
        Assortment.Mapping("test", 1))

    // re-enter existing set entry
    val col4 = col3 + ("test2")
    col4.set shouldBe Set("test2")
    col4.map shouldBe Map(("test", 1))
    col4.seq shouldBe Seq()
    col4.insertOrder shouldBe List(
        Assortment.Entry("test2"),
        Assortment.Mapping("test", 1))

    // re-enter existing mapping
    val col5 = col4 + ("test", 1)
    col5.set shouldBe Set("test2")
    col5.map shouldBe Map(("test", 1))
    col5.seq shouldBe Seq()
    col5.insertOrder shouldBe List(
        Assortment.Entry("test2"),
        Assortment.Mapping("test", 1))

    // change value to which key is mapped
    val col6 = col5 + ("test", 3)
    col6.set shouldBe Set("test2")
    col6.map shouldBe Map(("test", 3))
    col6.seq shouldBe Seq()
    col6.insertOrder shouldBe List(
        Assortment.Entry("test2"),
        Assortment.Mapping("test", 3))

    // change value to which key is mapped
    val col7 = col6 :+ 7
    col7.set shouldBe Set("test2")
    col7.map shouldBe Map(("test", 3))
    col7.seq shouldBe Seq(7)
    col7.insertOrder shouldBe List(
        Assortment.Item(7),
        Assortment.Entry("test2"),
        Assortment.Mapping("test", 3))
  }
}
