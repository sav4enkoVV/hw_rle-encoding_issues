import org.scalatest._
import flatspec._
import matchers._

import scala.collection.mutable.ArrayBuffer


class RleEncodingSpec extends AnyFlatSpec with should.Matchers {

  val rleEncodingSpec = new RleEncoding

  "RleEncoding" should "encode given string" in {
    val str = "XAAABBBCCCX"
    rleEncodingSpec.encode(str) should be (ArrayBuffer(
      UncompressedBlock(1,ArrayBuffer('X')),
      CompressedBlock(3,'A'),
      CompressedBlock(3,'B'),
      CompressedBlock(3,'C'),
      UncompressedBlock(1,ArrayBuffer('X'))))
  }

  it should "encode given string_1" in {
    val str = "XYXYYYZ"
    rleEncodingSpec.encode(str) should be (ArrayBuffer(
      UncompressedBlock(3,ArrayBuffer('X','Y','X')),
      CompressedBlock(3,'Y'),
      UncompressedBlock(1,ArrayBuffer('Z'))))
  }

  it should "encode given string_2" in {
    val str = "AABCDYYZXX"
    rleEncodingSpec.encode(str) should be (ArrayBuffer(
      CompressedBlock(2,'A'),
      UncompressedBlock(3,ArrayBuffer('B','C','D')),
      CompressedBlock(2,'Y'),
      UncompressedBlock(1,ArrayBuffer('Z')),
      CompressedBlock(2,'X')))
  }

  it should "encode given string_3" in {
    val str = "XXXXZZYYYYA"
    rleEncodingSpec.encode(str) should be (ArrayBuffer(
      CompressedBlock(4,'X'),
      CompressedBlock(2,'Z'),
      CompressedBlock(4,'Y'),
      UncompressedBlock(1,ArrayBuffer('A'))))
  }
}