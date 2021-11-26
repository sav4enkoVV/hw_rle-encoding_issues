import org.scalatest._
import flatspec._
import matchers._

class RleEncodingSpec extends AnyFlatSpec with should.Matchers {

  val rleEncodingSpec = new RleEncoding

  "RleEncoding" should "encode given string" in {
    val str = "XAAABBBCCCX"
    rleEncodingSpec.encode(str) should be (List(UncompressedBlock(1,List('X')), CompressedBlock(3,'A'), CompressedBlock(3,'B'), CompressedBlock(3,'C'), UncompressedBlock(1,List('X'))))
    val str1 = "XYXYYYZ"
    rleEncodingSpec.encode(str1) should be (List(UncompressedBlock(3,List('X','Y','X')), CompressedBlock(3,'Y'), UncompressedBlock(1,List('Z'))))
    val str2 = "AABCDYYZXX"
    rleEncodingSpec.encode(str2) should be (List(CompressedBlock(2,'A'), UncompressedBlock(3,List('B','C','D')), CompressedBlock(2,'Y'), UncompressedBlock(1,List('Z')), CompressedBlock(2,'X')))
    val str3 = "XXXXZZYYYYA"
    rleEncodingSpec.encode(str3) should be (List(CompressedBlock(4,'X'), CompressedBlock(2,'Z'), CompressedBlock(4,'Y'), UncompressedBlock(1,List('A'))))
  }

  it should "" in {
    ???
  }
}