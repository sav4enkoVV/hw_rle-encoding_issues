import scala.collection.mutable.Stack

object RleEncodingApp extends App {

  val rleEncoding = new RleEncoding

  println(rleEncoding.encode("ABCDE")) // A1B1C1D1E1

  println(rleEncoding.encode("AABBCCDD")) // A2B2C2D2
  println(rleEncoding.encode("XAABBCCDD")) // 1X2A2B2C2D

  println(rleEncoding.encode("AAAABBBCCXYZDDDDEEEFFFAAAAAABBBBBBBBBBBBBBBBBBBBBBBBBBBBB")) // A4B3C2XYZD4E3F3A6B29
  println(rleEncoding.encode("AAAABBBCCXYZDDDDEEEFFFAAAAAABBBBBBBBBBBBBBBBBBBBBBBBBBBBBX")) // A4B3C2XYZD4E3F3A6B29X1
}

trait Block {
  def length: Int
}

case class UncompressedBlock(length: Int, data: Stack[Char]) extends Block
case class CompressedBlock(length: Int, data: Char) extends Block

class RleEncoding {

  def encode(str: String): Stack[Block] = {
    val (prev, optBlock, result) =
      str.toCharArray.foldLeft((None: Option[Char], None: Option[Block], Stack.empty[Block])) {
        case ((None, _, result), char) =>
          (Some(char), None, result)

        case ((Some(prev), None, result), char) if prev == char =>
            (Some(char), Some(CompressedBlock(1, prev)), result)

        case ((Some(prev), None, result), char) =>
            (Some(char), Some(UncompressedBlock(1, Stack(prev))), result)

        case ((Some(prev), Some(block@CompressedBlock(_, _)), result), char) if prev == char =>
          (Some(char), Some(CompressedBlock(block.length + 1, block.data)), result)

        case ((Some(prev), Some(block@CompressedBlock(_, _)), result), char) =>
          (Some(char), None, result += CompressedBlock(block.length + 1, block.data))

        case ((Some(prev), Some(block@UncompressedBlock(_, _)), result), char) if prev != char =>
          (Some(char), Some(UncompressedBlock(block.length + 1, block.data += prev)), result)

        case ((Some(prev), Some(block@UncompressedBlock(_, _)), result), char) =>
          (Some(char), Some(CompressedBlock(1, prev)), result += block)
      }


    (optBlock, prev) match {
      case (Some(block@UncompressedBlock(_, _)), Some(prev)) => result += UncompressedBlock(block.length + 1, block.data += prev)
      case (Some(block@CompressedBlock(_, data)), Some(prev)) => result += CompressedBlock(block.length + 1, data)
      case (None, Some(prev)) => result += UncompressedBlock(1, Stack(prev))
      case (None, None) => Stack.empty[Block]
    }

  }
}
