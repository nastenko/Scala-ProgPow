import java.lang.Long

object TestUtils {

  def parseHexString(hexstr: String): Array[Long] = {
    (for(i <- 0 until 64 by 8) yield {
      val numStr = hexstr.substring(i, i + 8)
      ProgPow.bswap32(Long.parseLong(numStr, 16))
    }).toArray
  }

  def parseNonce(hexstr: String): Array[Long] = {
    (for(i <- 0 until 16 by 8) yield {
      val numStr = hexstr.substring(i, i + 8)
      Long.parseLong(numStr, 16).asInstanceOf[java.lang.Long]
    }).toArray.reverse
  }
}