// This is the DAG-generation part of Ethash implementation taken from here:
// https://github.com/input-output-hk/mantis/blob/master/src/main/scala/io/iohk/ethereum/consensus/Ethash.scala
//
import scala.annotation.tailrec
import akka.util.ByteString
import java.nio.{ByteBuffer, ByteOrder}
import java.util

import fr.cryptohash.{Keccak256, Keccak512}
import org.spongycastle.util.encoders.Hex

case class Dag(epoch: Long){

  lazy val cache: Array[Int] = Dag.makeCache(epoch)

  def apply(index: Int): Array[Int] = Dag.calcDatasetItem(cache, index)
}

object Dag {

  // value used in Fowler–Noll–Vo hash function
  val FNV_PRIME: Int = 0x01000193

  // bytes in word
  val WORD_BYTES: Int = 4

  // bytes in dataset at genesis
  val DATASET_BYTES_INIT: Long = BigInt(2).pow(30).toLong

  // dataset growth per epoch
  val DATASET_BYTES_GROWTH: Long = BigInt(2).pow(23).toLong

  // bytes in cache at genesis
  val CACHE_BYTES_INIT: Long = BigInt(2).pow(24).toLong

  // cache growth per epoch
  val CACHE_BYTES_GROWTH: Long = BigInt(2).pow(17).toLong

  // blocks per epoch
  val EPOCH_LENGTH: Int = 30000

  // width of mix
  val MIX_BYTES: Int = 128

  // hash length in bytes
  val HASH_BYTES: Int = 64

  // number of parents of each dataset element
  val DATASET_PARENTS: Int = 256

  // number of rounds in cache production
  val CACHE_ROUNDS: Int = 3

  // number of accesses in hashimoto loop
  val ACCESSES: Int = 64

  def kec256(input: ByteString): ByteString = {
    def kec256(input: Array[Byte]*): Array[Byte] = {
      val digest: Keccak256 = new Keccak256
      input.foreach(i => digest.update(i))
      digest.digest
    }
    ByteString(kec256(input.toArray))
  }

  def kec512(input: Array[Byte]*): Array[Byte] = {
    val digest = new Keccak512
    input.foreach(i => digest.update(i))
    digest.digest
  }

  def seed(epoch: Long): ByteString = {
    (BigInt(0) until epoch)
      .foldLeft(ByteString(Hex.decode("00" * 32))) { case (b, _) => kec256(b) }
  }

  def epoch(blockNumber: Long): Long = blockNumber / EPOCH_LENGTH

  // Size of cache in bytes
  def cacheSize(epoch: Long): Long = {
    val sz = (CACHE_BYTES_INIT + CACHE_BYTES_GROWTH * epoch) - HASH_BYTES
    highestPrimeBelow(sz, HASH_BYTES)
  }

  def dagSize(epoch: Long): Long = {
    val sz = DATASET_BYTES_INIT + DATASET_BYTES_GROWTH * epoch - MIX_BYTES
    highestPrimeBelow(sz, MIX_BYTES)
  }

  @tailrec
  private def highestPrimeBelow(n: Long, len: Long): Long = {
    if (isPrime(n / len)) n
    else highestPrimeBelow(n - 2 * len, len)
  }

  private def isPrime(n: BigInt): Boolean = {
    @tailrec
    def isPrime(n: BigInt, i: BigInt): Boolean =
      (n % i != 0) && ((i * i > n) || isPrime(n, i + 2))

    if (n == 2 || n == 3) true
    else if (n < 2 || n % 2 == 0) false
    else isPrime(n, 3)
  }

  private def remainderUnsigned(dividend: Int, divisor: Int): Int = {
    if (divisor >= 0) {
      if (dividend >= 0) {
        dividend % divisor
      } else {
        // The implementation is a Java port of algorithm described in the book
        // "Hacker's Delight" (section "Unsigned short division from signed division").
        val q = ((dividend >>> 1) / divisor) << 1
        val w = dividend - q * divisor
        if (w < 0 || w >= divisor) w - divisor else w
      }
    } else if (dividend >= 0 || dividend < divisor) dividend else dividend - divisor
  }

  private def fnv(v1: Int, v2: Int): Int = {
    (v1 * FNV_PRIME) ^ v2
  }
  // -------------------------------- Byte utils --------------------------------
  def getIntFromWord(arr: Array[Byte]): Int = {
    ByteBuffer.wrap(arr, 0, 4).order(ByteOrder.LITTLE_ENDIAN).getInt
  }

  def bytesToInts(bytes: Array[Byte]): Array[Int] =
    bytes.grouped(4).map(getIntFromWord).toArray

  def intsToBytes(input: Array[Int]): Array[Byte] = {
    input.flatMap { i =>
      Array(
        (i & 0xFF).toByte,
        ((i >> 8) & 0xFF).toByte,
        ((i >> 16) & 0xFF).toByte,
        ((i >> 24) & 0xFF).toByte)
    }
  }

  def xor(a: Array[Byte], b: Array[Byte]): Array[Byte] = {
    (a zip b) map { case (b1, b2) => (b1 ^ b2).toByte }
  }
  // ----------------------------------------------------------------------------

  def makeCache(epoch: Long): Array[Int] = {
    /* watch out, arrays are mutable here */

    val n = (cacheSize(epoch) / HASH_BYTES).toInt
    val s = seed(epoch).toArray[Byte]

    val bytes = new Array[Array[Byte]](n)
    bytes(0) = kec512(s)

    (1 until n).foreach { i =>
      bytes(i) = kec512(bytes(i - 1))
    }

    (0 until CACHE_ROUNDS).foreach { _ =>
      (0 until n).foreach { i =>
        val v = remainderUnsigned(getIntFromWord(bytes(i)), n)
        bytes(i) = kec512(xor(bytes((i - 1 + n) % n), bytes(v)))
      }
    }

    val res = new Array[Int](bytes.length * bytes(0).length / 4)
    bytes.indices.foreach { i =>
      val ints = bytesToInts(bytes(i))
      System.arraycopy(ints, 0, res, i * ints.length, ints.length)
    }
    res
  }

  def calcDatasetItem(cache: Array[Int], index: Int): Array[Int] = {
    /* watch out, arrays are mutable here */

    val r = HASH_BYTES / WORD_BYTES
    val n = cache.length / r
    val initialMix = util.Arrays.copyOfRange(cache, index % n * r, (index % n + 1) * r)

    initialMix(0) = index ^ initialMix(0)
    val mix = bytesToInts(kec512(intsToBytes(initialMix)))
    val dsParents = DATASET_PARENTS
    val mixLen = mix.length

    (0 until dsParents).foreach { j =>
      val cacheIdx = remainderUnsigned(fnv(index ^ j, mix(j % r)), n)
      val off = cacheIdx * r
      (0 until mixLen).foreach { k =>
        mix(k) = fnv(mix(k), cache(off + k))
      }
    }
    bytesToInts(kec512(intsToBytes(mix)))
  }
}
