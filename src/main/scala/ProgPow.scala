// This implementation is inspired by Solar Designer's C-ProgPow
// https://github.com/solardiz/c-progpow
//
// NOTES:
// Current implementation has two modes:
//  - light mode (DagMode.Light): DAG nodes are computed on the fly, without storing a whole DAG in memory;
//  - full mode  (DagMode.Full) : whole DAG is stored in memory.
// Lightweight computation is speeded up (about 7.7 times) by caching intermediate DAG nodes per hash computation.
// Such a cache takes 32 KB of memory.
// Current implementation computes one hash in light mode for about 0.16 seconds (Intel i5-750).
// Full mode (DagMode.Full) significantly speeds up hashing in comparison with the light mode, but also a significant amount of memory is needed (1 GB and more, depending on epoch).
// In the full mode a DAG for an each epoch is computed once, then stored into a file so afterwards it can be just loaded from the file.
//
// Since JVM doesn't support unsigned integers, the Long is used to store and perform arithmetical operations on 32-bit unsigned integers.
//
import java.math.BigInteger
import java.lang.Long

import DagMode.DagMode

import scala.annotation.tailrec
import scala.collection.mutable

case class Hash32 (uint32s: Array[Long] = Array.fill[Long](Hash32.Uint32sNum)(0L))
object Hash32 { val Uint32sNum = 8 }

case class KissMix (
    generator:  Kiss99,
    mix_dst:    Array[Long],
    mix_src:    Array[Long]
  ){
  def copy(): KissMix = {
    KissMix(generator.copy(), mix_dst.clone(), mix_src.clone())
  }
}

class ProgPow (epoch: Int, dagMode: DagMode) {

  private val MaxUint32 = 0xFFFFFFFFL
  private val DagNodeLength = 16 // in Ints

  private val dag = Dag(epoch, dagMode)

  // to speedup the lightweight computation of a hash
  private val nodesCache = mutable.Map[Int, Array[Int]]()

  def dagItem(index: Int): Long = {
    val node_index = index / DagNodeLength
    val item_index = index % DagNodeLength

    if(nodesCache.exists(_._1 == node_index)){
      nodesCache(node_index)(item_index).toLong & MaxUint32
    } else {
      val node = dag(node_index)
      nodesCache += (node_index -> node)
      node(item_index).toLong & MaxUint32
    }
  }

  def cacheDag(
    loop: Long,
    mix:  Array[Array[Long]]
  ): Array[Array[Long]] = {

    val max_addr_base = Dag.dagSize(epoch) / (ProgPow.Lanes * ProgPow.DagLoads * ProgPow.Uint32Size)
    val dag_addr_base = mix((loop % ProgPow.Lanes).toInt)(0) % max_addr_base

    val dagCache =
      (for(l <- 0 until ProgPow.Lanes) yield
        (for(i <- 0 until ProgPow.DagLoads) yield {
          val dag_addr_lane = dag_addr_base.toInt * ProgPow.Lanes + (l ^ loop.toInt) % ProgPow.Lanes
          val node_index = dag_addr_lane * ProgPow.DagLoads.toInt + i
          dagItem(node_index)
        }).toArray).toArray
    dagCache
  }

  def loop(
    loop:         Long,
    mix:          Array[Array[Long]],
    kissMix:      KissMix
  ): Unit = {

    val dag_cache = cacheDag(loop, mix)
    val (gen, mix_seq_dst, mix_seq_src) = (kissMix.generator, kissMix.mix_dst, kissMix.mix_src)
    var mix_seq_dst_cnt = 0
    var mix_seq_src_cnt = 0

    for(i <- 0 until Integer.max(ProgPow.CntCache, ProgPow.CntMath)){

      if(i < ProgPow.CntCache){
        val src = mix_seq_src(mix_seq_src_cnt % ProgPow.Regs).toInt; mix_seq_src_cnt += 1
        val dst = mix_seq_dst(mix_seq_dst_cnt % ProgPow.Regs).toInt; mix_seq_dst_cnt += 1
        val sel = gen.kiss99()

        for(l <- 0 until ProgPow.Lanes){
          val offset = (mix(l)(src) % (ProgPow.CacheBytes / ProgPow.Uint32Size)).toInt
          mix(l)(dst) = ProgPow.merge(mix(l)(dst), dagItem(offset), sel)
        }
      }

      if(i < ProgPow.CntMath){
        val src_rnd = gen.kiss99() % (ProgPow.Regs * (ProgPow.Regs - 1))
        val src1 = (src_rnd % ProgPow.Regs).toInt
        val src2 = {
          val s = src_rnd / ProgPow.Regs
          if (s >= src1) s + 1
          else s
        }.toInt
        val dst  = mix_seq_dst(mix_seq_dst_cnt % ProgPow.Regs).toInt; mix_seq_dst_cnt += 1
        val sel1 = gen.kiss99()
        val sel2 = gen.kiss99()

        for(l <- 0 until ProgPow.Lanes){
          val data = ProgPow.math(mix(l)(src1), mix(l)(src2), sel1)
          mix(l)(dst) = ProgPow.merge(mix(l)(dst), data, sel2)
        }
      }
    }

    for(i <- 0 until ProgPow.DagLoads){
      val dst = {
        if(i == 0) 0
        else {
          val d = mix_seq_dst(mix_seq_dst_cnt % ProgPow.Regs); mix_seq_dst_cnt += 1
          d.toInt
        }
      }
      val sel = gen.kiss99()
      for(l <- 0 until ProgPow.Lanes){
        mix(l)(dst) = ProgPow.merge(mix(l)(dst), dag_cache(l)(i), sel)
      }
    }
  }

  def hash(
    prog_seed:  Array[Long],
    nonce:      Array[Long],
    header:     Hash32
  ): Hash32 = {

    val seed_256 = ProgPow.keccak_f800_progpow(header, nonce, Hash32())
    val seed = Array[Long](ProgPow.bswap32(seed_256.uint32s(1)), ProgPow.bswap32(seed_256.uint32s(0)))

    val mix = (for (lane <- 0 until ProgPow.Lanes) yield {
      ProgPow.fill_mix(seed, lane.toLong)
    }).toArray

    val kissMix = ProgPow.progPowInit(prog_seed)

    for(l <- 0 until ProgPow.CntDag){
      loop(l.toLong, mix, kissMix.copy())
    }

    nodesCache.clear

    val digest_lanes =
      for(l <- 0 until ProgPow.Lanes) yield {
        mix(l).foldLeft(ProgPow.FnvOffsetBasis){
          (digest_lane, mix_i) =>
            ProgPow.fnv1a(digest_lane, mix_i)
        }
      }

    val digest = Hash32(Array.fill[Long](Hash32.Uint32sNum)(ProgPow.FnvOffsetBasis))
    for(l <- digest_lanes.indices){
      val i = l % Hash32.Uint32sNum
      digest.uint32s(i) = ProgPow.fnv1a(digest.uint32s(i), digest_lanes(l))
    }
    digest
  }
}

object ProgPow {

  val Lanes       = 16
  val Regs        = 32
  val DagLoads    = 4
  val CacheBytes  = 16 * 1024
  val CntDag      = 64
  val CntCache    = 12
  val CntMath     = 20
  val Uint32Size  = 4
  val MaxUint32   = 0xFFFFFFFFL

  private lazy val MaxUint32BigInteger = BigInteger.ZERO.setBit(32).subtract(BigInteger.ONE) // make the 0xFFFFFFFF mask

  private val keccakf_rndc: Array[Long] = Array[Long] (
    0x00000001L, 0x00008082L, 0x0000808aL, 0x80008000L, 0x0000808bL, 0x80000001L,
    0x80008081L, 0x00008009L, 0x0000008aL, 0x00000088L, 0x80008009L, 0x8000000aL,
    0x8000808bL, 0x0000008bL, 0x00008089L, 0x00008003L, 0x00008002L, 0x00000080L,
    0x0000800aL, 0x8000000aL, 0x80008081L, 0x00008080L, 0x80000001L, 0x80008008L
  )

  private val keccakf_rotc: Array[Int] = Array(
    1,  3,  6,  10, 15, 21, 28, 36, 45, 55, 2,  14,
    27, 41, 56, 8,  25, 43, 62, 18, 39, 61, 20, 44
  )
  private val keccakf_piln: Array[Int] = Array(
    10, 7,  11, 17, 18, 3, 5,  16, 8,  21, 24, 4,
    15, 23, 19, 13, 12, 2, 20, 14, 22, 9,  6,  1
  )

  private val FnvPrime       = 0x01000193L
  private val FnvOffsetBasis = 0x811c9dc5L

  def popcount(a: Long): Long = Long.bitCount(a).toLong

  def min(a: Long, b: Long): Long = Long.min(a, b)

  def max(a: Long, b: Long): Long = Long.max(a, b)

  def mul_hi32(a: Long, b: Long): Long = {
    assert(a >= 0L && a <= MaxUint32)
    assert(b >= 0L && b <= MaxUint32)

    val a_big = BigInteger.valueOf(a)
    val b_big = BigInteger.valueOf(b)
    val big_res = a_big.multiply(b_big).shiftRight(32)
    Long.parseLong(big_res.toString)
  }

  def mul32(a: Long, b: Long): Long = {
    assert(a >= 0L && a <= MaxUint32)
    assert(b >= 0L && b <= MaxUint32)

    val a_big = BigInteger.valueOf(a)
    val b_big = BigInteger.valueOf(b)
    val big_res = a_big.multiply(b_big).and(MaxUint32BigInteger)
    Long.parseLong(big_res.toString)
  }

  def bswap32(a: Long): Long = {
    assert(a >= 0L && a <= MaxUint32)

    val swap16 = ((a << 16) | (a >> 16)) & MaxUint32
    ((swap16 & 0x00FF00FFL) << 8) | ((swap16 >> 8) & 0x00FF00FFL)
  }

  def clz32(a: Long): Long = {
    assert(a >= 0L && a <= MaxUint32)

    @tailrec
    def clz(a: Long, num: Int): Long = {
      if(!((a >> num) == 0)){
        (32 - (num + 1)).toLong
      } else if(num == 0){
        32.toLong
      } else {
        clz(a, num - 1)
      }
    }
    clz(a, 31)
  }

  def rotl32(a: Long, shiftIn: Long): Long = {
    assert(a       >= 0L && a       <= MaxUint32)
    assert(shiftIn >= 0L && shiftIn <= MaxUint32)

    val shift = shiftIn % 32
    if(shift == 0L) a
    ((a << shift) | (a >> (32 - shift))) & MaxUint32
  }

  def rotr32(a: Long, shiftIn: Long): Long = {
    assert(a       >= 0L && a       <= MaxUint32)
    assert(shiftIn >= 0L && shiftIn <= MaxUint32)

    val shift = shiftIn % 32
    if(shift == 0L) a
    ((a >> shift) | (a << (32 - shift))) & MaxUint32
  }

  def fnv1a(h: Long, d: Long): Long = {
    assert(h >= 0L && h <= MaxUint32)
    assert(d >= 0L && d <= MaxUint32)

    ((h ^ d) * FnvPrime) & MaxUint32
  }

  def keccak_f800_round(
    st: Array[Long],
    r:  Int
  ): Unit = {

    assert(r >= 0L && r < keccakf_rndc.length)

    val bc = Array.fill(5)(0L)

    for (i <- 0 until 5){
        bc(i) = st(i) ^ st(i + 5) ^ st(i + 10) ^ st(i + 15) ^ st(i + 20)
    }

    for (i <- 0 until 5){
      val t = bc((i + 4) % 5) ^ rotl32(bc((i + 1) % 5), 1L)
      for (j <- 0 until 25 by 5){
        st(j + i) = st(j + i) ^ t
      }
    }

    var t = st(1)

    for (i <- 0 until 24){
      val j = keccakf_piln(i)
      bc(0) = st(j)
      st(j) = rotl32(t, keccakf_rotc(i).toLong)
      t = bc(0)
    }

    for (j <- 0 until 25 by 5){
      for (i <- 0 until 5){
        bc(i) = st(j + i)
      }
      for (i <- 0 until 5){
        val b = (bc((i + 1) % 5) ^ MaxUint32) & bc((i + 2) % 5)
        st(j + i) = st(j + i) ^ b
      }
    }
    st(0) = st(0) ^ keccakf_rndc(r)
  }

  def keccak_f800_progpow(
    header: Hash32,
    seed:   Array[Long],
    digest: Hash32
  ): Hash32 = {

    val st = Array.fill[Long](25)(0L)

    for(i <- 0 until 8) { st(i) = header.uint32s(i) }
    st(8) = seed(0)
    st(9) = seed(1)
    for(i <- 0 until 8) { st(10 + i) = digest.uint32s(i) }
    for(r <- 0 until 22){ ProgPow.keccak_f800_round(st, r) }

    Hash32(st.take(8))
  }

  def fill_mix(
    seed:     Array[Long],
    lane_id:  Long
  ): Array[Long] = {

    assert(lane_id >= 0L && lane_id <= MaxUint32)

    val z     = fnv1a(FnvOffsetBasis, seed(0))
    val w     = fnv1a(z, seed(1))
    val jsr   = fnv1a(w, lane_id)
    val jcong = fnv1a(jsr, lane_id)
    val generator = Kiss99(z, w, jsr, jcong)

    val mix = (for(i <- 0 until Regs) yield generator.kiss99()).toArray
    mix
  }

  def merge(
    a: Long,
    b: Long,
    r: Long
  ): Long = {

    assert(a >= 0L && a <= MaxUint32)
    assert(b >= 0L && b <= MaxUint32)
    assert(r >= 0L && r <= MaxUint32)

    r % 4 match {
      case 0 => (a * 33 + b) & MaxUint32
      case 1 => ((a ^ b) * 33) & MaxUint32
      case 2 => rotl32(a, (((r >> 16) & MaxUint32) % 31) + 1) ^ b
      case 3 => rotr32(a, (((r >> 16) & MaxUint32) % 31) + 1) ^ b
    }
  }

  def math(
    a: Long,
    b: Long,
    r: Long
  ): Long = {

    assert(a >= 0L && a <= MaxUint32)
    assert(b >= 0L && b <= MaxUint32)
    assert(r >= 0L && r <= MaxUint32)

    r % 11 match {
      case 0 => (a + b) & MaxUint32
      case 1 => mul32(a, b)
      case 2 => mul_hi32(a, b)
      case 3 => min(a, b)
      case 4 => rotl32(a, b)
      case 5 => rotr32(a, b)
      case 6 => a & b
      case 7 => a | b
      case 8 => a ^ b
      case 9 => clz32(a) + clz32(b)
      case 10 => popcount(a) + popcount(b)
    }
  }

  def progPowInit(
    prog_seed: Array[Long]
  ): KissMix = {

    val z     = fnv1a(FnvOffsetBasis, prog_seed(0))
    val w     = fnv1a(z, prog_seed(1))
    val jsr   = fnv1a(w, prog_seed(0))
    val jcong = fnv1a(jsr, prog_seed(1))

    val generator = Kiss99(z, w, jsr, jcong)

    val mix_seq_dst = Array.tabulate[Long](Regs)(n => n.toLong)
    val mix_seq_src = Array.tabulate[Long](Regs)(n => n.toLong)

    for (i <- Regs - 1 until 0 by -1) {
      val j_dst = (generator.kiss99() % (i + 1)).toInt
      val j_src = (generator.kiss99() % (i + 1)).toInt

      val (dstj, dsti) = (mix_seq_dst(j_dst), mix_seq_dst(i))
      mix_seq_dst(i) = dstj; mix_seq_dst(j_dst) = dsti
      val (srcj, srci) = (mix_seq_src(j_src), mix_seq_src(i))
      mix_seq_src(i) = srcj; mix_seq_src(j_src) = srci
    }
    KissMix(generator, mix_seq_dst, mix_seq_src)
  }
}
