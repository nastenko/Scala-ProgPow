import java.lang.Long

case class Kiss99 ( var z:      Long = 0L,
                    var w:      Long = 0L,
                    var jsr:    Long = 0L,
                    var jcong:  Long = 0L){

  val MaxUint32 = 0xFFFFFFFFL

  assert(z     >= 0L && z     <= MaxUint32)
  assert(w     >= 0L && w     <= MaxUint32)
  assert(jsr   >= 0L && jsr   <= MaxUint32)
  assert(jcong >= 0L && jcong <= MaxUint32)

  def kiss99(): Long = {

    z = (((36969L * (z & 0xFFFFL)) & MaxUint32) + (z >> 16)) & MaxUint32
    w = (((18000L * (w & 0xFFFFL)) & MaxUint32) + (w >> 16)) & MaxUint32

    val MWC = (((z << 16) & MaxUint32) + w) & MaxUint32

    jsr ^= (jsr << 17) & MaxUint32
    jsr ^= (jsr >> 13)
    jsr ^= (jsr << 5) & MaxUint32

    jcong = (((69069L * jcong) & MaxUint32) + 1234567L) & MaxUint32

    ((MWC ^ jcong) + jsr) & MaxUint32
  }
}