import spinal.core._

case class Poly (size : Int, sizePoly : Int) extends Component {

  val io = new Bundle {
    val wData1 = in Bits(size bits)
    val rData1 = out Bits(size bits)
    val wValid1 = in Bool
    val rAddress1 = in UInt(log2Up(sizePoly) bits)
    val rData2 = out Bits(size bits)
    val rAddress2 = in UInt(log2Up(sizePoly) bits)
  }

  var initValue : Array[Bits] = new Array[Bits](sizePoly)
  for (i <- 0 until sizePoly) {
    initValue(i) = 0
  }

  val mem = Mem(Bits(size bits), sizePoly ) init initValue
  /*
  mem.write(io.wAddress1, io.wData1, io.wValid1)
  io.rData1 := mem.readSync(io.rAddress1, io.rValid1)
  mem.write(io.wAddress2, io.wData2, io.wValid2)
  io.rData2 := mem.readSync(io.rAddress2, io.rValid2)
  */
  io.rData1 := mem.readWriteSync(io.rAddress1, io.wData1, True, io.wValid1)
  io.rData2 := mem.readSync(io.rAddress2, True)
}
