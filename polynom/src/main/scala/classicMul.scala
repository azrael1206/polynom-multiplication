import spinal.core._
import spinal.core.sim._



case class classicMul (sizeCoefficient : Int = 16, sizePolynom : Int = 256, toMod : Boolean = true) extends Component {

  val io = new Bundle {
    val wValid1 = in Bool
    val wValid2 = in Bool
    val rValid1 = in Bool
    val rValid2 = in Bool
    var rAddress1 : UInt = null

    var rAddress2 : UInt = null

    if (toMod) {
      rAddress1 = in UInt (log2Up(sizePolynom) bits)
      rAddress2 = in UInt (log2Up(sizePolynom) bits)
    } else {
      rAddress1 = in UInt (log2Up(2 * sizePolynom) bits)
      rAddress2 = in UInt (log2Up(2 * sizePolynom) bits)

    }
    val wAddress1 = in UInt (log2Up(sizePolynom) bits)
    val wAddress2 = in UInt (log2Up(sizePolynom) bits)
    val wData1 = in Bits(sizeCoefficient bits)
    val wData2 = in Bits(sizeCoefficient bits)
    val rData1 = out Bits(sizeCoefficient bits)
    val rData2= out Bits(sizeCoefficient bits)
    val start = in Bool
    val ready = out Bool
    val reset = in Bool
  }
  var maxCoefficient = 2^sizeCoefficient

  val poly1Reg = Poly(sizeCoefficient, sizePolynom)
  val poly2Reg = Poly(sizeCoefficient, sizePolynom)
  var outp : Poly = null
  val outptemp = Bits(sizeCoefficient bits)
  var temp : UInt = null
  var zwischen = Reg(Bits(2 * sizeCoefficient bits)) init 0
  if (toMod) {
    outp = Poly(sizeCoefficient, sizePolynom)
    temp = UInt(log2Up(sizePolynom) bits)
  } else {
    outp = Poly(sizeCoefficient, 2 * sizePolynom - 1)
    temp = UInt(log2Up(2 * sizePolynom) bits)
  }
  val start = Reg(Bool) init False
  val counterReg = Reg(UInt(log2Up(sizePolynom + 1)  bits)) init 0
  val counter2Reg = Reg(UInt(log2Up(sizePolynom) bits)) init 0
  val counter = UInt(log2Up(sizePolynom + 1) bits)
  val counter2 = UInt(log2Up(sizePolynom) bits)
  val counter3Reg = Reg(UInt(log2Up( 2 * sizePolynom) bits)) init 0

  val poly1 = Bits(sizeCoefficient bits)
  val poly2 = Bits(sizeCoefficient bits)
  val poly3 = Bits(sizeCoefficient bits)
  val poly4 = Bits(sizeCoefficient bits)
  val ready = Reg(Bool) init True

  val mult = Bits(2 * sizeCoefficient bits)
  val temp2 = Bits(2 * sizeCoefficient bits)
  val temp3 = Bits(sizeCoefficient bits)
  val reset = Reg(Bool) init False


  io.ready := ready
  if (toMod) {
    temp := mod(counterReg + counter2Reg)
  } else {
    temp := counterReg + counter2Reg
  }
  io.rData1.clearAll
  io.rData2.clearAll
  outptemp.clearAll
  poly1Reg.io.wValid1 := io.wValid1

  poly2Reg.io.wValid1 := io.wValid2

  poly1Reg.io.rAddress1 := io.wAddress1
  poly1Reg.io.wData1 := io.wData1
  poly2Reg.io.rAddress1 := io.wAddress2
  poly2Reg.io.wData1 := io.wData2

  poly1Reg.io.rAddress2.clearAll
  poly2Reg.io.rAddress2.clearAll
  outp.io.rAddress2.clearAll
  counter := 0
  counter2 := 0
  poly1.clearAll
  poly2.clearAll
  poly3.clearAll
  poly4.clearAll
  outp.io.wValid1 := start
  outp.io.rAddress1 := temp

  when (counterReg === counter2Reg) {
    mult := (poly1.asUInt * poly2.asUInt).asBits
  } otherwise {
    mult := ((poly1.asUInt * poly2.asUInt) + (poly3.asUInt * poly4.asUInt)).asBits
  }
  if (toMod) {
    when(counterReg + counter2Reg >= sizePolynom) {
      temp2 := (outptemp.asSInt - mult.asSInt).asBits
    } otherwise {
      temp2 := (outptemp.asSInt + mult.asSInt).asBits
    }

    when(sizePolynom - 1 - counterReg === 2 & sizePolynom - 1 - counter2Reg === 0) {
      zwischen := temp2
    } elsewhen (sizePolynom - 1 - counterReg === 1 & sizePolynom - 1 - counter2Reg === 1) {
      temp2 := (zwischen.asSInt - mult.asSInt).asBits
    }
  } else {

    when(sizePolynom - 1 - counterReg === 2 & sizePolynom - 1 - counter2Reg === 0) {
      zwischen := temp2
      temp2 := (outptemp.asSInt + mult.asSInt).asBits
    } elsewhen (sizePolynom - 1 - counterReg === 1 & sizePolynom - 1 - counter2Reg === 1) {
      temp2 := (zwischen.asSInt + mult.asSInt).asBits
    } otherwise {
      temp2 := (outptemp.asSInt + mult.asSInt).asBits
    }
  }
    temp3 := temp2(sizeCoefficient - 1 downto 0)

  outp.io.wData1 := temp3





  when(io.reset) {
    ready := False
    counter3Reg := 0
    reset := True
  } elsewhen(io.start) {
    start := True
    ready := False
    poly1Reg.io.rAddress1 := counter.resized
    poly2Reg.io.rAddress1 := counter2
    poly1Reg.io.rAddress2 := counter2
    poly2Reg.io.rAddress2 := counter.resized
    outp.io.rAddress1 := 0
    outptemp := outp.io.rData1
  } elsewhen(start) {
    when(counterReg === sizePolynom) {
      start := False
      outp.io.wValid1 := False
      counterReg := 0
      counter2Reg := 0
      counter := 0
      counter2 := 0
      ready := True
    }  elsewhen(counter2Reg === sizePolynom - 1) {
      counter2Reg := (counterReg + 1).resized
      counterReg := counterReg + 1
      counter := (counterReg + 1)
      counter2 := (counterReg + 1).resized
      poly1Reg.io.rAddress1 := counter.resized

      poly1 := poly1Reg.io.rData1
      poly2Reg.io.rAddress1 := counter2

      poly2 := poly2Reg.io.rData1
      poly1Reg.io.rAddress2 := counter2

      poly3 := poly1Reg.io.rData2
      poly2Reg.io.rAddress2 := counter.resized

      poly4 := poly2Reg.io.rData2
      if (toMod) {
        outp.io.rAddress2 := mod(counter + counter2)
      } else {
        outp.io.rAddress2 := counter + counter2
      }
      outptemp := outp.io.rData2
    } otherwise {
      counter2Reg := counter2Reg + 1
      counter := counterReg
      counter2 := counter2Reg + 1
      poly1Reg.io.rAddress1 := counter.resized

      poly1 := poly1Reg.io.rData1
      poly2Reg.io.rAddress1 := counter2

      poly2 := poly2Reg.io.rData1
      poly1Reg.io.rAddress2 := counter2

      poly3 := poly1Reg.io.rData2
      poly2Reg.io.rAddress2 := counter.resized

      poly4 := poly2Reg.io.rData2
      if (toMod) {
        outp.io.rAddress2 := mod(counter + counter2)
      } else {
        outp.io.rAddress2 := counter + counter2
      }
      outptemp := outp.io.rData2
    }

  } elsewhen (reset) {
    outp.io.rAddress1 := counter3Reg.resized
    poly1Reg.io.rAddress1 := counter3Reg.resized
    poly2Reg.io.rAddress1 := counter3Reg.resized
    outp.io.wValid1 := True
    poly1Reg.io.wValid1 := True
    poly2Reg.io.wValid1 := True
    outp.io.wData1.clearAll
    poly1Reg.io.wData1.clearAll
    poly2Reg.io.wData1.clearAll
    counter3Reg := counter3Reg + 1
    when (counter3Reg === 2 * sizePolynom - 1 ) {
      ready := True
      reset := False
    }
  } otherwise {

    when (io.rValid1) {
      outp.io.rAddress1 := io.rAddress1
      io.rData1 := outp.io.rData1
    }
    when (io.rValid2) {
      outp.io.rAddress2 := io.rAddress2
      io.rData2 := outp.io.rData2
    }
  }

   def mod(toMod: UInt): UInt = {
     val returnv = UInt(log2Up(sizePolynom - 1) bits)
     /*if (is2Pow) {
      returnv := toMod(log2Up(sizePolynom) - 1  downto 0)
    } else {*/
     when(toMod >= sizePolynom) {
       returnv := (toMod - (sizePolynom)).resized
     } otherwise {
       returnv := toMod.resized
     }
     // }
     return returnv.resized
   }

}




object recursion {
  def main(args: Array[String]): Unit = {
   SpinalVhdl(classicMul(16, 1024, true)).printPruned()

  }

}

