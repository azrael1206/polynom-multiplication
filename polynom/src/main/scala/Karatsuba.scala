import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

class Karatsuba (sizeCoefficent :Int, sizePolynom : Int, cClassic : Int, toMod : Boolean = true) extends Component {

  val io = new Bundle {
    val wValid1 = in Bool
    val wValid2 = in Bool
    val rValid1 = in Bool
    val rValid2 = in Bool
    //Die variable muss deshalb so erstellt werden, weil wir erst später entscheiden wie groß sie ist
    var rAddress1 : UInt = null
    var rAddress2 : UInt = null
    //entscheiden anhand von toMod wie größ die einzelnen Variablen werden
    if (toMod) {
      rAddress1 = in UInt (log2Up(sizePolynom) bits)
      rAddress2 = in UInt (log2Up(sizePolynom) bits)
    } else {
      rAddress1 = in UInt (log2Up(2 * sizePolynom) bits)
      rAddress2 = in UInt (log2Up(2 * sizePolynom) bits)
    }
    val wAddress1 = in UInt (log2Up(sizePolynom) bits)
    val wAddress2 = in UInt (log2Up(sizePolynom) bits)
    val wData1 = in Bits(sizeCoefficent bits)
    val wData2 = in Bits(sizeCoefficent bits)
    val rData1 = out Bits(sizeCoefficent bits)
    val rData2= out Bits(sizeCoefficent bits)
    val start = in Bool
    val ready = out Bool
    val reset = in Bool
  }

  var classic = false
  io.rData1.clearAll()
  io.rData2.clearAll()


  var sizeTemp = sizePolynom

  //Erweitern bis sizePolynom eine 2er Potenz ist
  if (!isPow2(sizeTemp)) {
    while(!isPow2(sizeTemp)) {
      sizeTemp = sizeTemp + 1
    }
  }

  //Hier wird entschieden wann auf die Klassische gesprungen wird
  if (sizeTemp / 2 == cClassic) {
    classic = true
  }
  val poly1Reg = Poly(sizeCoefficent, sizeTemp)
  val poly2Reg = Poly(sizeCoefficent, sizeTemp)
  var outp : Poly = null
  val ready = Reg(Bool) init True
  val counter = Reg(UInt (log2Up(sizeTemp / 2) bits)) init 0
  var counter2 : UInt = null
  val isCopy = Reg(Bool) init False
  counter2 = Reg(UInt (log2Up(2 * sizeTemp) bits)) init 0
  //wieder entscheiden wgroß die einzelne Variable wird
  if (toMod) {
    outp = Poly(sizeCoefficent, sizeTemp)

  } else {
    outp = Poly(sizeCoefficent, 2 * sizeTemp - 1)

  }

  io.ready := ready
  poly1Reg.io.wData1.clearAll
  poly1Reg.io.rAddress1.clearAll
  poly1Reg.io.rAddress2.clearAll
  poly2Reg.io.wData1.clearAll
  poly2Reg.io.rAddress1.clearAll
  poly2Reg.io.rAddress2.clearAll
  poly1Reg.io.wValid1 := False
  poly2Reg.io.wValid1 := False
  outp.io.wValid1 := False
  outp.io.rAddress1.clearAll
  outp.io.rAddress2.clearAll
  outp.io.wData1.clearAll



  // Dies ist einer der wichtigsten bereiche, hier wird der Recursive aufruf gemacht
  val rkarat = !classic generate new Area {
    val karat1 = new Karatsuba(sizeCoefficent, sizeTemp / 2, cClassic, false)
    val karat2 = new Karatsuba(sizeCoefficent, sizeTemp / 2, cClassic, false)
    val karat3 = new Karatsuba(sizeCoefficent, sizeTemp / 2, cClassic, false)
    karat1.io.wAddress1.clearAll
    karat1.io.wAddress2.clearAll
    karat2.io.wAddress1.clearAll
    karat2.io.wAddress2.clearAll
    karat3.io.wAddress1.clearAll
    karat3.io.wAddress2.clearAll

    karat1.io.rAddress1.clearAll
    karat1.io.rAddress2.clearAll
    karat2.io.rAddress1.clearAll
    karat2.io.rAddress2.clearAll
    karat3.io.rAddress1.clearAll
    karat3.io.rAddress2.clearAll

    karat1.io.wValid1 := isCopy
    karat1.io.wValid2 := isCopy
    karat2.io.wValid1 := isCopy
    karat2.io.wValid2 := isCopy
    karat3.io.wValid1 := isCopy
    karat3.io.wValid2 := isCopy
    karat1.io.rValid1 := !isCopy
    karat1.io.rValid2 := !isCopy
    karat2.io.rValid1 := !isCopy
    karat2.io.rValid2 := !isCopy
    karat3.io.rValid1 := !isCopy
    karat3.io.rValid2 := !isCopy
    karat1.io.wData1 := poly1Reg.io.rData1
    karat1.io.wData2 := poly2Reg.io.rData1
    karat2.io.wData1 := poly1Reg.io.rData2
    karat2.io.wData2 := poly2Reg.io.rData2
    karat3.io.wData1 := (poly1Reg.io.rData1.asUInt + poly1Reg.io.rData2.asUInt).asBits
    karat3.io.wData2 := (poly2Reg.io.rData1.asUInt + poly2Reg.io.rData2.asUInt).asBits
    karat1.io.start := False
    karat2.io.start := False
    karat3.io.start := False
    karat1.io.reset := False
    karat2.io.reset := False
    karat3.io.reset := False

    //eine State Machine um den Karatsuba abzuarbeiten
    val stateMachine =  new StateMachine {
      val idle = new State with EntryPoint
      val copy = new State
      val start = new State
      val waitc = new State
      val getResult = new State
      val getResult2 = new State
      val getResult3 = new State
      val getResult4 = new State
      val reset = new State



      //warte Position und die einzigste position in dem die Polynome geschrieben werden können und das Ausgangsregister gelesen werden kann
      idle.whenIsActive{
        poly1Reg.io.rAddress1 := io.wAddress1
        poly1Reg.io.wData1 := io.wData1
        poly2Reg.io.rAddress1 := io.wAddress2
        poly2Reg.io.wData1 := io.wData2
        poly1Reg.io.wValid1 := io.wValid1
        poly2Reg.io.wValid1:= io.wValid2
        when (io.rValid1) {
          outp.io.rAddress1 := io.rAddress1
          io.rData1 := outp.io.rData1
        }
        when (io.rValid2) {
          outp.io.rAddress2 := io.rAddress2
          io.rData2 := outp.io.rData2
        }
        when(io.start) {
          counter := 0
          poly1Reg.io.rAddress1 := 0
          poly2Reg.io.rAddress1 := 0
          poly1Reg.io.rAddress2 := (sizeTemp / 2)
          poly2Reg.io.rAddress2 := (sizeTemp / 2)
          isCopy := True
          ready := False
          goto(copy)
        }
        when (io.reset) {
          karat1.io.reset := True
          karat2.io.reset := True
          karat3.io.reset := True
          ready:= False
          counter2 := 0
          goto(reset)
        }
      }
      //Hier werden die Passenden werte in die Kinder kopiert
      copy.whenIsActive{
        karat1.io.wAddress1 := counter
        karat1.io.wAddress2 := counter
        karat2.io.wAddress1 := counter
        karat2.io.wAddress2 := counter
        karat3.io.wAddress1 := counter
        karat3.io.wAddress2 := counter
        when (counter =/= (sizeTemp / 2) - 1) {
          poly1Reg.io.rAddress1 := (counter + 1).resized
          poly2Reg.io.rAddress1 := (counter + 1).resized
          poly1Reg.io.rAddress2 := (sizeTemp / 2) + counter + 1
          poly2Reg.io.rAddress2 := (sizeTemp / 2) + counter + 1

          counter := counter + 1
        } otherwise {
          isCopy := False

          goto(start)
        }
      }

      //gewartet bis die Kinder fertig sind
      waitc.whenIsActive{
        when (karat1.io.ready & karat2.io.ready & karat3.io.ready) {
          counter2 := 0
          karat1.io.rAddress1 := 0
          karat2.io.rAddress1 := 0
          outp.io.rAddress2 := 0
          goto (getResult)

        }
      }
      //Kopiert 0 - (sizepolynom/2 -1) in das Ausgangsregister
      getResult.whenIsActive{
        outp.io.rAddress1 := counter2.resized
        outp.io.wValid1 := True
        outp.io.wData1 := karat1.io.rData1
        karat1.io.rAddress1 := (counter2 + 1).resized
        counter2 := counter2 + 1
        when (counter2 + 1 === (sizePolynom / 2)) {
          karat3.io.rAddress1 := 0
          karat1.io.rAddress2 := 0
          karat2.io.rAddress2 := 0
          goto(getResult2)
        }
      }
      //Kopiert sizepolynom/2 - sizepolynom -1 in das Ausgangsregister
      getResult2.whenIsActive{
        outp.io.rAddress1 := counter2.resized
        outp.io.wValid1 := True

        when(counter2 === sizePolynom - 1 ) {
          outp.io.wData1 := (karat3.io.rData1.asSInt - karat2.io.rData2.asSInt - karat1.io.rData2.asSInt).asBits
        } otherwise {
          outp.io.wData1 := (karat1.io.rData1.asSInt + (karat3.io.rData1.asSInt - karat2.io.rData2.asSInt - karat1.io.rData2.asSInt)).asBits
        }
        karat1.io.rAddress2 := (counter2 + 1 - sizePolynom/2).resized
        karat2.io.rAddress2 := (counter2 + 1 - sizePolynom/2).resized
        karat3.io.rAddress1 := (counter2 + 1 - sizePolynom/2).resized

        karat1.io.rAddress1 := (counter2 + 1).resized

        counter2 := counter2 + 1
        when (counter2 + 1 === sizePolynom) {
          karat2.io.rAddress1 := 0
          if (toMod) {
            outp.io.rAddress2 := mod(counter2 + 1)
          }
          goto(getResult3)
        }
      }
      //Kopiert sizepolynom - sizepolynom + sizepolynom - 1 ins Ausgangsregister
      getResult3.whenIsActive {
        outp.io.wValid1 := True
        if (toMod) {
          outp.io.rAddress1 := mod(counter2)
          outp.io.wData1 := (outp.io.rData2.asSInt - (karat2.io.rData1.asSInt + (karat3.io.rData1.asSInt - karat2.io.rData2.asSInt - karat1.io.rData2.asSInt))).asBits
          outp.io.rAddress2 := mod(counter2 + 1)

        } else {
          outp.io.rAddress1 := counter2
          outp.io.wData1 := (karat2.io.rData1.asSInt + (karat3.io.rData1.asSInt - karat2.io.rData2.asSInt - karat1.io.rData2.asSInt)).asBits
        }
        karat1.io.rAddress2 := (counter2 + 1 - sizePolynom/2).resized
        karat2.io.rAddress2 := (counter2 + 1 - sizePolynom/2).resized
        karat3.io.rAddress1 := (counter2 + 1 - sizePolynom/2).resized

        karat2.io.rAddress1 := (counter2 + 1 - sizePolynom).resized

        counter2 := counter2 + 1
        when (counter2 + 1 === sizePolynom + sizePolynom/2 - 1) {
          goto(getResult4)
        }
      }
      //Kopiert die letzten werte ins Ausgangsregister
      getResult4.whenIsActive {
        outp.io.wValid1 := True

        if (toMod) {
          outp.io.rAddress1 := mod(counter2)
          outp.io.rAddress2 := mod(counter2 + 1)
          outp.io.wData1 := (outp.io.rData2.asSInt - karat2.io.rData1.asSInt).asBits
        } else {
          outp.io.rAddress1 := counter2
          outp.io.wData1 := karat2.io.rData1
        }
        karat2.io.rAddress1 := (counter2 + 1 - sizePolynom).resized
        counter2 := counter2 + 1

        when(counter2 + 1 === 2 * sizePolynom - 1) {
          ready := True
          goto(idle)
        }
      }
      //ein Reset State in dem der Speicher komplett auf null gesetzt wird
      reset.whenIsActive{
        outp.io.rAddress1 := counter2.resized
        poly1Reg.io.rAddress1 := counter2.resized
        poly2Reg.io.rAddress1 := counter2.resized
        outp.io.wValid1 := True
        poly1Reg.io.wValid1 := True
        poly2Reg.io.wValid1 := True
        outp.io.wData1.clearAll
        poly1Reg.io.wData1.clearAll
        poly2Reg.io.wData1.clearAll
        counter2 := counter2 + 1
        when(counter2 === 2 * sizePolynom) {
          ready := True
          goto(idle)
        }
      }
    }




  }
  //Das ist der Abbruch von der Rekusrion. Ist komplett identisch wie der Rest aufgebaut
  val clMult = classic generate new Area {
    val cMult1 = classicMul(sizeCoefficent, sizeTemp / 2, false)
    val cMult2 = classicMul(sizeCoefficent, sizeTemp / 2, false)
    val cMult3 = classicMul(sizeCoefficent, sizeTemp / 2, false)

    cMult1.io.wAddress1.clearAll
    cMult1.io.wAddress2.clearAll
    cMult2.io.wAddress1.clearAll
    cMult2.io.wAddress2.clearAll
    cMult3.io.wAddress1.clearAll
    cMult3.io.wAddress2.clearAll

    cMult1.io.rAddress1.clearAll
    cMult1.io.rAddress2.clearAll
    cMult2.io.rAddress1.clearAll
    cMult2.io.rAddress2.clearAll
    cMult3.io.rAddress1.clearAll
    cMult3.io.rAddress2.clearAll

    cMult1.io.wValid1 := isCopy
    cMult1.io.wValid2 := isCopy
    cMult2.io.wValid1 := isCopy
    cMult2.io.wValid2 := isCopy
    cMult3.io.wValid1 := isCopy
    cMult3.io.wValid2 := isCopy
    cMult1.io.rValid1 := !isCopy
    cMult1.io.rValid2 := !isCopy
    cMult2.io.rValid1 := !isCopy
    cMult2.io.rValid2 := !isCopy
    cMult3.io.rValid1 := !isCopy
    cMult3.io.rValid2 := !isCopy
    cMult1.io.wData1 := poly1Reg.io.rData1
    cMult1.io.wData2 := poly2Reg.io.rData1
    cMult2.io.wData1 := poly1Reg.io.rData2
    cMult2.io.wData2 := poly2Reg.io.rData2
    cMult3.io.wData1 := (poly1Reg.io.rData1.asUInt + poly1Reg.io.rData2.asUInt).asBits
    cMult3.io.wData2 := (poly2Reg.io.rData1.asUInt + poly2Reg.io.rData2.asUInt).asBits
    cMult1.io.start := False
    cMult2.io.start := False
    cMult3.io.start := False
    cMult1.io.reset := False
    cMult2.io.reset := False
    cMult3.io.reset := False
    val stateMachine = new StateMachine {
      val idle = new State with EntryPoint
      val copy = new State
      val start = new State
      val waitc = new State
      val getResult = new State
      val getResult2 = new State
      val getResult3 = new State
      val getResult4 = new State
      val reset = new State


      idle.whenIsActive {
        poly1Reg.io.rAddress1 := io.wAddress1
        poly1Reg.io.wData1 := io.wData1
        poly2Reg.io.rAddress1 := io.wAddress2
        poly2Reg.io.wData1 := io.wData2
        poly1Reg.io.wValid1 := io.wValid1
        poly2Reg.io.wValid1 := io.wValid2
        when(io.rValid1) {
          outp.io.rAddress1 := io.rAddress1
          io.rData1 := outp.io.rData1
        }
        when(io.rValid2) {
          outp.io.rAddress2 := io.rAddress2
          io.rData2 := outp.io.rData2
        }
        when(io.start) {
          counter := 0
          poly1Reg.io.rAddress1 := 0
          poly2Reg.io.rAddress1 := 0
          poly1Reg.io.rAddress2 := (sizeTemp / 2)
          poly2Reg.io.rAddress2 := (sizeTemp / 2)
          isCopy := True
          ready := False
          goto(copy)
        }
        when (io.reset) {
          cMult1.io.reset := True
          cMult2.io.reset := True
          cMult3.io.reset := True
          ready:= False
          counter2 := 0
          goto(reset)
        }
      }

      copy.whenIsActive {
        cMult1.io.wAddress1 := counter
        cMult1.io.wAddress2 := counter
        cMult2.io.wAddress1 := counter
        cMult2.io.wAddress2 := counter
        cMult3.io.wAddress1 := counter
        cMult3.io.wAddress2 := counter
        when(counter =/= (sizeTemp / 2) - 1) {
          poly1Reg.io.rAddress1 := (counter + 1).resized
          poly2Reg.io.rAddress1 := (counter + 1).resized
          poly1Reg.io.rAddress2 := (sizeTemp / 2) + counter + 1
          poly2Reg.io.rAddress2 := (sizeTemp / 2) + counter + 1

          counter := counter + 1
        } otherwise {
          isCopy := False

          goto(start)
        }
      }
      start.whenIsActive {
        cMult1.io.start := True
        cMult2.io.start := True
        cMult3.io.start := True
        goto(waitc)
      }

      waitc.whenIsActive {
        when(cMult1.io.ready & cMult2.io.ready & cMult3.io.ready) {
          counter := 0
          cMult1.io.rAddress1 := 0
          cMult2.io.rAddress1 := 0
          outp.io.rAddress2 := 0
          goto(getResult)

        }
      }

      getResult.whenIsActive {
        outp.io.rAddress1 := counter2.resized
        outp.io.wValid1 := True
        outp.io.wData1 := cMult1.io.rData1
        cMult1.io.rAddress1 := (counter2 + 1).resized
        counter2 := counter2 + 1
        when(counter2 + 1 === (sizePolynom / 2)) {
          cMult3.io.rAddress1 := 0
          cMult1.io.rAddress2 := 0
          cMult2.io.rAddress2 := 0
          goto(getResult2)
        }
      }

      getResult2.whenIsActive {
        outp.io.rAddress1 := counter2.resized
        outp.io.wValid1 := True
        when(counter2 === sizePolynom - 1) {
          outp.io.wData1 := (cMult3.io.rData1.asSInt - cMult2.io.rData2.asSInt - cMult1.io.rData2.asSInt).asBits
        } otherwise {
          outp.io.wData1 := (cMult1.io.rData1.asSInt + (cMult3.io.rData1.asSInt - cMult2.io.rData2.asSInt - cMult1.io.rData2.asSInt)).asBits
        }
        cMult1.io.rAddress2 := (counter2 + 1 - sizePolynom / 2).resized
        cMult2.io.rAddress2 := (counter2 + 1 - sizePolynom / 2).resized
        cMult3.io.rAddress1 := (counter2 + 1 - sizePolynom / 2).resized

        cMult1.io.rAddress1 := (counter2 + 1).resized

        counter2 := counter2 + 1
        when(counter2 + 1 === sizePolynom) {
          if (toMod) {
            outp.io.rAddress2 := mod(counter2 + 1)
          }
          cMult2.io.rAddress1 := 0
          goto(getResult3)
        }
      }

      getResult3.whenIsActive {
        outp.io.wValid1 := True
        if (toMod) {
          outp.io.rAddress1 := mod(counter2)
          outp.io.wData1 := (outp.io.rData2.asSInt - (cMult2.io.rData1.asSInt + (cMult3.io.rData1.asSInt - cMult2.io.rData2.asSInt - cMult1.io.rData2.asSInt))).asBits
          outp.io.rAddress2 := mod(counter2 + 1)

        } else {
          outp.io.rAddress1 := counter2
          outp.io.wData1 := (cMult2.io.rData1.asSInt + (cMult3.io.rData1.asSInt - cMult2.io.rData2.asSInt - cMult1.io.rData2.asSInt)).asBits
        }
        cMult1.io.rAddress2 := (counter2 + 1 - sizePolynom / 2).resized
        cMult2.io.rAddress2 := (counter2 + 1 - sizePolynom / 2).resized
        cMult3.io.rAddress1 := (counter2 + 1 - sizePolynom / 2).resized

        cMult2.io.rAddress1 := (counter2 + 1 - sizePolynom).resized

        counter2 := counter2 + 1
        when(counter2 + 1 === sizePolynom + sizePolynom / 2 - 1) {
          goto(getResult4)
        }
      }

      getResult4.whenIsActive {
        outp.io.wValid1 := True

        if (toMod) {
          outp.io.rAddress1 := mod(counter2)
          outp.io.rAddress2 := mod(counter2 + 1)
          outp.io.wData1 := (outp.io.rData2.asSInt - cMult2.io.rData1.asSInt).asBits
        } else {
          outp.io.rAddress1 := counter2
          outp.io.wData1 := cMult2.io.rData1
        }

        cMult2.io.rAddress1 := (counter2 + 1 - sizePolynom).resized
        counter2 := counter2 + 1

        when(counter2 + 1 === 2 * sizePolynom - 1) {
          ready := True
          goto(idle)
        }
      }

      reset.whenIsActive{
        outp.io.rAddress1 := counter2.resized
        poly1Reg.io.rAddress1 := counter2.resized
        poly2Reg.io.rAddress1 := counter2.resized
        outp.io.wValid1 := True
        poly1Reg.io.wValid1 := True
        poly2Reg.io.wValid1 := True
        outp.io.wData1.clearAll
        poly1Reg.io.wData1.clearAll
        poly2Reg.io.wData1.clearAll
        counter2 := counter2 + 1
        when(counter2 === 2 * sizePolynom) {
          ready := True
          goto(idle)
        }
      }
    }

  }
    // eine start vereinfachte Polynom modulo Funktion
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


object Test {
  def main(args: Array[String]): Unit = {
    SpinalVhdl(new Karatsuba(16, 1024, 32, true)).printPruned()
  }
}