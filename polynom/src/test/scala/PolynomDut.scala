import spinal.core.sim._
import spinal.core._
import spinal.sim._

object PolynomDut {
  def main(args: Array[String]): Unit = {

    SimConfig.withWave.allOptimisation.compile(new classicMul(16, 8, true)).doSim {dut =>

      dut.clockDomain.forkStimulus(10)
      dut.io.start #= false
      dut.io.rValid1 #= false
      dut.io.rAddress1 #= 0
      dut.io.rValid2 #= false
      dut.io.rAddress2 #= 0
      dut.io.reset #=true
      dut.clockDomain.waitRisingEdge(1)
      dut.io.reset #=false
      dut.clockDomain.waitRisingEdge(3000)
      dut.clockDomain.waitRisingEdge(10)
      dut.io.wValid1 #= true
      dut.io.wAddress1 #= 0
      dut.io.wData1 #= 1
      dut.io.wValid2 #= true
      dut.io.wAddress2 #= 0
      dut.io.wData2 #= 1
      dut.clockDomain.waitRisingEdge()

      dut.io.wValid1 #= true
      dut.io.wAddress1 #= 1
      dut.io.wData1 #= 2
      dut.io.wValid2 #= true
      dut.io.wAddress2 #= 1
      dut.io.wData2 #= 2

      dut.clockDomain.waitRisingEdge()

      dut.io.wValid1 #= true
      dut.io.wAddress1 #= 2
      dut.io.wData1 #= 3
      dut.io.wValid2 #= true
      dut.io.wAddress2 #= 2
      dut.io.wData2 #= 3
      dut.clockDomain.waitRisingEdge()

      dut.io.wValid1 #= true
      dut.io.wAddress1 #= 3
      dut.io.wData1 #= 4
      dut.io.wValid2 #= true
      dut.io.wAddress2 #= 3
      dut.io.wData2 #= 4
      dut.clockDomain.waitRisingEdge()

      dut.io.wValid1 #= true
      dut.io.wAddress1 #= 4
      dut.io.wData1 #= 5
      dut.io.wValid2 #= true
      dut.io.wAddress2 #= 4
      dut.io.wData2 #= 5
      dut.clockDomain.waitRisingEdge()

      dut.io.wValid1 #= true
      dut.io.wAddress1 #= 5
      dut.io.wData1 #= 6
      dut.io.wValid2 #= true
      dut.io.wAddress2 #= 5
      dut.io.wData2 #= 6
      dut.clockDomain.waitRisingEdge()

      dut.io.wValid1 #= true
      dut.io.wAddress1 #= 6
      dut.io.wData1 #= 7
      dut.io.wValid2 #= true
      dut.io.wAddress2 #= 6
      dut.io.wData2 #= 7
      dut.clockDomain.waitRisingEdge()

      dut.io.wValid1 #= true
      dut.io.wAddress1 #= 7
      dut.io.wData1 #= 8
      dut.io.wValid2 #= true
      dut.io.wAddress2 #= 7
      dut.io.wData2 #= 8
      dut.clockDomain.waitRisingEdge()

      dut.io.wValid1 #= false
      dut.io.wValid2 #= false


      dut.clockDomain.waitRisingEdge(10)
      dut.io.start #= true
      dut.clockDomain.waitRisingEdge()
      dut.io.start #= false
      dut.clockDomain.waitRisingEdge(1000)

    }
  }
}