package eval_uart

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.com.uart._

import java.awt.image.SampleModel

class EvalUartTx extends Component {
  val clockDivideWidth : Int = 20 //50 * 1000000 / 115200
  val sampleSize : Int = 50 * 1000000 / 115200 / 8
  val io = new Bundle{
    val txd = out Bool()
    val go = in Bool()
    val data = in UInt(8 bits)
//    val write_payload =
  }
  val goReg = RegInit(False)
  goReg := io.go.rise()
  val goDelay = myfunc.Delay.PulseByCycle(io.go, sampleSize * 8 * 8)
  var g = new UartCtrlGenerics(dataWidthMax = 8,
    clockDividerWidth = clockDivideWidth,
    preSamplingSize = 1 * sampleSize,
    samplingSize = 5 * sampleSize,
    postSamplingSize = 2 * sampleSize)
  val evalUartCtrl = new UartCtrlTx(g)
  io.txd <> evalUartCtrl.io.txd

  //configuration
  evalUartCtrl.io.configFrame.dataLength := 7
  evalUartCtrl.io.configFrame.stop := UartStopType.ONE
  evalUartCtrl.io.configFrame.parity := UartParityType.NONE
  evalUartCtrl.io.samplingTick := True
  evalUartCtrl.io.write.valid := goDelay
  evalUartCtrl.io.write.payload := io.data.asBits
  evalUartCtrl.io.cts := False
  evalUartCtrl.io.break := False
}

object EvalUartTxSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.doSim(new EvalUartTx){dut=>
      dut.clockDomain.forkStimulus(20)
      dut.io.go #= false
      sleep(400)
      dut.io.data #= 2
      dut.io.go #= true
      sleep(60)
      dut.io.go #= false
      sleep(2500000)
    }
  }
}

object EvalUartTxVerilog {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = Verilog,
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
        clockEdge = RISING,
        resetActiveLevel = LOW),
      targetDirectory="verilog")
      .generate(new EvalUartTx())
  }
}
