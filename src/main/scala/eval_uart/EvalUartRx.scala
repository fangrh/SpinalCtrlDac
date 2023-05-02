package eval_uart

import spinal.core._
import spinal.lib._
import spinal.lib.com.uart._

class EvalUartRx extends Component {
  val clockDivideWidth : Int = 20
  val sampleSize : Int = 1//50 * 1000000 / 115200 / 8 + 1
  val io = new Bundle{
    val rxd = in Bool()
  }
  var g = new UartCtrlGenerics(dataWidthMax = 8,
              clockDividerWidth = clockDivideWidth,
              preSamplingSize = 1 * sampleSize,
              samplingSize = 5 * sampleSize,
              postSamplingSize = 2 * sampleSize)
  val evalUartCtrl = new UartCtrlRx(g)

//  evalUartCtrl.io.read.payload.asOutput()
//  evalUartCtrl.io.read.valid.asOutput()
  io.rxd <> evalUartCtrl.io.rxd
  evalUartCtrl.io.read.ready <> True
  //configuration
  evalUartCtrl.io.configFrame.dataLength := 7
  evalUartCtrl.io.configFrame.stop := UartStopType.ONE
  evalUartCtrl.io.configFrame.parity := UartParityType.NONE
  evalUartCtrl.io.samplingTick := False
}

object EvalUartRxVerilog {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = Verilog,
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
                                                    clockEdge = RISING,
                                                    resetActiveLevel = LOW),
                                                  targetDirectory="verilog")
    .generate(new EvalUartRx)
  }
}