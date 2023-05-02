package eval_uart

import spinal.core._
import spinal.lib._
import spinal.lib.com.uart._
import spinal.core.sim._

class EvalUartCtrl(dataWidthMax:Int=8, baudRate : Int = 115200) extends Component {
  val sampleSize : Int = 1
  val clockDivide = 50 * 1000000 / baudRate / 8

  var g = new UartCtrlGenerics(dataWidthMax = dataWidthMax,
    clockDividerWidth = 20,
    preSamplingSize = 1 * sampleSize,
    samplingSize = 5 * sampleSize,
    postSamplingSize = 2 * sampleSize)
  val io = new Bundle {
    val write  = slave(Stream(Bits(g.dataWidthMax bit)))
    val read   = master(Stream(Bits(g.dataWidthMax bit)))
    val uart   = master(Uart())
  }
  val config = UartCtrlConfig(g)
  val tx = new UartCtrlTx(g)
  val rx = new UartCtrlRx(g)

  //Clock divider used by RX and TX
  val clockDivider = new Area {
    val counter = Reg(UInt(g.clockDividerWidth bits)) init(0)
    val tick = counter === 0

    counter := counter - 1
    when(tick) {
      counter := config.clockDivider
    }
  }

  tx.io.samplingTick := clockDivider.tick
  rx.io.samplingTick := clockDivider.tick

  tx.io.configFrame := config.frame
  rx.io.configFrame := config.frame

  tx.io.write << io.write
  rx.io.read >> io.read

  io.uart.txd <> tx.io.txd
  io.uart.rxd <> rx.io.rxd
  tx.io.cts := False
  tx.io.break := False
  config.frame.dataLength := dataWidthMax - 1
  config.frame.stop := UartStopType.ONE
  config.frame.parity := UartParityType.NONE
  config.clockDivider := clockDivide
}

class EvalUartCtrlTest(dataWidthMax : Int=8) extends Component{
  val io = new Bundle{
    val data = in UInt(dataWidthMax bits)
    val valid = in Bool()
  }
  val validDelay = Bool()
  val uartMaster = new EvalUartCtrl(dataWidthMax)
  val uartSlave = new EvalUartCtrl(dataWidthMax)

  validDelay := myfunc.Delay.PulseByCycle(io.valid, 8 * 8 * uartMaster.clockDivide)
  uartMaster.io.write.valid <> validDelay
  uartMaster.io.write.payload := io.data.asBits
  uartMaster.io.uart.txd <> uartSlave.io.uart.rxd
  uartMaster.io.uart.rxd <> uartSlave.io.uart.txd
  uartSlave.io.write.payload := 0
  uartSlave.io.write.valid := False
}

object EvalUartCtrlTestSim{
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.doSim(new EvalUartCtrlTest(dataWidthMax = 8)) {dut=>
      dut.clockDomain.forkStimulus(20)
      sleep(400)
      dut.io.valid #= false
      def sendUart(data:Int):Unit = {
      dut.io.data #= data
      sleep(60)
      dut.io.valid #= true
      sleep(60)
      dut.io.valid #= false
      sleep(300000)}
      sendUart(1)
      sendUart(2)
      sendUart(4)
      sendUart(8)
      sendUart(16)
      sendUart(32)
      sendUart(64)
      sendUart(128)
    }
  }
}

object EvalUartCtrlTestVerilog {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = Verilog,
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
                                                    clockEdge = RISING,
                                                    resetActiveLevel = LOW),
                                                  targetDirectory="verilog")
    .generate(new EvalUartCtrl(8))
  }
}

object EvalUartCtrlVerilog {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = Verilog,
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
                                                    clockEdge = RISING,
                                                    resetActiveLevel = LOW),
                                                  targetDirectory="verilog")
    .generate(new EvalUartCtrl)
  }
}