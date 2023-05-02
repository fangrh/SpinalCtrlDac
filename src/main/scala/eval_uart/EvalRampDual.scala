package eval_uart

import key.KeyDebounce
import spinal.core._
import spinal.lib._

class EvalRampDual(clockDivide : Int=32, dataWidth : Int=24, niCtrl : Boolean = false, rampCycle:Int=1000) extends Component {
  val rxd = in Bool()
  val io1 = new Bundle{
    val ss = out Bool()
    val mosi = out Bool()
    val sclk = out Bool()
    val key = in Bool()
    val button = in Bool()
    val ldac_n = out Bool()
  }
  val watch_dog = out Bits(4 bits)
  val io2 = new Bundle{
    val ss = out Bool()
    val mosi = out Bool()
    val sclk = out Bool()
    val key = in Bool()
    val button = in Bool()
    val ldac_n = out Bool()
  }

  val button1 = Bool()
  val key_deb1 = new KeyDebounce(50, 20, 2) // 到时如果安装在板卡控制时，则需要把debounce time弄小或者取消
  key_deb1.key_in := io1.button
  key_deb1.key_out <> button1 // 2个周期的trig

  val button2 = Bool()
  val key_deb2 = new KeyDebounce(50, 20, 2) // 到时如果安装在板卡控制时，则需要把debounce time弄小或者取消
  key_deb2.key_in := io2.button
  key_deb2.key_out <> button2 // 2个周期的trig

  val key1 = Bool()
  val key2 = Bool()

  key1 := io1.key | button1
  key2 := io2.key | button2

  val eval1 = new EvalRamp(clockDivide = clockDivide, dataWidth=dataWidth, niCtrl = niCtrl, rampCycle=rampCycle, logo = '!')
  eval1.io.rxd := rxd
  eval1.io.ss <> io1.ss
  eval1.io.mosi <> io1.mosi
  eval1.io.sclk <> io1.sclk
  eval1.io.key <> key1
  eval1.io.ldac_n <> io1.ldac_n
  eval1.watch_dog <> watch_dog
  val eval2 = new EvalRamp(clockDivide = clockDivide, dataWidth=dataWidth, niCtrl = niCtrl, rampCycle=rampCycle, logo = '@')
  eval2.io.rxd := rxd
  eval2.io.ss <> io2.ss
  eval2.io.mosi <> io2.mosi
  eval2.io.sclk <> io2.sclk
  eval2.io.key <> key2
  eval2.io.ldac_n <> io2.ldac_n
}



object EvalRampDualVerilog {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = Verilog,
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
                                                    clockEdge = RISING,
                                                    resetActiveLevel = LOW),
                                                  targetDirectory="verilog")
    .generate(new EvalRampDual(niCtrl = true))
  }
}