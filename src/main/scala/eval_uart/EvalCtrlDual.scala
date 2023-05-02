package eval_uart

import spinal.core._
import spinal.lib._

class EvalCtrlDual(clockDivide : Int=16, dataWidth : Int=24, niCtrl:Boolean=false) extends Component{
  val io1 = new Bundle{
    val ss = out Bool()
    val mosi = out Bool()
    val sclk = out Bool()
    val key = in Bool()
    val ldac_n = out Bool()
  }
  val io2 = new Bundle{
    val ss = out Bool()
    val mosi = out Bool()
    val sclk = out Bool()
    val key = in Bool()
    val ldac_n = out Bool()
  }
  val rxd = in Bool()
  val cntRst = in Bool()

  val evalCtrl1 = new EvalCtrl(clockDivide = clockDivide, dataWidth=dataWidth, niCtrl=niCtrl, logo = '$')
  evalCtrl1.io <> io1
  evalCtrl1.rxd <> rxd
  evalCtrl1.cntRst <> cntRst
  val evalCtrl2 = new EvalCtrl(clockDivide=clockDivide, dataWidth=dataWidth, niCtrl=niCtrl, logo = '%')
  evalCtrl2.io <> io2
  evalCtrl2.rxd <> rxd
  evalCtrl2.cntRst <> cntRst
}

object EvalCtrlDualVerilog{
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = Verilog,
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
                                                    clockEdge = RISING,
                                                    resetActiveLevel = LOW),
                                                  targetDirectory="verilog/")
      .generate(new EvalCtrlDual(niCtrl = true))
  }
}