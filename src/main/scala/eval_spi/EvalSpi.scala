package eval_spi

import spinal.core._
import spinal.lib._
import com.spi._
import spinal.core.sim._
//import spinal.core.

case class EvalSpiCtrlIo(dataWidth:Int=24) extends Bundle with IMasterSlave {
  val ss = Bool()
  val mosi = Bool()
  val sclk = Bool()
  val go = Bool()  //触发
  val data = Bits(dataWidth bits)

  override def asMaster(): Unit = {
    in(go, data)
    out(ss, mosi, sclk)
  }
}

class EvalSpiCtrl(dataWidth : Int = 24, ClockDivide : Int=6) extends Component {
  val io = EvalSpiCtrlIo(dataWidth)
  io.asMaster()
  val evalSpiCtrlGeneric = SpiMasterCtrlGenerics(ssWidth = 1, timerWidth = log2Up(ClockDivide+1), dataWidth = dataWidth)
  val evalSpiCtrl = SpiMasterCtrl(evalSpiCtrlGeneric)
  val valid = Bool()
  valid := myfunc.Delay.PulseByCycle(io.go, (2*(ClockDivide+1)) * (dataWidth) + 1)
  io.ss := ~valid
  io.mosi <> evalSpiCtrl.io.spi.mosi
  io.sclk <> evalSpiCtrl.io.spi.sclk

  //configure
  evalSpiCtrl.io.config.kind.cpha := False
  evalSpiCtrl.io.config.kind.cpol := True
  evalSpiCtrl.io.config.sclkToogle := ClockDivide
  evalSpiCtrl.io.config.ss.activeHigh := IntToBits(1)
  evalSpiCtrl.io.config.ss.setup := 1
  evalSpiCtrl.io.config.ss.hold := 1
  evalSpiCtrl.io.config.ss.disable := 0
  evalSpiCtrl.io.cmd.valid := valid
  evalSpiCtrl.io.cmd.payload.mode := SpiMasterCtrlCmdMode.DATA
  val args = RegInit(False)
  evalSpiCtrl.io.cmd.payload.args := (args.asBits ## io.data)
}

object EvalSpiCtrlSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.doSim(new EvalSpiCtrl(8, 16)) {dut=>
      dut.clockDomain.forkStimulus(20)
      dut.io.go #= false
      sleep(200)
      def sendSpi(data:Int) : Unit = {
        sleep(400)
        dut.io.data #= data
        dut.io.go #= true
        sleep(40)
        dut.io.go #= false
        sleep(2000)
        dut.io.go #= false
        sleep(500)
      }
      sendSpi(1)
      sendSpi(2)
      sendSpi(4)
      sendSpi(8)
      sendSpi(16)
      sendSpi(32)
      sendSpi(64)
      sendSpi(128)
    }
  }
}

object EvalSpiCtrlVerilog {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = Verilog,
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
                                                    clockEdge = RISING,
                                                    resetActiveLevel = LOW),
                                                  targetDirectory="verilog/")
    .generate(new EvalSpiCtrl(24, 6))
  }
}