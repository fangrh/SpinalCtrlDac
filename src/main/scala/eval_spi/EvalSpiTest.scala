package eval_spi

import key.KeyDebounce
import spinal.core._
import spinal.lib._
import spinal.core.sim._

class EvalSpiCtrlTest(dataWidth:Int=24, ClockDivide:Int=6) extends Component {
  //parameters
  val words : Int = 8

  //IO口声明
  val evalIo = EvalSpiCtrlIo(dataWidth)
  evalIo.ss.asOutput()
  evalIo.sclk.asOutput()
  evalIo.mosi.asOutput()

  //按键声明与整形
  val key = in Bool()
  val key_go = Bool() //按键整形结果
  val key_deb = new KeyDebounce(50, 20, 2)
  key_deb.key_in <> key
  key_deb.key_out <> key_go  // 2个周期的trig
  evalIo.go := key_go

  val memAdr = Counter(0 to (words - 1))

  when(evalIo.ss.rise()) {
    memAdr.value := memAdr.value + 1
  }

  val evalSpiCtrl = new EvalSpiCtrl(dataWidth = dataWidth, ClockDivide = 4)
  evalIo <> evalSpiCtrl.io

  val spiMem = Mem(Bits(dataWidth bits), wordCount=words) init(Vector(1, 2, 4, 8, 16, 32, 64, 128))
  evalIo.data := spiMem.readSync(
    enable = key_go,
    address = memAdr
  )
}



object EvalSpiCtrlTestSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.doSim(new EvalSpiCtrlTest()) {dut=>
      dut.clockDomain.forkStimulus(20)
      dut.key #= true
      sleep(400)
      def keyDown :Unit={
      dut.key #= false
      sleep(21000000)
      dut.key #= true
      sleep(20000)}
      keyDown
      keyDown
      keyDown
      dut.clockDomain.assertReset()
      keyDown
      keyDown
      keyDown
    }
  }
}

object EvalSpiCtrlTestVerilog {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = Verilog,
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
                                                    clockEdge = RISING,
                                                    resetActiveLevel = LOW),
                                                  targetDirectory="verilog/")
    .generate(new EvalSpiCtrlTest(24, 4))
  }
}
