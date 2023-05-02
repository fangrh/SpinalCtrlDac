package eval_uart

import eval_spi.EvalSpiCtrl
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.fsm._

class EvalRampTest(dataWidth : Int=24, clockDivide : Int=6, rampCycle : Int= 10) extends Component {
  val io = new Bundle{
    val key_go = in Bool()
  }

  val section = 16

  val initValue = Reg(UInt(20 bits)) init(0xF0000)
  val ramDirMem = Mem(UInt(1 bits), wordCount = section) init(Vec(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))  //ramp 方向
  val ramValueMem = Mem(UInt(20 bits), wordCount = section) init(Vec(1, 2, 0, 4, 5, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)) //ramp 大小
  val ramTimeMem = Mem(UInt(log2Up(1000000000/rampCycle) bits), wordCount = section) init(Vec(5, 11, 14, 18, 29, 0,0,0,0,0,0,0,0,0,0,0)) //ramp 时间,需要初始化为最大值

  val totalSectionCnt = Reg(UInt(log2Up(16) bits)) init(4)  // 总共的时间计数，单位是10 us

  val batchCycle = dataWidth * clockDivide  //spi需要的时钟周期
  val spiGo = Reg(Bool())

  val spiDataPre = Reg(UInt(4 bits)) init(1)  // spi data 开头
  val spiDataBody = Reg(UInt(20 bits)) init(0)  // spi data 的主体

//  val spiGo : Bool = myfunc.Delay.PulseByCycle(io.key_go, batchCycle)
//  val evalSpiCtrl = new EvalSpiCtrl(dataWidth = dataWidth, ClockDivide = clockDivide)
  val spiClkCnt = Counter(0 to (rampCycle * 1000 / 20 - 1))
  spiClkCnt.increment()

  val fsm_spi = new StateMachine{
    val IDLE = new State with EntryPoint
    val GoHIGH = new State
    val GoLOW = new State
    val JUDGE = new State

    val memReadEnable = RegInit(False)

    val currentCnt = Reg(UInt(log2Up(50*rampCycle) bits)) init(0)  //及时专用
    val currentRampCnt = Reg(UInt(log2Up(1000000000/rampCycle) bits)) init(0)
    val currentRampTime = Reg(UInt(log2Up(1000000000/rampCycle) bits)) init(0)
    val currentRampValue = Reg(UInt(20 bits)) init(0)
    val currentRampDir = RegInit(False)

    val currentSectionCnt = Reg(UInt(4 bits)) init(0)  // 纪录第几段


    val currentValue = Reg(UInt(20 bits)) init(initValue)

    currentRampTime := ramTimeMem.readSync(enable = memReadEnable, address = currentSectionCnt)
    currentRampValue := ramValueMem.readSync(enable = memReadEnable, address = currentSectionCnt)
    currentRampDir := ramDirMem.readSync(enable = memReadEnable, address = currentSectionCnt).asBool

    IDLE
      .onEntry{spiGo := False
        memReadEnable := True}
      .whenIsActive{
        when(io.key_go){
          currentCnt := spiClkCnt
          goto(GoHIGH)
        }
      }
      .onExit(memReadEnable := False)
    GoHIGH
      .whenIsActive{
        when(currentSectionCnt===totalSectionCnt) {
          goto(IDLE)
        }.elsewhen(spiClkCnt.value === (currentCnt + 2)){
          spiGo := True & (currentRampValue =/= 0)
          goto(GoLOW)
        }
      }
    GoLOW
      .whenIsActive{
        when(spiClkCnt.value === (currentCnt + 2 + batchCycle)){
          spiGo.clear()
          goto(JUDGE)
        }
      }
    JUDGE
      .onEntry{
        currentRampCnt := currentRampCnt + 1
        memReadEnable := True
        when(currentRampCnt === currentRampTime){
          currentSectionCnt := currentSectionCnt + 1
        }
      }
      .whenIsActive{
        when(spiClkCnt.value === (currentCnt + 2 + batchCycle + 4)){
        goto(GoHIGH)
        }
      }
      .onExit{
        memReadEnable := False
        when(currentRampDir) {
          currentValue := currentValue - currentRampValue
        }.otherwise(currentValue := currentValue + currentRampValue)
      }
  }
}


object EvalRampTestSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.doSim(new EvalRampTest()){dut=>
      dut.clockDomain.forkStimulus(20)
      dut.io.key_go #= false
      sleep(400)
      sleep(50000)
      dut.io.key_go #= true
      sleep(60)
      dut.io.key_go #= false
      sleep(1000000)
    }
  }
}

object EvalRampTestVerilog {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = Verilog,
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
                                                    clockEdge = RISING,
                                                    resetActiveLevel = LOW),
                                                  targetDirectory="verilog")
    .generate(new EvalRampTest())
  }
}