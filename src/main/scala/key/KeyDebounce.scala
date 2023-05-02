package key

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.sim._

class KeyDebounce(clockFreq:Int=50, debounceTime:Int=20, pulseLength:Int=5) extends Component {
/*
clockFreq in MHz, debounceTime in ms, if the key_in continue a debounceTime, the key_out get a pulseLength of pulseLength of clock
 */
  val key_in = in Bool()
  val key_out = out Bool()
  val key_out_reg = RegInit(False)
  val keyFall = RegInit(False)
  val keyRise = RegInit(False)
  keyFall := key_in.fall()
  keyRise := key_in.rise()
  val totalCount = clockFreq * 1000 * debounceTime
  val fsm = new StateMachine {
    val IDLE = new State with EntryPoint
    val COUNT = new State
    val SEND = new StateDelay(pulseLength)
    val timeCounter = Reg(UInt(log2Up(totalCount) bits)) init(0)

    IDLE
      .onEntry(timeCounter.clearAll())
      .whenIsActive{
        key_out_reg := False
        when(keyFall){
          goto(COUNT)
        }
      }
    COUNT
      .onEntry(timeCounter.clearAll())
      .whenIsActive{
        when(!key_in & !(timeCounter === totalCount)){
          timeCounter := timeCounter + 1
        }.elsewhen(key_in & !(timeCounter === totalCount)){
          goto(IDLE)
        }.otherwise(goto(SEND))
      }
    SEND
      .onEntry(key_out_reg := True)
      .whenCompleted(goto(IDLE))
  }
  key_out := key_out_reg
}


object KeyDebounceSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.doSim(new KeyDebounce()) {dut=>
      dut.clockDomain.forkStimulus(20)
      dut.key_in #= true
      sleep(400)
      dut.key_in #= true
      def keyDown(Time:Int) : Unit={
        sleep(400)
        dut.key_in #= false
        sleep(1000000 * Time)
        dut.key_in #= true
        sleep(1100000 * 2)
      }
      keyDown(21)
      keyDown(19)
      keyDown(25)
    }
  }
}