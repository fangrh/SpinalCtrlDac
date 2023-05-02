package myfunc

import spinal.core._
import spinal.lib._

object Delay extends Component {
  def PulseByCycle(signal: Bool, cycle:Int) : Bool={
    /*
    Detect the rising of signal, and return a signal with a delay of cycles
     */
    val Pulse = RegInit(False)
    val clkCnt = Reg(UInt(log2Up(cycle) bits)) init(0)
    val clkCnt_will_increase = RegInit(False)
    val tmp_signal = Bool()
    val res = Bool()
    tmp_signal := signal
    when (tmp_signal.rise()){
      Pulse.set()
      clkCnt_will_increase := True
      clkCnt := 0
    }
    when (clkCnt_will_increase){
      clkCnt := clkCnt + 1
      when(clkCnt === (cycle -1)){
        Pulse.clear()
        clkCnt_will_increase := False
        clkCnt := 0
      }
    }
    res := Pulse
    res
  }
}
