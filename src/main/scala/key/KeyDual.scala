package key

import spinal.core._
import spinal.lib._

class KeyDual extends Component {
  val io = new Bundle{
    val key1 = in Bool()
    val pulse1 = out Bool()
    val key2 = in Bool()
    val pulse2 = out Bool()
  }
  val keyDebounce1 = new KeyDebounce(pulseLength = 10)
  val keyDebounce2 = new KeyDebounce(pulseLength = 10)
  keyDebounce1.key_in <> io.key1
  keyDebounce1.key_out <> io.pulse1
  keyDebounce2.key_in <> io.key2
  keyDebounce2.key_out <> io.pulse2
}


object KeyDualVer {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = Verilog,
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
                                                    clockEdge = RISING,
                                                    resetActiveLevel = LOW),
                                                  targetDirectory="../key/src")
    .generate(new KeyDual)
  }
}