package eval_uart

import spinal.core.{ASYNC, BOOT, ClockDomainConfig, LOW, RISING, SpinalConfig, Verilog}

object EvalRampVerilog {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = Verilog,
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
        clockEdge = RISING,
        resetActiveLevel = LOW),
      targetDirectory="../src")
      .generate(new EvalRamp(rampCycle = 1000, niCtrl = false, clockDivide = 32)) // rampCycle 每个ramp的周期，单位为us
  }
}
