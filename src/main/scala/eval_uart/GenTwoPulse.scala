package eval_uart

import spinal.core._
import spinal.lib._
import convert.binaryToDec
import eval_spi.EvalSpiCtrl
import key.KeyDebounce
import spinal.core.Component.push
import spinal.core.fiber.Handle
import spinal.core.sim._
import spinal.lib.fsm._
import myfunc.Delay.PulseByCycle

import scala.math
import scala.math

class GenTwoPulse(clockDivide : Int=6, dataWidth : Int=32, niCtrl:Boolean=false, logo : Char = '(') extends Component {

  val rxd = in Bool()
  val key = in Bool()
  val ctrl = out Bool()
  val led = out Bits(4 bits)

  val led_reg = Reg(Bits(4 bits)) init(B"0000")
  led := led_reg

  val mem_wordCont = 16
  val initValue = Math.pow(0, 19).toInt
  val spiDataMem = Mem(Bits(dataWidth-4 bits), wordCount = mem_wordCont) init(Vector(initValue, initValue, initValue, initValue, initValue, initValue, initValue, initValue, initValue, initValue, initValue, initValue, initValue, initValue, initValue, initValue))  // 用于存储Spi的数据

  val time1 = Reg(Bits(dataWidth-4 bits))
  val time2 = Reg(Bits(dataWidth-4 bits))
  val time3 = Reg(Bits(dataWidth-4 bits))

  val adress1 = Reg(UInt(4 bits)) init(0)
  val adress2 = Reg(UInt(4 bits)) init(1)
  val adress3 = Reg(UInt(4 bits)) init(2)

  time1 := spiDataMem.readSync(address = adress1, enable = True)
  time2 := spiDataMem.readSync(address = adress2, enable = True)
  time3 := spiDataMem.readSync(address = adress3, enable = True)

  val uartCtrl = new EvalUartCtrl(dataWidthMax = 8, baudRate = 115200)
  uartCtrl.io.uart.rxd <> rxd
  val commandSize = 12 // 指令的长度
  val rxGrocery = Vec(Reg(Bits(8 bits)) init(0), commandSize)
  val rxStoreValid = Bool()

  //移位寄存器
  when(uartCtrl.io.read.valid & rxStoreValid){
    for (i <- 0 to commandSize - 2){
      rxGrocery(i+1) := rxGrocery(i)
    }
    rxGrocery(0) := uartCtrl.io.read.payload
  }

  //ASCII码转换函数
  def asciiConvert(input8Bits : Bits) : UInt = {
    val ascii = Reg(UInt(4 bits)) init(15)
      switch(input8Bits){
        is('0'.toInt){ascii := 0}
        is('1'.toInt)(ascii := 1)
        is('2'.toInt)(ascii := 2)
        is('3'.toInt)(ascii := 3)
        is('4'.toInt)(ascii := 4)
        is('5'.toInt)(ascii := 5)
        is('6'.toInt)(ascii := 6)
        is('7'.toInt)(ascii := 7)
        is('8'.toInt)(ascii := 8)
        is('9'.toInt)(ascii := 9)
        is('A'.toInt)(ascii := 10)
        is('B'.toInt)(ascii := 11)
        is('C'.toInt)(ascii := 12)
        is('D'.toInt)(ascii := 13)
        is('E'.toInt)(ascii := 14)
        is('F'.toInt)(ascii := 15)
  }
    ascii
  }

  def string2Int(str:String) : Int = {
    var result = 0
    for (i <- 3 downto 0){
      result = result + str(i) * Math.pow(256, i).toInt
    }
    result
  }

  val delayFsm = new StateMachine{
    val IDLE = new State with EntryPoint
    val PULSE1 = new State
    val EVOLUTION = new State
    val PULSE2 = new State

    val totalCount = Math.pow(2, dataWidth-4).toInt
    val timeCounter = Reg(UInt(log2Up(totalCount) bits)) init(0)
    val clockSeparate = 50 //50时钟一个计数
    val clockCounter = Reg(UInt(log2Up(clockSeparate) bits)) init(0)

    val ctrlOutput = Reg(Bool()) init(False)

    IDLE.onEntry{
      timeCounter.clearAll()
      clockCounter.clearAll()
      ctrlOutput := False
      led_reg := B"0001"
    }
      .whenIsActive{
        when(PulseByCycle(key, 2)){
          goto(PULSE1)
        }
      }

    PULSE1.onEntry{
      timeCounter.clearAll()
      clockCounter.clearAll()
      led_reg := B"0010"
    }
      .whenIsActive{
        ctrlOutput := True
        clockCounter := clockCounter + 1
        when((clockCounter === clockSeparate-1) & !(timeCounter === time1.asUInt)){
          clockCounter := 0
          timeCounter := timeCounter + 1
        }.elsewhen(timeCounter === time1.asUInt){
          goto(EVOLUTION)
        }
      }

    EVOLUTION.onEntry{
      timeCounter.clearAll()
      clockCounter.clearAll()
      led_reg := B"0100"
    }
      .whenIsActive{
        ctrlOutput := False
        clockCounter := clockCounter + 1
        when((clockCounter === clockSeparate-1) & !(timeCounter === time2.asUInt)){
          clockCounter := 0
          timeCounter := timeCounter + 1
        }.elsewhen(timeCounter === time2.asUInt){
          goto(PULSE2)
        }
      }

    PULSE2.onEntry{
      timeCounter.clearAll()
      clockCounter.clearAll()
      led_reg := B"1000"
    }
      .whenIsActive{
        ctrlOutput := True
        clockCounter := clockCounter + 1
        when((clockCounter === clockSeparate-1) & !(timeCounter === time3.asUInt)){
          clockCounter := 0
          timeCounter := timeCounter + 1
        }.elsewhen(timeCounter === time3.asUInt){
          goto(IDLE)
        }
      }
  }

  ctrl := delayFsm.ctrlOutput

  val fsm = new StateMachine{
    val IDLE = new State with EntryPoint
    val ACTION = new State
//    val QUERY = new State
    val SET = new StateDelay(4)
    val WAIT = new State
//    val HOLD = new StateDelay(4)

    val dataWriteEnable = Reg(Bool())

    val groceryValid = RegInit(False)
    rxStoreValid := groceryValid

    IDLE
      .whenIsActive{
        groceryValid := True
        when((rxGrocery(commandSize-1) === logo.toByte) | (rxGrocery(commandSize-1) === '?'.toByte))(goto(ACTION))
      }
      .onExit(groceryValid := False)
    ACTION
      .whenIsActive{
//        when(rxCtrl === string2Vec("#set"))(goto(SET))
        switch(rxGrocery(commandSize-4)##rxGrocery(commandSize-3)##rxGrocery(commandSize-2)##rxGrocery(commandSize-1)){
          is(string2Int(Array(logo.toString, "set").mkString(""))){
            goto(SET)
          }
        }
      }
    SET
      .onEntry(dataWriteEnable := True)
      .whenCompleted{
        dataWriteEnable := False
        groceryValid := True //重新可以写入串口
        goto(WAIT)
      }
    WAIT
      .whenIsActive{
        when(!(rxGrocery(commandSize-1) === logo.toByte) & !(rxGrocery(commandSize-1) === '?'.toByte))(goto(IDLE))
      }

    spiDataMem.write(
      address = asciiConvert(rxGrocery(commandSize-5)),
      data = asciiConvert(rxGrocery(commandSize-6)).asBits ## asciiConvert(rxGrocery(commandSize-7)).asBits ## asciiConvert(rxGrocery(commandSize-8)).asBits ## asciiConvert(rxGrocery(commandSize-9)).asBits ## asciiConvert(rxGrocery(commandSize-10)).asBits ## asciiConvert(rxGrocery(commandSize-11)).asBits ## asciiConvert(rxGrocery(commandSize-12)).asBits,
      enable = dataWriteEnable
    )
  }
}

object GenTwoPulseSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.doSim(new GenTwoPulse(niCtrl = true)){dut=>
      dut.clockDomain.forkStimulus(20)
      dut.key #= false
      dut.rxd #= true
      sleep(200000)
      def senUart(data:Int=0): Unit ={
        var tmpData = data
        dut.rxd #= true
        sleep(8680)
        dut.rxd #= false
        sleep(8680)
        for (i <- 0 to 7){
          if (tmpData % 2==1){
            tmpData = (tmpData-1) / 2
            dut.rxd #= true
            sleep(8680)
          }else{
            tmpData = tmpData / 2
            dut.rxd #= false
            sleep(8680)
          }
        } // 循环触发
        dut.rxd #= true
        sleep(8680)
      }
      def sendCommand(str:String) : Unit={
      for(i <- str){
        senUart(i.toByte)
      }}
      def key_trig : Unit={
        dut.key #= true
        sleep(50)
        dut.key #= false
        sleep(210000)}
      key_trig
      sleep(200000)
      sendCommand(")set000010")
      sendCommand(")set100050")
      sendCommand(")set200030")
      sleep((100000))
      key_trig
      key_trig
      key_trig
    }
  }
}

object GenTwoPulseVerilog {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = Verilog,
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
                                                    clockEdge = RISING,
                                                    resetActiveLevel = LOW),
                                                  targetDirectory="verilog")
    .generate(new GenTwoPulse(niCtrl = true))
  }
}