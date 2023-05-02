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

import scala.math
import scala.math

class EvalCtrl(clockDivide : Int=6, dataWidth : Int=24, niCtrl:Boolean=false, logo : Char = '#') extends Component {
  val batchTime = clockDivide * (dataWidth+4) * 2
  val CMD : Int = binaryToDec.convert("001000000000000000010010")
  val SET : Int = binaryToDec.convert("000110000000000000000000")
  val rxd = in Bool()
  val cntRst = in Bool()
  val io = new Bundle{
//    val txd = out Bool()
  val ss = out Bool()
  val mosi = out Bool()
  val sclk = out Bool()
  val key = in Bool()
  val ldac_n = out Bool()
  }

  val ldacReg = RegInit(False)
  ldacReg := myfunc.Delay.PulseByCycle(io.ss.rise(), 4)
  io.ldac_n := !myfunc.Delay.PulseByCycle(ldacReg.rise(), 4)

  val initialSpiMaster = new EvalSpiCtrl(dataWidth, clockDivide)
  val initData = Reg(UInt(dataWidth bits))
  val initialGo = Reg(Bool())
  val initialMask = Reg(Bool())
  val initialCounter = Reg(UInt(log2Up(batchTime * 20 + 4) bits)) init(0) // +4是为了保证有batchTime这个值
  when (!(initialCounter === batchTime * 20)){
    initialCounter := initialCounter + 1
    switch(initialCounter){
      is(batchTime) {
        initialMask := True
        initialGo := False
        initData := CMD
        }
      is(12*batchTime) {
        initialGo := True}
      is(13*batchTime) {
        initialGo := False
        initData := SET
      }
      is(14*batchTime) {initialGo := True}
      is(15*batchTime) {initialGo := False}
      is(16*batchTime-1) {initialMask := False}
    }
  }

  initialSpiMaster.io.go <> initialGo
  initialSpiMaster.io.data <> initData.asBits

  val init_ss = Bool()
  val init_sclk = Bool()
  val init_mosi = Bool()

  init_ss := initialSpiMaster.io.ss
  init_sclk := initialSpiMaster.io.sclk
  init_mosi := initialSpiMaster.io.mosi

  val mem_wordCont = 16
  val initValue = Math.pow(2, 19).toInt // 该值对应于DAC 电压0V
  // 创建寄存器用于存储电压值
  val spiDataMem = Mem(Bits(dataWidth-4 bits), wordCount = mem_wordCont) init(Vector(initValue, initValue, initValue, initValue, initValue, initValue, initValue, initValue, initValue, initValue, initValue, initValue, initValue, initValue, initValue, initValue))  // 用于存储Spi的数据


  val uartCtrl = new EvalUartCtrl(dataWidthMax = 8, baudRate = 115200)
  uartCtrl.io.uart.rxd <> rxd
  val commandSize = 10 // 指令的长度,是一个队列,First in first out
  val rxGrocery = Vec(Reg(Bits(8 bits)) init(0), commandSize)
  val rxStoreValid = Bool()
  when(uartCtrl.io.read.valid & rxStoreValid){
    for (i <- 0 to commandSize - 2){
      rxGrocery(i+1) := rxGrocery(i)
    }
    rxGrocery(0) := uartCtrl.io.read.payload
  }

  // 将0-F转换为4位二进制
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


  val dataHead = Reg(UInt(4 bits)) init(1)


  def string2Int(str:String) : Int = {
    var result = 0
    for (i <- 3 downto 0){
      result = result + str(i) * Math.pow(256, i).toInt
    }
    result
  }

  val force_go = Reg(Bool()) init(False)
  val force_ss = Reg(Bool()) init(True)

  val fsm = new StateMachine{
    val IDLE = new State with EntryPoint
    val ACTION = new State
//    val QUERY = new State
    val SET = new StateDelay(4)
    val FORCE = new StateDelay(4)
    val WAIT = new State
//    val HOLD = new StateDelay(4)

    val dataWriteEnable = Reg(Bool())

    val groceryValid = RegInit(False)
    rxStoreValid := groceryValid
    //逻辑状态
    //初始是IDLE
    //IDLE状态下如果rxGrocery最后一个寄存器 接收到logo字符就跳到Action
    //action中检测到后2-4个的指令，跳转到对应的指令中
    IDLE
      .whenIsActive{
        groceryValid := True
        when((rxGrocery(commandSize-1) === logo.toByte) | (rxGrocery(commandSize-1) === '?'.toByte))(goto(ACTION))
      }
      .onExit(groceryValid := False)
    ACTION
      .whenIsActive{
        switch(rxGrocery(commandSize-4)##rxGrocery(commandSize-3)##rxGrocery(commandSize-2)##rxGrocery(commandSize-1)){
          is(string2Int(Array(logo.toString, "set").mkString(""))){
            goto(SET)
          }
          is(string2Int(Array(logo.toString, "fce").mkString(""))){
            goto(FORCE)
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
    FORCE // 强制设置值
      .onEntry{
        force_go.set()
      }
      .whenCompleted{
        force_go.clear()
        groceryValid := True //重新可以写入串口
        goto(WAIT)
      }
    WAIT
      .whenIsActive{
        when(!(rxGrocery(commandSize-1) === logo.toByte) & !(rxGrocery(commandSize-1) === '?'.toByte))(goto(IDLE))
      }

    spiDataMem.write(
      address = asciiConvert(rxGrocery(commandSize-5)),
      data = asciiConvert(rxGrocery(commandSize-6)).asBits ## asciiConvert(rxGrocery(commandSize-7)).asBits ## asciiConvert(rxGrocery(commandSize-8)).asBits ## asciiConvert(rxGrocery(commandSize-9)).asBits ## asciiConvert(rxGrocery(commandSize-10)).asBits,
      enable = dataWriteEnable
    )
  }
  val key_go = RegInit(False) //按键整形结果
  val cntRst_go = RegInit(False)
  if (niCtrl){
    key_go := io.key
    cntRst_go := cntRst
  }else {
    val key_deb = new KeyDebounce(50, 20, 2) // 到时如果安装在板卡控制时，则需要把debounce time弄小或者取消
    val cntRst_deb = new KeyDebounce(50, 20, 10) // 到时如果安装在板卡控制时，则需要把debounce time弄小或者取消
    key_deb.key_in := io.key
    key_deb.key_out <> key_go // 2个周期的trig
    cntRst_deb.key_in := cntRst
    cntRst_deb.key_out <> cntRst_go // 2个周期的trig
  }
  val batchCycle = dataWidth * clockDivide
  val spiGo : Bool = myfunc.Delay.PulseByCycle(key_go, batchCycle)

  val spiMemAdr = Counter(0 to (mem_wordCont - 1))  // spi读取的内存地址
  val eval_ss = Bool()
  val eval_sclk = Bool()
  val eval_mosi = Bool()
  when(!initialMask & eval_ss.rise() & !cntRst_go) {
    spiMemAdr.value := spiMemAdr.value + 1
  }.elsewhen(cntRst_go){
    spiMemAdr.value := 0
  }
  val evalSpiCtrl = new EvalSpiCtrl(dataWidth = dataWidth, ClockDivide = clockDivide)
  evalSpiCtrl.io.ss <> eval_ss
  evalSpiCtrl.io.go := spiGo.rise()
  evalSpiCtrl.io.data := dataHead.asBits ## spiDataMem.readSync(
    enable = spiGo,
    address = spiMemAdr  //内存地址
  )
  evalSpiCtrl.io.sclk <> eval_sclk
  evalSpiCtrl.io.mosi <> eval_mosi

  val force_sclk = Bool()
  val force_mosi = Bool()
  val force_mask = Reg(Bool()) init(False)
  force_mask := myfunc.Delay.PulseByCycle(force_go, batchTime)
  val forceSpiCtrl = new EvalSpiCtrl(dataWidth=dataWidth, ClockDivide = clockDivide)
  forceSpiCtrl.io.go <> force_go
  forceSpiCtrl.io.ss <> force_ss
  forceSpiCtrl.io.data <> dataHead.asBits ## asciiConvert(rxGrocery(commandSize-6)).asBits ## asciiConvert(rxGrocery(commandSize-7)).asBits ## asciiConvert(rxGrocery(commandSize-8)).asBits ## asciiConvert(rxGrocery(commandSize-9)).asBits ## asciiConvert(rxGrocery(commandSize-10)).asBits
  forceSpiCtrl.io.sclk <> force_sclk
  forceSpiCtrl.io.mosi <> force_mosi
//  io.ss := (eval_ss & force_ss & !initialMask) | (init_ss & initialMask)
//  io.sclk := (eval_sclk & !initialMask) | (init_sclk & initialMask) | (force_sclk & !initialMask)
//  io.mosi := (eval_mosi & !initialMask) | (init_mosi & initialMask) | (force_mosi & !initialMask)

  io.ss := (eval_ss & !initialMask & !force_mask) | (init_ss & initialMask & !force_mask) | (force_ss & !initialMask & force_mask)
  io.sclk := (eval_sclk & !initialMask & !force_mask) | (init_sclk & initialMask & !force_mask) | (force_sclk & !initialMask & force_mask)
  io.mosi := (eval_mosi & !initialMask & !force_mask) | (init_mosi & initialMask & !force_mask) | (force_mosi & !initialMask & force_mask)

}

object EvalCtrlSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.doSim(new EvalCtrl(niCtrl = true)){dut=>
      dut.clockDomain.forkStimulus(20)
      dut.io.key #= false
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
      sendCommand("#set012345")
      sendCommand("#set154321")
      sendCommand("#fce003956")
      sleep((10000))
      sendCommand("#fce013956")
      sleep((10000))
      def key_trig : Unit={
      dut.io.key #= true
      sleep(50)
      dut.io.key #= false
      sleep(21000)}
      key_trig
      key_trig
      key_trig
    }
  }
}

object EvalCtrlSimKey {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.doSim(new EvalCtrl(niCtrl = false)){dut=>
      dut.clockDomain.forkStimulus(20)
      dut.io.key #= true
      dut.rxd #= true
      dut.cntRst #= true
      sleep(50000)
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
      val sendCommand = "fdsa#set0FF1F6jkldsfj"
      for(i <- sendCommand){
        senUart(i.toByte)
      }
      dut.io.key #= false
      sleep(21000000)
      dut.io.key #= true
      sleep(20000)
    }
  }
}

object EvalCtrlVerilog {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = Verilog,
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
                                                    clockEdge = RISING,
                                                    resetActiveLevel = LOW),
                                                  targetDirectory="verilog")
    .generate(new EvalCtrl(niCtrl = true))
  }
}