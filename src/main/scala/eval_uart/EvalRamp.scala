package eval_uart

import adder.CarryAdder
import spinal.core._
import spinal.lib._
import convert.binaryToDec
import eval_spi.EvalSpiCtrl
import key.KeyDebounce
import spinal.core.sim._
import spinal.lib.fsm._


class EvalRamp(clockDivide : Int=6, dataWidth : Int=24, niCtrl:Boolean=false, rampCycle:Int=20, logo:Char='@') extends Component {
  /*
  clockDivede : spi的时钟分频
  dataWidth : 发送SPI的位数，请勿改变
  niCtrl : false为用手按动，true为用ni控制
  ramCycle : ramp一次的周期，单位为us
  logo : 串口通讯标识符
   */
  val batchTime = clockDivide * (dataWidth+4) * 2
  val CMD : Int = binaryToDec.convert("001000000000000000010010")
  val SET : Int = binaryToDec.convert("000110000000000000000000")
  val io = new Bundle{
    //    val txd = out Bool()
    val rxd = in Bool()
    val ss = out Bool()
    val mosi = out Bool()
    val sclk = out Bool()
    val key = in Bool()
    val ldac_n = out Bool()
  }
  val watch_dog = out Bits(4 bits)

  val watch_dog_tmp = Reg(Bits(4 bits))

  val watch_dog_reg = Reg(Bits(4 bits))

  val watch_counter = Counter(0 to 50000000)
  watch_counter.increment()

  when(watch_counter === 0){
    watch_dog_tmp := watch_dog_reg
  }
  watch_dog := watch_dog_tmp

  val ldacReg = RegInit(False)
  ldacReg := myfunc.Delay.PulseByCycle(io.ss.rise(), 16)
  io.ldac_n := !myfunc.Delay.PulseByCycle(ldacReg.rise(), 16)

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

  val section = 16
  val rampInitValue = Reg(UInt(32 bits)) init(U"32'h80000000")
//  val rampInitValue = Reg(UInt(20 bits)) init(0x80000)
  val ramValueMem = Mem(UInt(32 bits), wordCount = section) init(Vec(500000, 1000000, 1500000, 2000000, 2000000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)) //ramp 大小
//  val ramTimeMem = Mem(UInt(log2Up(1000000000/rampCycle) bits), wordCount = section) init(Vec(5000, 11000, 15000, 18000, 0, 0,0,0,0,0,0,0,0,0,0,0)) //ramp 时间,需要初始化为最大值
  val ramTimeMem = Mem(UInt(32 bits), wordCount = section) init(Vec(1000, 1000, 1000, 1000, 0, 0,0,0,0,0,0,0,0,0,0,0)) //ramp 时间,需要初始化为最大值
  val totalSectionCnt = Reg(UInt(log2Up(16) bits)) init(4)  // 总共的时间计数，单位是10 us
  val spiClkCnt = Counter(0 to (rampCycle * 1000 / 20 - 1))
  spiClkCnt.increment()

  val uartCtrl = new EvalUartCtrl(dataWidthMax = 8, baudRate = 115200)
  uartCtrl.io.uart.rxd <> io.rxd
  val commandSize = 13 // 串口指令的长度
  val rxGrocery = Vec(Reg(Bits(8 bits)) init(0), commandSize)
  val rxStoreValid = Bool()
  when(uartCtrl.io.read.valid & rxStoreValid){
    for (i <- 0 to commandSize - 2){
      rxGrocery(i+1) := rxGrocery(i)
    }
    rxGrocery(0) := uartCtrl.io.read.payload
  }
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

  val fsm_uart = new StateMachine{
    val IDLE = new State with EntryPoint
    val ACTION = new State
    //    val QUERY = new State
    val INI = new State  // 设置初始值
    val VAL = new StateDelay(4)  // 设置Ramp值
    val TIM = new StateDelay(4) //设置时间
    val SEC = new State // 设置段数，默认5段
    val WAIT = new State
    //    val HOLD = new StateDelay(4)

    val valWriteEnable = Reg(Bool())
    val timWriteEnable = Reg(Bool())

    val groceryValid = RegInit(False)
    rxStoreValid := groceryValid

    IDLE
      .whenIsActive{
        groceryValid := True
        valWriteEnable := False
        timWriteEnable := False
        when((rxGrocery(commandSize-1) === logo.toByte) | (rxGrocery(commandSize-1) === '?'.toByte))(goto(ACTION))
      }
      .onExit(groceryValid := False)
    ACTION
      .whenIsActive{
        //        when(rxCtrl === string2Vec("#set"))(goto(SET))
        switch(rxGrocery(commandSize-4)##rxGrocery(commandSize-3)##rxGrocery(commandSize-2)##rxGrocery(commandSize-1)){
          is(string2Int(Array(logo.toString, "INI").mkString(""))){
            goto(INI)
          }
          is(string2Int(Array(logo.toString, "VAL").mkString(""))){
            goto(VAL)
          }
          is(string2Int(Array(logo.toString, "TIM").mkString(""))){
            goto(TIM)
          }
          is(string2Int(Array(logo.toString, "SEC").mkString(""))){
            goto(SEC)
          }
        }
      }
    INI
      .whenIsActive{
        rampInitValue := (asciiConvert(rxGrocery(commandSize-6)).asBits ## asciiConvert(rxGrocery(commandSize-7)).asBits ## asciiConvert(rxGrocery(commandSize-8)).asBits ## asciiConvert(rxGrocery(commandSize-9)).asBits ## asciiConvert(rxGrocery(commandSize-10)).asBits ## asciiConvert(rxGrocery(commandSize-11)).asBits ## asciiConvert(rxGrocery(commandSize-12)).asBits ## asciiConvert(rxGrocery(commandSize-13)).asBits).asUInt
        groceryValid := True
        goto(WAIT)
      }
    VAL
      .onEntry(valWriteEnable := True)
      .whenCompleted{
        valWriteEnable := False
        groceryValid := True
        goto(WAIT)
      }
    SEC
      .whenIsActive{
        totalSectionCnt := asciiConvert(rxGrocery(commandSize-13))
        groceryValid := True
        goto(WAIT)
      }
    TIM
      .onEntry(timWriteEnable := True)
      .whenCompleted{
        timWriteEnable := False
        groceryValid := True
        goto(WAIT)
      }
    WAIT
      .whenIsActive{
        when(!(rxGrocery(commandSize-1) === logo.toByte) & !(rxGrocery(commandSize-1) === '?'.toByte))(goto(IDLE))
      }
    ramValueMem.write(
      address = asciiConvert(rxGrocery(commandSize-5)),
      data = (asciiConvert(rxGrocery(commandSize-6)).asBits ## asciiConvert(rxGrocery(commandSize-7)).asBits ## asciiConvert(rxGrocery(commandSize-8)).asBits ## asciiConvert(rxGrocery(commandSize-9)).asBits ## asciiConvert(rxGrocery(commandSize-10)).asBits ## asciiConvert(rxGrocery(commandSize-11)).asBits ## asciiConvert(rxGrocery(commandSize-12)).asBits ## asciiConvert(rxGrocery(commandSize-13)).asBits).asUInt,
      enable = valWriteEnable
    )
    ramTimeMem.write(
      address = asciiConvert(rxGrocery(commandSize-5)),
      data = (asciiConvert(rxGrocery(commandSize-6)).asBits ## asciiConvert(rxGrocery(commandSize-7)).asBits ## asciiConvert(rxGrocery(commandSize-8)).asBits ## asciiConvert(rxGrocery(commandSize-9)).asBits ## asciiConvert(rxGrocery(commandSize-10)).asBits ## asciiConvert(rxGrocery(commandSize-11)).asBits ## asciiConvert(rxGrocery(commandSize-12)).asBits ## asciiConvert(rxGrocery(commandSize-13)).asBits).asUInt,
      enable = timWriteEnable
    )
  }

  val spiGo = RegInit(False)
  val batchCycle = (2*(clockDivide+1)) * (dataWidth+2) + 1  //spi需要的时钟周期

  val currentValue = Reg(UInt(32 bits)) init(rampInitValue)  // 当前的spi值

  val memReadEnable = RegInit(False)

  val currentCnt = Reg(UInt(log2Up(50*rampCycle) bits)) init(0)  //计时专用
  val currentRampCnt = Reg(UInt(32 bits)) init(0)  //进行多少次值改变
  val currentRampTime = Reg(UInt(32 bits)) init(0)
  val currentRampValue = Reg(UInt(32 bits)) init(0)

  val currentSectionCnt = Reg(UInt(4 bits)) init(0)  // 纪录第几段

  currentRampTime := ramTimeMem.readSync(enable = memReadEnable, address = currentSectionCnt)
  currentRampValue := ramValueMem.readSync(enable = memReadEnable, address = currentSectionCnt)

  val fsm_spi = new StateMachine{
    val SPIIDLE = new State with EntryPoint
    val SPIGoHIGH = new State
    val SPIGoLOW = new State
    val SPIJUDGE = new State
    val SPIINIT = new State

//    val time_out = Reg(UInt())

    SPIIDLE
      .onEntry{spiGo := False
        memReadEnable := True
        currentSectionCnt := 0
        currentRampCnt := 0}
      .whenIsActive{
        watch_dog_reg := B"0000"
        currentValue := rampInitValue
        when(key_go.rise()){
          currentCnt := spiClkCnt
          goto(SPIGoHIGH)
        }
      }
      .onExit(memReadEnable := False)
    SPIGoHIGH
      .whenIsActive{
        watch_dog_reg := B"0001"
        when(currentSectionCnt>=totalSectionCnt) {
          goto(SPIINIT)
          spiGo := True
        }.elsewhen((spiClkCnt.value === (currentCnt + 2))|((spiClkCnt.value===(currentCnt+2-(rampCycle * 1000 / 20 - 1)))&((currentCnt+2)>(rampCycle * 1000 / 20 - 1)))){ //这里会有问题 currentCnt + 2有可能spiClkCnt.value永远无法大于(排除，并没事)
          spiGo := True & (currentRampValue =/= 0)
          goto(SPIGoLOW)
        }
      }
//      .onExit(watch_dog_reg := B"0000")
    SPIGoLOW
      .whenIsActive{
        watch_dog_reg := B"0010"
        when((spiClkCnt.value === (currentCnt + 2 + batchCycle))|((spiClkCnt.value===(currentCnt+2+batchCycle-(rampCycle * 1000 / 20 - 1)))&((currentCnt+2+batchCycle)>(rampCycle * 1000 / 20 - 1)))){  //这里会有问题
          spiGo.clear()
          goto(SPIJUDGE)
        }
      }
//      .onExit(watch_dog_reg := B"0000")
    SPIJUDGE
      .onEntry{
        currentRampCnt := currentRampCnt + 1
        memReadEnable := True
        when(currentRampCnt >= currentRampTime){
          currentSectionCnt := currentSectionCnt + 1
        }
      }
      .whenIsActive{
        watch_dog_reg := B"0100"
        when((spiClkCnt.value === (currentCnt + 2 + batchCycle + 8))|((spiClkCnt.value===(currentCnt+2+batchCycle+8-(rampCycle * 1000 / 20 - 1)))&((currentCnt+2+batchCycle+8)>(rampCycle * 1000 / 20 - 1)))){
          currentValue := currentRampValue + currentValue
          goto(SPIGoHIGH)
        }
      }
      .onExit{
        memReadEnable := False
//        watch_dog_reg := B"0000"
      }
    SPIINIT
      .onEntry(currentValue := rampInitValue)
      .whenIsActive{
        watch_dog_reg := B"1000"
        when((spiClkCnt.value === (currentCnt + 2 + batchCycle))|((spiClkCnt.value===(currentCnt+2+batchCycle-(rampCycle * 1000 / 20 - 1)))&((currentCnt+2+batchCycle)>(rampCycle * 1000 / 20 - 1)))){
          spiGo.clear()
          goto(SPIIDLE)
        }
      }
//      .onExit(watch_dog_reg:=B"0000")
  }

  val key_go = RegInit(False) //按键整形结果
  val cntRst_go = RegInit(False)
  if (niCtrl){
    key_go := io.key
  }else {
    val key_deb = new KeyDebounce(50, 20, 2) // 到时如果安装在板卡控制时，则需要把debounce time弄小或者取消
    key_deb.key_in := io.key
    key_deb.key_out <> key_go // 2个周期的trig
  }

  val spiMemAdr = Counter(0 to (section - 1))  // spi读取的内存地址
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
  evalSpiCtrl.io.go <> spiGo
  evalSpiCtrl.io.data := dataHead.asBits ## currentValue(31 downto 12)
  evalSpiCtrl.io.sclk <> eval_sclk
  evalSpiCtrl.io.mosi <> eval_mosi

  io.ss := (eval_ss & !initialMask) | (init_ss & initialMask)
  io.sclk := (eval_sclk & !initialMask) | (init_sclk & initialMask)
  io.mosi := (eval_mosi & !initialMask) | (init_mosi & initialMask)
}


object EvalRampSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.doSim(new EvalRamp(niCtrl = false)){dut=>
      dut.clockDomain.forkStimulus(20)
      dut.io.key #= false
      dut.io.rxd #= true
      sleep(200000)
      def senUart(data:Int=0): Unit ={
        var tmpData = data
        dut.io.rxd #= true
        sleep(8680)
        dut.io.rxd #= false
        sleep(8680)
        for (i <- 0 to 7){
          if (tmpData % 2==1){
            tmpData = (tmpData-1) / 2
            dut.io.rxd #= true
            sleep(8680)
          }else{
            tmpData = tmpData / 2
            dut.io.rxd #= false
            sleep(8680)
          }
        } // 循环触发
        dut.io.rxd #= true
        sleep(8680)
      }
      def sendCommand(str:String) : Unit={
        for(i <- str){
          senUart(i.toByte)
        }}
      sendCommand("@INI012345")
      sendCommand("@VAL054321")
      sendCommand("@TIM100F00")
      sendCommand("@TIM000F00")
      sendCommand("@SEC000004")
//      sendCommand("#set203956")
      def key_trig : Unit={
        dut.io.key #= true
        sleep(50)
        dut.io.key #= false
        sleep(21000)
//        sleep(40000000)
      }
      key_trig
      sleep(40000000)
//      key_trig
//      key_trig
    }
  }
}

