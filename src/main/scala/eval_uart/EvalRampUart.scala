package eval_uart

import spinal.core._
import spinal.lib._
import spinal.lib.fsm.{EntryPoint, State, StateDelay, StateMachine}

class EvalRampUart(commandSize : Int=13, logo:Char='@', rampCycle:Int=1000) extends Component{
  /*
  commandSize暂时定为13不要动
   */
  val io = new Bundle{
    val rxd = in Bool()
    val rampInitValue = out UInt(32 bits)
//    val ramValueMem = Mem(UInt(32 bits), wordCount = section)
  }

  def string2Int(str:String) : Int = {
    var result = 0
    for (i <- 3 downto 0){
      result = result + str(i) * Math.pow(256, i).toInt
    }
    result
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

  val rxGrocery = Vec(Reg(Bits(8 bits)) init(0), commandSize)
  val rxStoreValid = Bool()
  val uartCtrl = new EvalUartCtrl(dataWidthMax = 8, baudRate = 115200)
  uartCtrl.io.uart.rxd <> io.rxd
  when(uartCtrl.io.read.valid & rxStoreValid){
    for (i <- 0 to commandSize - 2){
      rxGrocery(i+1) := rxGrocery(i)
    }
    rxGrocery(0) := uartCtrl.io.read.payload
  }

  val section = 16
  val rampInitValue = Reg(UInt(32 bits)) init(U"32'h80000000")
  //  val rampInitValue = Reg(UInt(20 bits)) init(0x80000)
  val rampValueMem = Mem(UInt(32 bits), wordCount = section) init(Vec(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)) //ramp 大小
  //  val ramTimeMem = Mem(UInt(log2Up(1000000000/rampCycle) bits), wordCount = section) init(Vec(5000, 11000, 15000, 18000, 0, 0,0,0,0,0,0,0,0,0,0,0)) //ramp 时间,需要初始化为最大值
  val rampTimeMem = Mem(UInt(32 bits), wordCount = section) init(Vec(1, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0)) //ramp 时间,需要初始化为最大值
  val totalSectionCnt = Reg(UInt(log2Up(16) bits)) init(4)  // 总共的时间计数，单位是10 us

  val currentValue = Reg(UInt(32 bits)) init(rampInitValue)  // 当前的spi值

  val memReadEnable = RegInit(False)

  val currentCnt = Reg(UInt(log2Up(50*rampCycle) bits)) init(0)  //及时专用
  val currentRampCnt = Reg(UInt(32 bits)) init(0)  //进行多少次值改变
  val currentRampTime = Reg(UInt(32 bits)) init(0)
  val currentRampValue = Reg(UInt(32 bits)) init(0)

  val currentSectionCnt = Reg(UInt(4 bits)) init(0)  // 纪录第几段

  currentRampTime := rampTimeMem.readSync(enable = memReadEnable, address = currentSectionCnt)
  currentRampValue := rampValueMem.readSync(enable = memReadEnable, address = currentSectionCnt)

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
    rampValueMem.write(
      address = asciiConvert(rxGrocery(commandSize-5)),
      data = (asciiConvert(rxGrocery(commandSize-6)).asBits ## asciiConvert(rxGrocery(commandSize-7)).asBits ## asciiConvert(rxGrocery(commandSize-8)).asBits ## asciiConvert(rxGrocery(commandSize-9)).asBits ## asciiConvert(rxGrocery(commandSize-10)).asBits ## asciiConvert(rxGrocery(commandSize-11)).asBits ## asciiConvert(rxGrocery(commandSize-12)).asBits ## asciiConvert(rxGrocery(commandSize-13)).asBits).asUInt,
      enable = valWriteEnable
    )
    rampTimeMem.write(
      address = asciiConvert(rxGrocery(commandSize-5)),
      data = (asciiConvert(rxGrocery(commandSize-6)).asBits ## asciiConvert(rxGrocery(commandSize-7)).asBits ## asciiConvert(rxGrocery(commandSize-8)).asBits ## asciiConvert(rxGrocery(commandSize-9)).asBits ## asciiConvert(rxGrocery(commandSize-10)).asBits ## asciiConvert(rxGrocery(commandSize-11)).asBits ## asciiConvert(rxGrocery(commandSize-12)).asBits ## asciiConvert(rxGrocery(commandSize-13)).asBits).asUInt,
      enable = timWriteEnable
    )
  }
}


object EvalRampUartVerilog {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = Verilog,
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
                                                    clockEdge = RISING,
                                                    resetActiveLevel = LOW),
                                                  targetDirectory="verilog")
    .generate(new EvalRampUart())
  }
}