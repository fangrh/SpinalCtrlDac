package convert

object binaryToDec {
  // Convert binary string to int.将二进制的字符串转换为整数
  def convert(src:String) : Int = {
    if(!src.matches("[0|1]*")){
      println("invalid input")
      return 0
    }

    src.length match{
      case 0 => 0
      case 1 => src.toInt * Math.pow(2,0).toInt
      case _ => convert(src.substring(1, src.length)) + src.substring(0, 1).toInt * (1 << src.length - 1)
    }
  }
}

object test{
  def main(args: Array[String]): Unit = {
    println(binaryToDec.convert("111"))
  }
}