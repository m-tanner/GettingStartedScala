object Robber extends App {
  println(rob(nums = Array(1, 2, 3, 5)))

  def rob(nums: Array[Int]): Int = {
    val (_, toReturnValue) = nums.foldLeft(0, 0) {
      case ((previousMax, currentMax), num) => (currentMax, math.max(currentMax, previousMax + num))
      case _ => throw new RuntimeException
    }
    toReturnValue
  }
}
