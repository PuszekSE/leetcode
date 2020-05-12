object SingleSingle {

  def singleNonDuplicate(nums: Array[Int]): Int = {
    def findSingle(start: Int, stop: Int): Int = {
      val mid = (stop + start) / 2
      if (start == stop) {
        nums(start)
      } else {
        if (nums(mid) == nums(mid - 1)) {
          if (mid % 2 == 0) {
            findSingle(start, mid - 2)
          } else {
            findSingle(mid + 1, stop)
          }
        } else if (nums(mid) == nums(mid + 1)) {
          if (mid % 2 == 0) {
            findSingle(mid + 2, stop)
          } else {
            findSingle(start, mid - 1)
          }
        } else nums(mid)
      }
    }

    findSingle(0, nums.length - 1)
  }

  def main(args: Array[String]): Unit = {
    val example = Array(1, 1, 2, 3, 3, 4, 4, 8, 8)
    val example2 = Array(3, 3, 7, 7, 10, 11, 11)

    println(singleNonDuplicate(example))
    println(singleNonDuplicate(example2))
  }

}


