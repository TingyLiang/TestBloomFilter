package priv.robin

import scala.collection.mutable
import scala.util.Random

object testBloomFilter {

  val size = Int.MaxValue
  val filter = new mutable.BitSet(size)
  val funSize = 5

  val funcs = {
    val fs = new Array[HashFun](funSize)
    val random = new Random()
    for(i <- 0.until(funSize)){
      fs(i) = new HashFun(size,random.nextInt() )
    }
    fs
  }


  def contains(string: String) :Boolean = {
    funcs.foreach(f =>{
      if (!filter(f.hash(string)))
        return false
    })
    true
  }

  def add(value: String) = {
    funcs.foreach(f =>{
      filter(f.hash(value)) = true
    })
  }

  def main(args: Array[String]): Unit = {

    val v1 = "a"
    val v2 = "b"
    testBloomFilter.add(v1)
    println(s"value a exists: ${testBloomFilter.contains(v1)}")
    println(s"value b exists: ${testBloomFilter.contains(v2)}")


  }
}

class HashFun(bound: Int, seed: Int) {
  def hash(value: String): Int = {
    var pos = 0
    val len = value.length
    value.foreach(c =>{
      pos = seed * pos + c
    })

    pos & (bound -1)
  }
}