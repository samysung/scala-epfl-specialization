package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean =  {

    def iterateChars(balance:Tuple2[Int,Int],chars:Array[Char]): Boolean= {
      if(chars.isEmpty) {
        if(balance._1==balance._2) true
        else false
      }
      else {
        chars.head match {
          case '(' => iterateChars((balance._1+1,balance._2), chars.tail)
          case ')' => if(balance._2+1>balance._1) false else{
            iterateChars((balance._1,balance._2+1), chars.tail)
          }
          case _ => iterateChars(balance, chars.tail)
        }
      }
    }
    if(chars.isEmpty) true
    else iterateChars((0,0),chars)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) :Tuple2[Int,Int]= {
      if(chars.isEmpty)  (arg1,arg2)
      else {
        chars.head match {
          case '(' => traverse(idx+1,until,arg1+1,arg2)
          case ')' =>  if(arg1-1<0)traverse(idx+1,until,arg1-1,arg2-1) else traverse(idx+1,until,arg1-1,arg2)
          case _ => traverse(idx+1,until,arg1,arg2)
        }
      }

    }

    def reduce(from: Int, until: Int):Tuple2[Int,Int] = {
     if(until-from<0){val v=traverse(from,until,0,0);(v._1,v._2)}
      else{val split = from + (until-from)/2;
       val (l,r)=parallel(reduce(from,split),reduce(split,until));
       (l._1+r._1,l._2+r._2)}
    }

    reduce(0, chars.length) == (true,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
