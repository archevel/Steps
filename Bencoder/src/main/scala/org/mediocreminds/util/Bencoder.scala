package org.mediocreminds.util

/**
 * Created by IntelliJ IDEA.
 * User: archevel
 * Date: 2009-nov-10
 * Time: 14:36:06
 * To change this template use File | Settings | File Templates.
 */

import scala.collection.immutable.SortedMap
import scala.collection.immutable.TreeMap


abstract class Benable(val x: Any) {
  override def equals(that: Any) = that match {
    case other: Benable => other.x == x
    case _ => false
  }
}
class BenInt(override val x: Int) extends Benable

object BenInt extends Benable {
  def apply(int: Int) = new BenInt(int)

  def unapply(benInt: BenInt) = Some(benInt.x)
}

class BenString(override val x: String, val array: Array[Byte]) extends Benable with Ordered[BenString] {
  override def compare(that: BenString): Int = {
    x.asInstanceOf[String] compare that.x.asInstanceOf[String]
  }
}

object BenString {
  def apply(string: String) = new BenString(string, string.getBytes("UTF-8"))

  def apply(string: String, array: Array[Byte]) = new BenString(string, array)

  def unapply(benString: BenString) = Some(benString.x, benString.array)
}

class BenList(override val x: List[Benable]) extends Benable
object BenList extends Benable {
  def apply(list: List[Benable]) = new BenList(list)

  def unapply(benList: BenList) = Some(benList.x)
}
class BenMap(override val x: SortedMap[BenString, Benable]) extends Benable
object BenMap extends Benable {
  def apply(map: SortedMap[BenString, Benable]) = new BenMap(map)

  def unapply(benMap: BenMap) = Some(benMap.x)
}

object BenConversions {
  // Implicit definitions converting string, int, list and map to benables and
  // Tuples: (String, String),(String, int),(String, List[Benable]),
  // (String, SortedMap[BenString, Benable]) to
  // (BenString, BenString), (BenString, BenInt), (BenString, BenList) and
  // (BenString, BenMap)
  implicit def int2BenInt(int: Int) = BenInt(int)

  implicit def string2BenString(string: String) = BenString(string)

  implicit def list2BenList(list: List[Benable]) = BenList(list)

  implicit def sortedMap2BenMap(map: SortedMap[BenString, Benable]) = BenMap(map)

  implicit def stringStringTuple2BenStringBenStringTuple(t: (String, String)) = (BenString(t._1), BenString(t._2))

  implicit def stringIntTuple2BenStringBenIntTuple(t: (String, Int)) = (BenString(t._1), BenInt(t._2))

  implicit def stringListTuple2BenStringBenListTuple(t: (String, List[Benable])) = (BenString(t._1), BenList(t._2))

  implicit def stringMapTuple2BenStringBenMapTuple(t: (String, SortedMap[BenString, Benable])) = (BenString(t._1), BenMap(t._2))
}

object Bencoder {
  import BenConversions._

  def encode(benable: Benable): Array[Byte] = {
    benable match {
      case BenString(_, array) => {
        val encString = array
        val encLength = array.length.toString.getBytes("US-ASCII")
        val encSeparator = ":".getBytes("US-ASCII")
        encLength ++ encSeparator ++ encString
      }
      case BenInt(int) => ("i" + int + "e") getBytes ("US-ASCII")
      case BenList(list) => {
        list.foldLeft(Array('l'.asInstanceOf[Byte]))((ar, benable) => {
          ar ++ encode(benable)
        }) ++ Array('e'.asInstanceOf[Byte])
      }
      case BenMap(map) => {
        (map.foldLeft(Array('d'.asInstanceOf[Byte]))((ar, tuple) => {
          ar ++ encode(tuple._1) ++ encode(tuple._2)
        })) ++ Array('e'.asInstanceOf[Byte])
      }
    }
  }

  def decode(array: Array[Byte]): Option[Benable] = {
    def decodeBenString(list: List[Byte]): (BenString, List[Byte]) = {
      val (strLength, strAndTail) = list splitAt (list findIndexOf (x => x == ':'))
      val toRead = (new String(strLength.toArray, "US-ASCII")).toInt
      val (str, tail) = (strAndTail tail) splitAt (toRead)
      if (str.length != toRead) {
        throw new IllegalArgumentException("LIst does not contain bencoded data")
      }
      (BenString(new String(str.toArray, "UTF-8"), str.toArray), tail) // UTF-8
    }

    def decodeBenInt(list: List[Byte]): (BenInt, List[Byte]) = {
      val (integer, rest) = list splitAt (list findIndexOf (x => x == 'e'))
      integer match {
        case '-' :: '0' :: tail => throw new IllegalArgumentException("List does not contain bencoded data")
        case '0' :: x :: tail if x >= '0' && x <= '9' => throw new IllegalArgumentException("List does not contain bencoded data")
        case Nil => throw new IllegalArgumentException("List does not contain bencoded data")
        case _ => {
          val i = (new String(integer.toArray, "US-ASCII")).toInt
          (i, rest tail) // tail removes the 'e'
        }
      }
    }

    def decodeBenList(list: List[Byte], benList: List[Benable]): (BenList, List[Byte]) = {
      list match {
        case 'd' :: tail => { // innerDecode map
          val (benable, rest) = decodeBenMap(tail, TreeMap[BenString, Benable]())
          decodeBenList(rest, benable :: benList)
        }
        case 'l' :: tail => { // innerDecode list
          val (benable, rest) = decodeBenList(tail, List[Benable]())
          decodeBenList(rest, benable :: benList)
        }
        case 'i' :: tail => { // innerDecode int
          val (benable, rest) = decodeBenInt(tail)
          decodeBenList(rest, benable :: benList)
        }
        case x :: tail if x <= '9' && x >= '0' => { // innerDecode string
          val (benable, rest) = decodeBenString(x :: tail)
          decodeBenList(rest, benable :: benList)
        }
        case 'e' :: tail => // end of list
          (BenList(benList.reverse), tail) // reversing to preserve order...
        case _ => throw new IllegalArgumentException("List does not contain bencoded data")
      }
    }

    def decodeBenMap(list: List[Byte], benMap: SortedMap[BenString, Benable]): (BenMap, List[Byte]) = {
      list match {
        case 'e' :: tail => // end of map
          (BenMap(benMap), tail)
        case x :: tail if x <= '9' && x >= '0' => { // innerDecode string
          val (benString, valueAndRest) = decodeBenString(x :: tail)
          valueAndRest match {
            case 'd' :: tail => { // innerDecode map
              val (benable, rest) = decodeBenMap(tail, TreeMap[BenString, Benable]())
              decodeBenMap(rest, benMap + ((benString, benable)))
            }
            case 'l' :: tail => { // innerDecode list
              val (benable, rest) = decodeBenList(tail, List[Benable]())
              decodeBenMap(rest, benMap + ((benString, benable)))
            }
            case 'i' :: tail => { // innerDecode int
              val (benable, rest) = decodeBenInt(tail)
              decodeBenMap(rest, benMap + ((benString, benable)))
            }
            case x :: tail if x <= '9' && x >= '0' => { // innerDecode string
              val (benable, rest) = decodeBenString(x :: tail)
              decodeBenMap(rest, benMap + ((benString, benable)))
            }
            case _ => throw new IllegalArgumentException("List does not contain bencoded data")
          }
        }
        case _ => throw new IllegalArgumentException("List does not contain bencoded data")
      }
    }

    def innerDecode(list: List[Byte]): Benable = {
      (list match {
        case 'd' :: tail => // innerDecode map
          decodeBenMap(tail, TreeMap[BenString, Benable]())
        case 'l' :: tail =>
          decodeBenList(tail, List[Benable]()) // innerDecode list
        case 'i' :: tail =>
          decodeBenInt(tail) // innerDecode int
        case x :: tail if x <= '9' && x >= '0' => // innerDecode string
          decodeBenString(x :: tail)
        case _ => None
      }) match {
      // the whole list has been decoded
        case (x: Benable, Nil) => x
        // if it's not an empty list we haven't been able to innerDecode the
        // list properly... there is still data left (or we have returned somethin strange.
        case _ => throw new IllegalArgumentException("List does not contain bencoded data")
      }
    }

    try {
      Some(innerDecode(array toList))
    } catch {
      case _ => None // Whenever failure to decode occurs
    }
  }
}