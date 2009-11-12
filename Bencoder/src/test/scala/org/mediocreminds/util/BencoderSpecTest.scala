package org.mediocreminds.util

//import org.specs._
// import org.specs.runner.{ConsoleRunner, JUnit4}

import org.specs._
import matcher.Matcher
import runner.{JUnit4}
import BenConversions._
import scala.collection.immutable.TreeMap


/**
 * Created by IntelliJ IDEA.
 * User: archevel
 * Date: 2009-nov-10
 * Time: 15:24:47
 * To change this template use File | Settings | File Templates.
 */

class BencoderSpecTest extends JUnit4(BencoderSpec)

object BencoderSpec extends Specification {
  def beSomeBenable(benable: Benable) = new Matcher[Option[Benable]] {
    def apply(someBenable: => Option[Benable]) = {
      ((someBenable match {
        case Some(b) => b.x == benable.x
        case None => false
      }), "Benable " + benable + " is equal to Benable in " + someBenable, "Benable " + benable + " is not equal to Benable in " + someBenable)
    }
  }
  "A Bencoder " should {
    "encode a BenString to a bencoded string" in {
      val encoding = List[Byte]('4', ':', 'e', 'm', 'i', 'l')
      Bencoder.encode("emil") must containInOrder(encoding)
    }
    "encode an to a bencoded integer" in {
      val encoding = List[Byte]('i', '1', 'e')
      Bencoder.encode(1) must containInOrder(encoding)
    }
    "encode an empty List[Benable] to a bencoded list" in {
      val encoding = List[Byte]('l', 'e')
      Bencoder.encode(List[Benable]()) must containInOrder(encoding)
    }
    "encode a List[Benable] to a bencoded list" in {
      val encoding = List[Byte]('l', 'd', 'e', 'i', '1', 'e', '4', ':', 'e', 'm', 'i', 'l', 'e')
      Bencoder.encode(List[Benable](TreeMap[BenString, Benable](), 1, "emil")) must containInOrder(encoding)
    }
    "encode an empty SortedMap[BenString, Benable] to a bencoded dictionary" in {
      val encoding = List[Byte]('d', 'e')
      Bencoder.encode(TreeMap[BenString, Benable]()) must containInOrder(encoding)
    }
    "encode an SortedMap[BenString, Benable] to a bencoded dictionary" in {
      val encoding = List[Byte]('d', '4', ':', 'e', 'm', 'i', 'l', 'i', '1', 'e', 'e')
      Bencoder.encode(TreeMap[BenString, Benable]() + (("emil", 1))) must containInOrder(encoding)
    }
    "decode an Array contianing a bencoded integer" in {
      val encoding1 = List[Byte]('i', '1', 'e').toArray
      (Bencoder.decode(encoding1)) must beSomeBenable(BenInt(1))

      val encoding2 = List[Byte]('i', '0', 'e').toArray
      Bencoder.decode(encoding2) must beSomeBenable(BenInt(0))
      val encoding3 = List[Byte]('i', '-', '1', 'e').toArray
      (Bencoder.decode(encoding3)) must beSomeBenable(BenInt(-1))
    }
    "not decode an Array containing a incorrectly bencoded integer" in {
      val encoding1 = List[Byte]('i', '0', '1', 'e').toArray
      Bencoder.decode(encoding1) must beNone
      val encoding2 = List[Byte]('i', '-', '0', 'e').toArray
      Bencoder.decode(encoding2) must beNone
      val encoding3 = List[Byte]('i', '0', '1', 'e').toArray
      Bencoder.decode(encoding3) must beNone
    }
    "decode an Array contianing a bencoded string" in {
      val encoding1 = List[Byte]('4', ':', 'e', 'm', 'i', 'l').toArray
      Bencoder.decode(encoding1) must beSomeBenable(BenString("emil"))
      val encoding2 = List[Byte]('0', ':').toArray
      Bencoder.decode(encoding2) must beSomeBenable(BenString(""))
    }
    "not decode an Array contianing a incorrectly bencoded string" in {
      val encoding1 = List[Byte]('4', ':', 'e', 'm', 'l').toArray
      Bencoder.decode(encoding1) must beNone
      val encoding2 = List[Byte]('4', ':', 'e', 'm', 'i', 'i', 'l').toArray
      Bencoder.decode(encoding2) must beNone
    }
    "decode an Array contianing a bencoded list" in {
      val encoding1 = List[Byte]('l', 'e').toArray
      Bencoder.decode(encoding1) must beSomeBenable(BenList(List[Benable]()))
      val encoding2 = List[Byte]('l', 'd', 'e', 'i', '1', 'e', '4', ':', 'e', 'm', 'i', 'l', 'e').toArray
      Bencoder.decode(encoding2) must beSomeBenable(BenList(List[Benable](TreeMap[BenString, Benable](), 1, "emil")))
    }
    "not decode an Array contianing a incorrectly bencoded list" in {
      val encoding1 = List[Byte]('l').toArray
      Bencoder.decode(encoding1) must beNone
      val encoding2 = List[Byte]('d', 'e', 'i', '1', 'e', '4', ':', 'e', 'm', 'i', 'l', 'e').toArray
      Bencoder.decode(encoding2) must beNone
    }
    "decode an Array contianing a bencoded dictionary" in {
      val encoding1 = List[Byte]('d', '4', ':', 'e', 'm', 'i', 'l', 'i', '1', 'e', 'e').toArray
      Bencoder.decode(encoding1) must beSomeBenable(BenMap(TreeMap[BenString, Benable]() + (("emil", 1))))
      val encoding2 = List[Byte]('d', 'e').toArray
      Bencoder.decode(encoding2) must beSomeBenable(BenMap(TreeMap[BenString, Benable]()))
    }
    "not decode an Array contianing a incorrectly bencoded dictionary" in {
      val encoding1 = List[Byte]('d').toArray
      Bencoder.decode(encoding1) must beNone
      val encoding2 = List[Byte]('d', '4', ':', 'e', 'm', 'i', 'l', 'i', '1', 'e', 'e', 'e').toArray
      Bencoder.decode(encoding2) must beNone
    }
  }
}