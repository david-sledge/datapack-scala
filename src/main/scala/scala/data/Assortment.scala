package scala.data

import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.Seq
import Assortment._

final class Assortment[K, V] private[data](
    _insertOrder: List[Element[K, V]]) {

  lazy val insertOrder = _insertOrder.reverse

  lazy val (set, map, seq) =
    ((Set[K](), Map[K, V](), Seq[V]()) /: _insertOrder)((acc, elem) => {
      acc match {
        case (set, map, seq) =>
          elem match {
            case Entry(key) => (set + key, map, seq)
            case Mapping(key, value) =>
              (set, map + (key -> value), seq)
            case Item(value) => (set, map, seq :+ value)
          }
      }
    })

  def this() = {
    this(List[Element[K, V]]())
  }

  def closeZipper[T](left: List[T], right: List[T]): List[T] =
    left match {
      case elem :: tail => closeZipper(tail, elem :: right)
      case Nil => right
    }

  def findAndReplace(
      left: List[Element[K, V]],
      right: List[Element[K, V]],
      entryF: (K, List[Element[K, V]],
          List[Element[K, V]]) =>
            Option[List[Element[K, V]]],
      mappingF: (K, V, List[Element[K, V]],
          List[Element[K, V]]) =>
            Option[List[Element[K, V]]])
  : Option[List[Element[K, V]]] = {
    right match {
      case Nil => None
      case elem :: tail => elem match {
        case Entry(key_) => entryF(key_, left, tail) match {
          case None => findAndReplace(elem :: left, tail, entryF, mappingF)
          case ol @ Some(_) => ol
        }
        case Mapping(key_, value_) =>
          mappingF(key_, value_, left, tail) match {
            case None => findAndReplace(elem :: left, tail, entryF, mappingF)
            case ol @ Some(_) => ol
          }
        case Item(_) =>
          findAndReplace(elem :: left, tail, entryF, mappingF)
      }
    }
  }

  def +(key: K) = {

    val entryF: (K, List[Element[K, V]],
          List[Element[K, V]]) =>
            Option[List[Element[K, V]]] =
      (key_, left, tail) => if (key == key_) Some(_insertOrder) else None

    val mappingF: (K, V, List[Element[K, V]],
          List[Element[K, V]]) =>
            Option[List[Element[K, V]]] =
      (key_, value_, left, tail) =>
      if (key == key_)
        Some(closeZipper(left, Entry[K, V](key) :: tail))
      else None

    new Assortment(
        findAndReplace(Nil, _insertOrder, entryF, mappingF) match {
      case None => Entry[K, V](key) :: _insertOrder
      case Some(list) => list
    })
  }

  def +(key: K, value: V) = {

    val entryF: (K, List[Element[K, V]],
          List[Element[K, V]]) =>
            Option[List[Element[K, V]]] =
      (key_, left, tail) =>
        if (key == key_)
          Some(closeZipper(
              left, Mapping[K, V](key, value) :: tail))
        else None

    val mappingF: (K, V, List[Element[K, V]],
          List[Element[K, V]]) =>
            Option[List[Element[K, V]]] =
      (key_, value_, left, tail) =>
        if (key == key_)
          if (value_ == value) Some(_insertOrder)
          else Some(closeZipper(
              left, Mapping[K, V](key, value) :: tail))
        else None

    new Assortment(findAndReplace(Nil, _insertOrder, entryF, mappingF)
        match {
          case None => Mapping[K, V](key, value) :: _insertOrder
          case Some(list) => list
        })
  }

  def :+(value: V) = new Assortment(Item[K, V](value) :: _insertOrder)

  lazy val string = s"($set, $map, $seq)"

  override def toString = string

  override def equals(o: Any) = {
    if (o.isInstanceOf[Assortment[_, _]])
    {
      val mc = o.asInstanceOf[Assortment[_, _]]

      set == mc.set && map == mc.map && seq == mc.seq
    }
    else false
  }

  lazy val hashcode = set.hashCode ^ map.hashCode ^ seq.hashCode

  override def hashCode = hashcode

  def containsKey(key: K) = map.contains(key) || set.contains(key)

  def containsValue(value: V) =
    map.values.exists(_ == value) || seq.contains(value)
}

object Assortment {
  def apply[K, V]() = new Assortment[K, V]

  abstract class Element[K, V]
  case class Entry [K, V](key: K) extends Element[K, V]
  case class Mapping [K, V](key: K, value: V) extends Element[K, V]
  case class Item [K, V](value: V) extends Element[K, V]

}
