/* Binary Search Implementation: https://www.geeksforgeeks.org/binary-search/ */

import scala.reflect.ClassTag // for type erasure

// Type parameterized ArrayWithSearch class with upper bound on AnyVal and Implict Ordering using Context Bounds - Implicit class

implicit class ArrayWithSearch[T <: AnyVal : Ordering](val sortedValues:Array[T]) {

    def binarySearch(searchElement: T):Option[Int] = {
        
        if(sortedValues.isEmpty) return None

        val total = sortedValues.size
        var first = 0
        var last = total-1

        while(first <= last) {
            val middle = first + (last - first)/2
            if(sortedValues(middle) == searchElement) return Some(middle)
            else if(implicitly[Ordering[T]].gt(sortedValues(middle), searchElement)) last = middle - 1
            else first = middle + 1
        }

        return None
    }
}

// Companion object for SortedArray class with Implict Ordering and ClassTag

object ArrayWithSearch {
    def apply[T <: AnyVal : ClassTag : Ordering](values:T*) = { 
        new ArrayWithSearch[T](values.toArray)
    }
}