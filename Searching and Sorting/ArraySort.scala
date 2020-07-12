/* Sorting functions implementation */

implicit class ArrayWithSort[T <: AnyVal: Ordering: scala.reflect.ClassTag](values: Array[T]) {

    private def max(a: T, b: T):Boolean = implicitly[Ordering[T]].gt(a, b)
    private def min(a: T, b: T):Boolean = implicitly[Ordering[T]].lt(a, b)

    def mergeSort(): Array[T] = {

        val tempCopy = values.clone // array cloned for every call

        def merge(leftIndex: Int, middleIndex: Int, rightIndex: Int): Unit = {

            // println("merge", leftIndex, middleIndex, rightIndex)

            val left = tempCopy.slice(leftIndex, middleIndex+1)
            val right = tempCopy.slice(middleIndex+1, rightIndex+1)

            // println(left.mkString("|"), right.mkString("|"))
        
            if(!left.isEmpty && !right.isEmpty) {

                var (currentLeft, currentRight) = (0, 0)
                val (leftMax, rightMax) = (left.size, right.size)
                var current = leftIndex
            
                while(currentLeft < leftMax && currentRight < rightMax) {
                    if(min(left(currentLeft), right(currentRight))) { tempCopy(current) = left(currentLeft); currentLeft += 1; current += 1 }
                    else { tempCopy(current) = right(currentRight); currentRight += 1; current += 1 }
                }

                if(currentLeft < leftMax) left.slice(currentLeft, leftMax).copyToArray(tempCopy, current)
                else if(currentRight < rightMax) right.slice(currentRight, rightMax).copyToArray(tempCopy, current)
            }
        }

        def mergeSort(start: Int, end: Int): Unit = {
            if(start < end) { // so that atleast 2 elements needed for split!
                val middle = (start + end)/2
                mergeSort(start, middle)
                mergeSort(middle+1, end)
                merge(start, middle, end)
            }
        }

        mergeSort(0, values.size - 1)
        return tempCopy
    }

    def quickSort(): Array[T] = { // speed efficiency as fast as sorted implementation in scala Array!
        
        val tempCopy = values.clone // array cloned for every call - for immutablity

        def quickSortRecurse(start: Int, end: Int): Unit = {

            if(end - start == 1) { // base case - 2 elements
                if(min(tempCopy(end), tempCopy(start))) { 
                    val temp = tempCopy(start)
                    tempCopy(start) = tempCopy(end)
                    tempCopy(end) = temp
                }
            }

            else if(start < end) {

                val pivot = tempCopy(start) // pivot strategy - first element - not randomized!!
                
                var leftPointer = start + 1
                var rightPointer = end

                while(leftPointer < rightPointer) {

                    val leftPointerLtPivot = min(tempCopy(leftPointer), pivot)
                    val rightPointerGtPivot = min(pivot, tempCopy(rightPointer))

                    // interchange
                    if(!leftPointerLtPivot && !rightPointerGtPivot) {
                        val temp = tempCopy(leftPointer)
                        tempCopy(leftPointer) = tempCopy(rightPointer)
                        tempCopy(rightPointer) = temp
                        rightPointer -= 1
                        leftPointer += 1
                    } 

                    else {
                        if(rightPointerGtPivot) rightPointer -= 1
                        if(leftPointerLtPivot) leftPointer += 1 
                    }
                }

                val pivotIndex = rightPointer

                tempCopy(start) = tempCopy(pivotIndex)
                tempCopy(pivotIndex) = pivot

                quickSortRecurse(start, pivotIndex - 1)
                quickSortRecurse(pivotIndex + 1, end)
            }
        }

        quickSortRecurse(0, values.size - 1)
        return tempCopy
    }
}