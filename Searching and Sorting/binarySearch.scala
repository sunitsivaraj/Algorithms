/* binary search */

def binarySearch(values:Array[Int], item:Int):Int = {
    val total_values = values.size
    def binarySearchRecurse(start_index:Int, end_index:Int):Int = {
        
        if(end_index < start_index) return -1

        val middle_index:Int = (start_index + end_index)/2
        
        val start = values(start_index)
        val end = values(end_index)
        val middle = values(middle_index)

        if(middle == item) return middle_index
        else if(middle > item) return binarySearchRecurse(start_index, middle_index-1)
        else binarySearchRecurse(middle_index+1, end_index)
    }

    binarySearchRecurse(0, total_values-1)
}