/* activity selection problem */

/* select maximum number of activities - passing start and end times */
def get_activities(activities:List[Tuple2[Int, Int]]):MutableList[Tuple2[Int, Int]] = {
    if(activities.size == 0) return new MutableList[Tuple2[Int, Int]]()
    val selected_activities = scala.collection.mutable.MutableList[Tuple2[Int, Int]]()
    val sorted_activities = activities.sortBy(activity => activity._2)
    selected_activities += sorted_activities(0)

    for(index <- 1 until sorted_activities.length) {
        val current_actitivity = sorted_activities(index)
        val previous_selected = selected_activities.last
        if(current_actitivity._1 >= previous_selected._2) selected_activities += current_actitivity
    }

    return selected_activities
}