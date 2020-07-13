package greedy

case class Activity(start: Int, end: Int)

object ActivitySelection {

    def getMaxActivities(activities: Array[Activity]): Int = {

        if(activities.isEmpty) return 0

        val totalActivities = activities.size
        val activitiesSorted = activities.sortBy(_.end)

        var totalSelectedActivities = 1
        var currentActivity = activities.head
        var activityIndex = 1
        
        while(activityIndex < totalActivities) {
            val potentialActivity = activities(activityIndex)
            if(potentialActivity.start >= currentActivity.end) {
                totalSelectedActivities += 1
                currentActivity = potentialActivity
            }
            activityIndex += 1
        }

        return totalSelectedActivities
    }

}