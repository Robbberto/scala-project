import scala.io.Source

object HelloWorld {

  def userAvgRat(user: Int, valuesAsList: List[(Int, Int, Int)]): Double = {
/*
Function that computes the average rating for a user. In case the user did not rate any movie, it returns 0
*/

    val userData = valuesAsList.filter(x=>x._1 == user)
    if (userData.size == 0){
      return 0.0
    } else {
      val avgRatingUser = userData.map(_._3).sum/userData.size.toDouble
      return avgRatingUser
    }
  }

  def itemAvgRat(item: Int, valuesAsList: List[(Int, Int, Int)]): Double = {
/*
Function that computes the average rating of a movie. In case the movie has no reviews, it returns 0
*/
    val itemData = valuesAsList.filter(x=>x._2 == item)
    if (itemData.size == 0){
      return 0.0
    } else {
      val avgRatingItem = itemData.map(_._3).sum/itemData.size.toDouble
      return avgRatingItem
    }
   }
  

  def scale(x: Double, avgRat: Double): Double = {
/*
Function "scale" as described in the project description
*/
    if (x>avgRat){
      5 - avgRat
    } else if (x<avgRat) {
      avgRat - 1
    } else {
      1
    }
  }


  def predict(avgRat: Double, avgDev: Double, globalAvg:Double): Double = {
/*
Prediction function as in formula 5 in the project description
*/

	if (avgRat == 0.0){
	  return globalAvg
	} else {
	  val prediction = avgRat + avgDev * scale((avgRat + avgDev), avgRat)
	  return prediction
	}
  }



  def globalAvgDev(item: Int, valuesAsList: List[(Int, Int, Int)]): Double = { 
/*
Function that computes the global average deviation for a given item
*/

	// filtri tutti quelli di item 1
	// li scorri, incontri un user, calcoli il suo average rating (usando il df completo)
	// calcoli la sua deviation per item 1
	// calcoli global deviation
	//STO ASSUMENDO CHE UN UTENTE NON PUO FARE 2 REVIEW DELLO STESSO OGGETTO, confirmed by forum

	val itemData = valuesAsList.filter(x=>x._2 == item)
	if (itemData.size == 0){
	  return 0.0
	} else {
	  val deviations = for (review <- itemData;
				avgRat = userAvgRat(review._1, valuesAsList);
				deviation = (review._3 - avgRat) / scale(review._3, avgRat)
			     ) yield (review._1, deviation)

	  val avgGlobalDev = deviations.map(_._2).sum/deviations.size.toDouble
	  return avgGlobalDev
	}
  }


  def main(args: Array[String]) {
/*
Main function that handles everything
*/

    var filename = "/Users/rob/Documents/EPFL/systems/data/ml-100k/u2.base"
    val parsed =
            for (line<-Source.fromFile(filename).getLines;
                 Array(user, movie, rating, timestamp) = line.split("\t")
            ) yield (user.toInt, movie.toInt, rating.toInt)// rimuovo il timestamp
    val valuesAsList = parsed.toList



/* Versione vecchia, senza usare la funzione apposita (può essere eliminata, mantengo temporaneamente per reference

    val user1Data = valuesAsList.filter(x=>x._1 == 1)
    val avgRatingUser1 = user1Data.map(_._3).sum/user1Data.size.toDouble
    
    val item1Data = valuesAsList.filter(x=>x._2 == 1)
    val avgRatingItem1 = item1Data.map(_._3).sum/item1Data.size.toDouble

    println("Average rating user 1: " + avgRatingUser1)
    println("Average rating item 1: " + avgRatingItem1)

*/

    // QUESTION B1
    val globalAvg = valuesAsList.map(_._3).sum/valuesAsList.size.toDouble
    println("Global average: " + globalAvg)

    val avgRatUser1 = userAvgRat(1, valuesAsList)
    val avgRatItem1 = itemAvgRat(1, valuesAsList)
    val avgDevItem1 = globalAvgDev(1, valuesAsList)

    println("Average rating user 1 with function: " + avgRatUser1)
    println("Average rating item 1 with function: " + avgRatItem1)
    println("Average deviation item 1: " + avgDevItem1)


    println("Prediction user 1, item 1: " + predict(avgRatUser1, avgDevItem1, globalAvg))

    // QUESTION B2


  }
}