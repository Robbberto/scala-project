import scala.io.Source

object HelloWorld {

  def computeUserAvgRat(user: Int, valuesAsList: List[(Int, Int, Int)]): Double = {
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

  def computeItemAvgRat(item: Int, valuesAsList: List[(Int, Int, Int)]): Double = {
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



  def computeGlobalAvgDev(item: Int, valuesAsList: List[(Int, Int, Int)]): Double = { 
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
				avgRat = computeUserAvgRat(review._1, valuesAsList);
				deviation = (review._3 - avgRat) / scale(review._3, avgRat)
			     ) yield (review._1, deviation)

	  val avgGlobalDev = deviations.map(_._2).sum/deviations.size.toDouble
	  return avgGlobalDev
	}
  }

    def computeGlobalAvg(valuesAsList: List[(Int, Int, Int)]): Double = {
	return valuesAsList.map(_._3).sum/valuesAsList.size.toDouble
  }

    def computePredictionsGlobalAvg(testValuesAsList: List[(Int, Int, Int)], trainValuesAsList: List[(Int, Int, Int)]): Double = {
/*
Function that does the predictions using Global Average on all the elements of the test set. Training is done with the training set.
*/
	  val globalAvg = computeGlobalAvg(trainValuesAsList)
	// predictions è una lista di tuple (valore vero, valore predetto)
	  println("Number of values to predict: " + testValuesAsList.size)

	  val predictions = for ((review, i) <- testValuesAsList.zipWithIndex;
				//println("Prediction " + i);
				prediction = globalAvg
			     ) yield (review._3, prediction)

	  return predictions

}

    def computePredictionsItemAvg(testValuesAsList: List[(Int, Int, Int)], trainValuesAsList: List[(Int, Int, Int)]): Double = {
/*
Function that does the predictions using Item Averages on all the elements of the test set. Training is done with the training set.
*/
	// predictions è una lista di tuple (valore vero, valore predetto)
	  println("Number of values to predict: " + testValuesAsList.size)

	  val predictions = for ((review, i) <- testValuesAsList.zipWithIndex;
				//println("Prediction " + i);
				prediction = computeItemAvgRat(review._2, trainValuesAsList)
			     ) yield (review._3, prediction)

	  return predictions

}


    def computePredictionsUserAvg(testValuesAsList: List[(Int, Int, Int)], trainValuesAsList: List[(Int, Int, Int)]): Double = {
/*
Function that does the predictions using Item Averages on all the elements of the test set. Training is done with the training set.
*/
	// predictions è una lista di tuple (valore vero, valore predetto)
	  println("Number of values to predict: " + testValuesAsList.size)

	  val predictions = for ((review, i) <- testValuesAsList.zipWithIndex;
				//println("Prediction " + i);
				prediction = computeUserAvgRat(review._1, trainValuesAsList)
			     ) yield (review._3, prediction)

	  return predictions

}




   def computePredictionsEq5(testValuesAsList: List[(Int, Int, Int)], trainValuesAsList: List[(Int, Int, Int)]): Double = {
/*
Function that does the predictions using equation 5 on all the elements of the test set. Training is done with the training set.
*/
	  val globalAvg = computeGlobalAvg(trainValuesAsList)
	// predictions è una lista di tuple (valore vero, valore predetto)
	  println("Number of values to predict: " + testValuesAsList.size)

	  val predictions = for ((review, i) <- testValuesAsList.zipWithIndex;
				//println("Prediction " + i);
				avgRat = computeUserAvgRat(review._1, trainValuesAsList);
				avgDev = computeGlobalAvgDev(review._2, trainValuesAsList);
				prediction = predict(avgRat, avgDev, globalAvg)
			     ) yield (review._3, prediction)

	  return predictions

}




   def computeMae(predictions: List[(Int, Double)]): Double = {
	// ora calcoliamo il MAE
	val errors = for (couple <- predictions;
	  	       	increment = (couple._2 - couple._1).abs
	  		) yield increment 
	  var mae = errors.sum
	  mae = mae / testValuesAsList.size
	  return mae
   }


   def extractValues(path: String): List[(Int, Int, Int)] = {
     val parsed =
            for (line<-Source.fromFile(path).getLines;
                 Array(user, movie, rating, timestamp) = line.split("\t")
            ) yield (user.toInt, movie.toInt, rating.toInt)// rimuovo il timestamp
     val valuesAsList = parsed.toList
     return valuesAsList

   }


  def main(args: Array[String]) {
/*
Main function that handles everything
*/
    val trainDataPath = "/Users/rob/Documents/EPFL/systems/data/ml-100k/u2.base"
    val trainValuesAsList = extractValues(trainDataPath)


    // QUESTION B1
    val globalAvgWithFunct = computeGlobalAvg(trainValuesAsList)
    val globalAvg = trainValuesAsList.map(_._3).sum/trainValuesAsList.size.toDouble
    println("Global average: " + globalAvg)
    println("Global average with function: " + globalAvgWithFunct)



    val avgRatUser1 = computeUserAvgRat(1, trainValuesAsList)
    val avgRatItem1 = computeItemAvgRat(1, trainValuesAsList)
    val avgDevItem1 = computeGlobalAvgDev(1, trainValuesAsList)

    println("Average rating user 1 with function: " + avgRatUser1)
    println("Average rating item 1 with function: " + avgRatItem1)
    println("Average deviation item 1: " + avgDevItem1)


    println("Prediction user 1, item 1: " + predict(avgRatUser1, avgDevItem1, globalAvg))


    // QUESTION B2

    val testDataPath = "/Users/rob/Documents/EPFL/systems/data/ml-100k/u2.test"
    val testValuesAsList = extractValues(testDataPath)

    //mae with baseline prediction function
    val starting_time = System.nanoTime()
    val predictions = computePredictionsEq5(testValuesAsList, trainValuesAsList)
    val maePredictionFunct = computeMae(predictions)
    val timing = System.nanoTime() - starting_time
    val resultBaseline: Seq[Double] = Seq(mae, timing)


    //mae with global average
    val starting_time = System.nanoTime()
    val predictions = computePredictionsGlobalAvg(testValuesAsList, trainValuesAsList)
    val maeGlobalAvg = computeMae(predictions)
    val timing = System.nanoTime() - starting_time
    val resultGlobalAvg: Seq[Double] = Seq(mae, timing)


    //mae with average ratings for users
    val starting_time = System.nanoTime()
    val predictions = computePredictionsUserAvg(testValuesAsList, trainValuesAsList)
    val maeUserAvg = computeMae(predictions)
    val timing = System.nanoTime() - starting_time
    val resultUserAvg: Seq[Double] = Seq(mae, timing)

    //mae with average ratings for items
    val starting_time = System.nanoTime()
    val predictions = computePredictionsItemAvg(testValuesAsList, trainValuesAsList)
    val maeItemAvg = computeMae(predictions)
    val timing = System.nanoTime() - starting_time
    val resultItemAvg: Seq[Double] = Seq(mae, timing)


    println("MAE with baseline prediction function: " + maePredictionFunct)
    println("Time needed: " + resultBaseline._1)
    println("\n")

    println("MAE with global average: " + maeGlobalAvg)
    println("Time needed: " + resultGlobalAvg._1)
    println("\n")

    println("MAE with average ratings for users: " + maeUserAvg)
    println("Time needed: " + resultUserAvg._1)
    println("\n")

    println("MAE with average ratings for items: " + maeItemAvg)
    println("Time needed: " + resultItemAvg._1)
    println("\n")




  }
}