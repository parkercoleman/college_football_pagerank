package com.pcoleman.collegefootballpr

class PageRank(dataSet: Map[String, Set[String]]) {

  def calculatePageRank(): Map[String, Double] ={

    val ITERATION_NUMBER = 20
    val LAMBDA = .8
    val N = dataSet.keySet.size

    /*Initialized to 1.0/N */
    var currentRank = dataSet.map(keyValPair =>{(keyValPair._1, 1.0/N)})

    for( i <- 1 to ITERATION_NUMBER) {
      currentRank = currentRank.map(keyValTuple => {
        val teamName = keyValTuple._1

        /*Get the likely hood that we reach this nodes connected to us*/
        val prIncoming = dataSet.filter(_._2.contains(teamName)).foldLeft(0.0)((sum, node) => {
          sum + currentRank(node._1) / node._2.size
        })

        /*Get the likely hood we jump here from a sink node, so our SUM(PR) value = 100*/
        val prFromSinkNodes = dataSet.filter(_._2.isEmpty).foldLeft(0.0)((sum, node) =>{
          sum + currentRank(node._1) / N
        })

        (teamName, ((1 - LAMBDA) / N) + (LAMBDA * prIncoming) + (LAMBDA * prFromSinkNodes))
      })
    }
    currentRank.toMap
  }
}
