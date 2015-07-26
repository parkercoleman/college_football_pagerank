package com.pcoleman.collegefootballpr

import java.nio.charset.StandardCharsets
import java.nio.file.{Paths, Files}

import scala.collection.mutable
import scala.io.Source



class DotFileGenerator(matrix: Map[String, Set[String]], rank: Map[String, Double], fileName: String){

  def generateDotFile(): String ={
    val output = new StringBuilder("digraph G {\n")
    val rankSeq = rank.toSeq.sortWith(_._2 > _._2)
    var i = 1
    /*Declare all the nodes*/
    for(t <- rankSeq){
      output.append("\t%s [label=\"%s\\nRANK: %s\\n PR_VAL:%s \"]\n".format(t._1.hashCode().abs, t._1, i, t._2))
      i+=1
    }
    /*Define lines between nodes*/
    for(team <- matrix){
      for(lostTo <- team._2){
        output.append("\t%s->%s\n".format(team._1.hashCode.abs, lostTo.hashCode.abs))
      }
    }
    output.append("}")
    output.toString()
  }

  def printOutputToFile(): Unit ={
    val s = generateDotFile()
    /*Thanks Vladimir Matveev http://stackoverflow.com/questions/6879427/scala-write-string-to-file-in-one-statement*/
    Files.write(Paths.get(fileName), generateDotFile().getBytes(StandardCharsets.UTF_8))
  }
}

class DataParser(fileName: String) {

  /**
   * The sparse matrix will be broken up by year, with each year pointing to a different sparse matrix.
   * The matricies themselves will be hashmaps where the keys are team names, each pointing to a set of teams they've lost to that season
   * If teams tied, the link is bi-directional.  In the rare occasion teams play each other multiple times per season, a result may be thrown out
   *
   * example:
   * 1996 -> {
   * "teamName" -> ["lostto1", "lostto2", "lostto3"]
   * }
   */
  def generateSparseMatrix(): Map[Int, Map[String, Set[String]]] = {

    val sparseMatrix = new mutable.HashMap[Int, mutable.HashMap[String, mutable.Set[String]]]()

    var currentYear = 0;
    Source.fromFile(fileName).getLines().foreach(line => {
      if (line.startsWith("--")) {
        currentYear = line.replace("--", "").toInt
        sparseMatrix(currentYear) = new mutable.HashMap[String, mutable.Set[String]]
      }
      else {
        val charArray = Array.ofDim[Char](line.length)
        line.getChars(0, line.length, charArray, 0)


        val resultList = mutable.MutableList[String]()
        List((0, 11), (11, 38), (38, 43), (43, 70), (70, 74)).foreach(sliceVals => {
          resultList += charArray.slice(sliceVals._1, sliceVals._2).map(_.toString()).reduce(_ + _).trim()
        })

        /*left hand team*/
        val lht = resultList(1)
        /*right hand team*/
        val rht = resultList(3)
        val lhs = resultList(2).toInt
        val rhs = resultList(4).toInt

        /*Ensure both teams are in the set*/
        if(!sparseMatrix(currentYear).keySet.contains(lht)) sparseMatrix(currentYear)(lht) = new mutable.HashSet[String]
        if(!sparseMatrix(currentYear).keySet.contains(rht)) sparseMatrix(currentYear)(rht) = new mutable.HashSet[String]


        /*Add this line's data to the sparseMatrix*/
        if(lhs > rhs){
          /*Left hand team won*/
          sparseMatrix(currentYear)(rht) += lht
        }
        else if(lhs < rhs){
          /*Right hand team won*/
          sparseMatrix(currentYear)(lht) += rht
        }
        else{
          /*A tie*/
          sparseMatrix(currentYear)(lht) += rht
          sparseMatrix(currentYear)(rht) += lht
        }
      }
    })

    /*Now go in and make everything immutable haha what a mess*/
    sparseMatrix.map(k1 => {(k1._1, k1._2.map(k2=>{(k2._1, k2._2.toSet)}).toMap)}).toMap
  }
}