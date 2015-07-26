package com.pcoleman.collegefootballpr


object Main {

  def main(args: Array[String]): Unit = {
    val p = new DataParser("./src/main/resources/dwilson_data.txt")
    val year = args(0).toInt

    val m = p.generateSparseMatrix()
    val pr = new PageRank(m(year))
    val r = pr.calculatePageRank()

    println("RESULTS")
    r.toSeq.sortWith(_._2 > _._2).foreach(println(_))

    new DotFileGenerator(m(year).toMap, r, "output.dot").printOutputToFile()
  }
}
