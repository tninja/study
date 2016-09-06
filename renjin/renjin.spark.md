Goal
====

-   Incorporating Renjin with Spark, to make the R calculation scalable

Install
=======

Repackage Renjin jar (Required for spark \< 1.6)
------------------------------------------------

-   To use renjin on spark \< 1.6, we need to repackage it due to a guava version conflict between renjin & spark.
-   There used to be a repackaged version branch of renjin: <https://github.com/bedatadriven/renjin>
-   Building the repackaged version require certain dependencies (like gcc 4.6). I used a ubuntu 14.04 LTS on AWS to build this.

Protocol
========

Launch spark-shell with Renjin Jar
----------------------------------

``` bash
  export JARS="/Users/tninja/Downloads/renjin-script-engine-0.9.0-SNAPSHOT-jar-with-dependencies.jar,/Users/tninja/lib/spark-csv_2.10-1.3.0.jar"
  spark-shell --master yarn-client --jars "${JARS}"
```

Prepare data as RDD[String]
---------------------------

``` scala
  // The data is from weka example file
  val df = sqlContext.read.format("com.databricks.spark.csv").option("header", "true").load("/user/tninja/airline.arff.csv")

  df.registerTempTable("airlineTable")

  val dfWithYear = sqlContext.sql("""
                                                                SELECT Date, passenger_numbers, substring(Date, 1, 4) AS year
                                                                FROM airlineTable
                                                                """)

  val header = dfWithYear.columns.mkString("\t")

  val dataRdd = dfWithYear.rdd
  .groupBy( row => row.get(2) )
  .mapValues(
    rowList => {
        val rowText = rowList.map( row => row.mkString("\t")).mkString("\n")
        val rowTextWithHead = header ++ "\n" ++ rowText
        rowTextWithHead
    }
  ).values

  val predDf = dataRdd
  .map( rowTextWithHeader => {
    import org.renjin.script.RenjinScriptEngineFactory
    val factory = new RenjinScriptEngineFactory
    val engine = factory.getScriptEngine
    engine.eval(s"inputDfInText <- '${rowTextWithHeader}'")
    engine.eval("df <- read.csv(textConnection(inputDfInText), sep='\t', colClasses=c('POSIXct', 'numeric', 'numeric'))")
    engine.eval("pred.df <- as.data.frame(predict(lm(passenger_numbers ~ Date, data=df), newdata=df, interval='prediction',\
                            level=0.99))")
    engine.eval("pred.df").toString
  }).toDF

  predDf.take(3) // show the result
```

Call renjin in .map function
----------------------------

-   Embedded the data into R script as a csv string
-   Evaluate R code as a string
-   Return the result as a string (or you can choose other ways)

``` scala
  val predDf = dataRdd
  .map( rowTextWithHeader => {
    import org.renjin.script.RenjinScriptEngineFactory
    val factory = new RenjinScriptEngineFactory
    val engine = factory.getScriptEngine
    engine.eval(s"inputDfInText <- '${rowTextWithHeader}'")
    engine.eval("df <- read.csv(textConnection(inputDfInText), sep='\t', colClasses=c('POSIXct', 'numeric', 'numeric'))")
    engine.eval("pred.df <- as.data.frame(predict(lm(passenger_numbers ~ Date, data=df), newdata=df, interval='prediction',\
                            level=0.99))")
    engine.eval("pred.df").toString
  }).toDF

  predDf.take(3) // sow the result
```
