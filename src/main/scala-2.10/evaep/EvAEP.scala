package evaep

/**
  * The MIT License
  *
  * Copyright 2016 Ángel Miguel García-Vico.
  *
  * Permission is hereby granted, free of charge, to any person obtaining a copy
  * of this software and associated documentation files (the "Software"), to deal
  * in the Software without restriction, including without limitation the rights
  * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  * copies of the Software, and to permit persons to whom the Software is
  * furnished to do so, subject to the following conditions:
  *
  * The above copyright notice and this permission notice shall be included in
  * all copies or substantial portions of the Software.
  *
  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  * THE SOFTWARE.
  */

/**
  * Created by Ángel Miguel García-Vico (agvico@ujaen.es) on 15/12/16.
  */

import java.io._
import java.text.DecimalFormat

import org.apache.spark.SparkContext
import utils.{ConfusionMatrix, FisherExact, QualityMeasures, Randomize}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source


class EvAEP extends Serializable {

  // The number of maps for spark
  // Structures
  val Variables: TableVar = new TableVar
  // Set of variables of the dataset and their characteristics
  private val Examples: TableDat = new TableDat
  // Set of instances of the dataset
  private val AG: Genetic = new Genetic
  // Genetic Algorithm
  // Output quality measure file
  var partitions: Int = 0
  private var seed: Long = 0
  // Seed for radom generator
  private var nombre_alg: String = "EvAEP-Spark"
  // ALgorithm name
  private var input_file_tra: String = ""
  // Input mandatory file training
  private var input_file_tst: String = ""
  // Input mandatory file test
  private var input_file_ref: String = ""
  // Input mandatory file training
  private var output_file_tra: String = ""
  // Output mandatory file training
  private var output_file_tst: String = ""
  // Output mandatory file test
  private var rule_file: String = ""
  // Auxiliary output file for rules
  private var seg_file: String = ""
  // Auxiliary output file for tracking
  private var qmeasure_file: String = ""

  /**
    * It Prints the parameters obtained from the parameters file.
    * ONLY FOR DEBUG PURPOSES
    */
  def printParameters(): Unit = {

    println(nombre_alg)
    println(input_file_tra)
    println(input_file_tst)
    println(this.input_file_ref)
    println(output_file_tra)
    println(output_file_tst)
    println(rule_file)
    println(seg_file)
    println(qmeasure_file)
    println(AG.getRulesRep)
    println(seed)
    println(Variables.getNLabel)
    println(AG.getNEval)
    println(AG.getLengthPopulation)
    println(AG.getProbCross)
    println(AG.getProbMutation)
    println(AG.getFitness)
    println(AG.getInitialisation)
    println(AG.getRoundRobin)
    println(partitions)
    println("class: ".concat(Variables.getNClass.toString()))
  }

  /**
    * It executes the EvAEP-Spark algorithm
    */
  def runAlgorithm(sc: SparkContext, numPartitions: Int, nLabels: Int): Unit = {
    println("\nEvAEFP-Spark Implementation\n")

    // Start time
    val t_ini = System.currentTimeMillis()

    //printParameters() // Print only when debugging

    // Read the data
    println("\nReading the data...\n")
    readDataset(this.input_file_tra, sc, numPartitions, nLabels)

    if (this.seed != 0)
      Randomize.setSeed(this.seed)

    val popFinal: ArrayBuffer[Individual] = new ArrayBuffer[Individual](0)
    var classFinal: ArrayBuffer[Int] = new ArrayBuffer[Int](0)

    //Starts the algorithm
    if (!AG.getRoundRobin) {
      //--------------------
      //ONE VERSUS ALL STUDY
      //--------------------
      println("\nOne Vs. All STUDY")
      var numRulesGenerated = 0

      for (clase <- 0 until Variables.getNClass) {
        val actualClass: String = Variables.classNames(clase)

        Variables.setNumClassObj(clase)
        Variables.setNameClassObj(actualClass)
        println("Generate rules for class: " + actualClass)

        // Set examples as not covered
        Examples.cubiertos.clear(0, Examples.getNEx)

        // Load the number of examples of the target class
        Examples.setExamplesClassObj(Variables.getNumClassObj)

        // Variables Initialization
        Examples.setExamplesCovered(0)
        Examples.setExamplesCoveredClass(0)
        var terminar: Boolean = false

        println("\nTarget class number: " + Variables.getNumClassObj + " (value " + Variables.getNameClassObj + ")")

        val contents: String = "\n" concat "--------------------------------------------\n" concat "|                 Class " concat Variables.getNumClassObj.toString() concat "                  |\n" concat "--------------------------------------------\n\n"
        println(contents)

        println("Number of rule: \n")

        var rulesClass: Boolean = false



        do {
          // Execute the genetic algorithm
          val result: Individual = AG.GeneticAlgorithm(Variables, Examples, seg_file)
          println("CALLS TO RANDOMIZE: " + Randomize.calls)
          println("GENS: " + AG.getGen)
          // Check the stopping condition
          //terminar = true
          terminar = if ((result.getMeasures.getGRat < 1) || Examples.getExamplesCoveredClass == Examples.getExamplesClassObj || result.getMeasures.getNSup == 0) {
            true
          } else {
            false
          }
          if (!rulesClass || !terminar) {
            // Print the resulting rule
            println("#" + numRulesGenerated + ":\n")
            result.Print("")
            result.getMeasures.Print("", AG)

            // add the individual the results set
            popFinal += result
            classFinal += clase
            numRulesGenerated += 1

            // Update the Examples structure (This should be optimised)

            for(j <- 0 until result.cubre.size()){

              if (result.cubre.get(j)) {
                if (!Examples.getCovered(j)) {
                  Examples.setCovered(j, true)
                  Examples.setExamplesCovered(Examples.getExamplesCovered + 1)
                  if (Examples.getClass(j) == Variables.getNumClassObj)
                    Examples.setExamplesCoveredClass(Examples.getExamplesCoveredClass + 1)
                }
              }
            }
            println("Ej. cubiertos TOTAL: " + Examples.getExamplesCovered)
            println("EJ. cubiertos CLASE: " + Examples.getExamplesCoveredClass)
            println("EJ: " + Examples.cubiertos.cardinality())


          }
          rulesClass = true

        } while (!terminar)
      }
    } else {
      //-----------------
      //ROUND ROBIN STUDY
      //-----------------
      println("Round Robin STUDY")
    }


    println("Algorithm terminated\n\n")

    println("Calculating measures\n")
    println("====================\n\n")

    // Calculate training predictions (.tra file)
    calculatePredictions(popFinal, classFinal, false)

    Examples.datosRDD.unpersist()

    // Read test data and calculate the predictions
    readDataset(input_file_tst, sc, numPartitions, nLabels)
    calculatePredictions(popFinal, classFinal, true)
    calculateQualityMeasures(popFinal, classFinal)
    WriteRules(popFinal, popFinal.length, classFinal)

    // Finish the algorithm execution, for debug purposes
    val t_end = System.currentTimeMillis()
    val execTime: Double = (t_end - t_ini) / 1000.0
    println("BUILD SUCCESSFUL. (" concat execTime.toString() concat " seconds.)")
  }

  def readParameters(file: String, sc: SparkContext): Unit = {
    //val stream = getClass.getResourceAsStream(file)
    //val fich = scala.io.Source.fromInputStream( stream ).getLines().toList
    //val fichero = sc.parallelize(fich)
    //val fichero = sc.textFile(file)
    val fichero = Source.fromFile(file)
    val lineas = fichero.getLines().map(l => l.split(" "))

    for (linea <- lineas) {
      if (linea(0).equalsIgnoreCase("algorithm")) {
        nombre_alg = linea(2)
      } else if (linea(0).equalsIgnoreCase("inputData")) {
        input_file_ref = linea(2).trim().substring(1, linea(2).length() - 1)
        input_file_tra = linea(3).trim().substring(1, linea(3).length() - 1)
        input_file_tst = linea(4).trim().substring(1, linea(4).length() - 1)
      } else if (linea(0).equalsIgnoreCase("outputData")) {
        output_file_tra = linea(2).trim().substring(1, linea(2).length() - 1)
        output_file_tst = linea(3).trim().substring(1, linea(3).length() - 1)
        rule_file = linea(4).trim().substring(1, linea(4).length() - 1)
        qmeasure_file = linea(5).trim().substring(1, linea(5).length() - 1)
        seg_file = linea(6).trim().substring(1, linea(6).length() - 1)
      } else if (linea(0).equalsIgnoreCase("RulesRep")) {
        //Inicializar variable algoritmo genético
        AG.setRulesRep(linea(2).toUpperCase)
      } else if (linea(0).equalsIgnoreCase("seed")) {
        seed = linea(2).toLong
      } else if (linea(0).equalsIgnoreCase("nLabels")) {
        this.Variables.setNLabel(linea(2).toInt)
      } else if (linea(0).equalsIgnoreCase("nEval")) {
        //Inicializar variable algoritmo genético
        AG.setNEval(linea(2).toInt)
      } else if (linea(0).equalsIgnoreCase("popLength")) {
        //Inicializar variable algoritmo genético
        AG.setLengthPopulation(linea(2).toInt)
      } else if (linea(0).equalsIgnoreCase("crossProb")) {
        //Inicializar variable algoritmo genético
        AG.setProbCross(linea(2).toFloat)
      } else if (linea(0).equalsIgnoreCase("mutProb")) {
        //Inicializar variable algoritmo genético
        AG.setProbMutation(linea(2).toFloat)
      } else if (linea(0).equalsIgnoreCase("NumPartitions")) {
        partitions = linea(2).toInt
      } else if (linea(0).equalsIgnoreCase("Fitness")) {
        AG.setFitness(linea(2))
      } else if (linea(0).equalsIgnoreCase("Initialisation")) {
        AG.setInitialisation(linea(2))
      } else if (linea(0).equalsIgnoreCase("RoundRobin")) {
        if (linea(2).equalsIgnoreCase("NO")) {
          AG.setRoundRobin(false)
        } else {
          AG.setRoundRobin(true)
        }
      }
    }
  }

  /**
    * Reads a KEEL dataset, fuzzifies numeric variables and store the results
    * in "variables" the information of the variables and in
    * "instanciasTotal.datosRDD" the RDD of the instances.
    */
  def readDataset(file: String, sc: SparkContext, numPartitions: Int, nLabels: Int) {
    // Catch the dataset, and parse ir
    //val stream = getClass.getResourceAsStream(file)
    //val fich = scala.io.Source.fromInputStream( stream ).getLines().toList
    val fichero = sc.textFile(file, numPartitions)
    //val fichero = sc.parallelize(fich,numPartitions).cache()
    val head = fichero.filter { linea => linea.startsWith("@") }
    val linesHead = head.filter { line => line.startsWith("@attribute") }
    val linesInstances = fichero.filter(linea => !linea.contains("@") && !linea.trim().isEmpty)//.cache()
    val inputs = fichero.filter { linea => linea.startsWith("@inputs") }.first()
    val outputs = fichero.filter { linea => linea.startsWith("@output") }.first()

    val outputs_values = linesHead
      .map(line => line.split("( *)(\\{)( *)|( *)(\\})( *)|( *)(\\[)( *)|( *)(\\])( *)|( *)(,)( *)| "))
      .filter(l =>
        outputs.matches(".* " concat l(1))
      ).first().drop(2)

    Variables.classNames = new Array[String](outputs_values.length)
    var i = -1
    outputs_values.foreach { x =>
      i += 1
      Variables.classNames(i) = x
    }
    // Parse, initialize and fuzzify the variables
    Variables.setNClass(outputs_values.length)
    Variables.setNLabel(nLabels) // Sets the number of fuzzy labels for numeric class
    Variables.setNClass(outputs_values.length)
    Variables.loadVars(linesHead, inputs)
    Variables.InitSemantics("")
    //Variables.imprimir()

    // Read the instances
    Examples.loadData2(linesInstances, Variables, outputs_values, sc, numPartitions)
  }


  /**
    * It calculates the class predicted for a given set of instances. Also, it stores a file with the predicted results
    * (.tra or .tst) file.
    *
    * @param pop
    * @param clasInd
    * @param test
    */
  def calculatePredictions(pop: ArrayBuffer[Individual], clasInd: ArrayBuffer[Int], test: Boolean) = {

    val predictions = Examples.datosRDD.map(x => {
      val data = x._2

      var j = -1
      val disp = pop.map(individual => {
        j += 1
        var disparoFuzzy = 1f
        var numVarNoInterv = 0
        for (i <- 0 until Variables.getNVars) {
          if (!Variables.getContinuous(i)) {
            // Discrete variable
            if (individual.getCromElem(i) <= Variables.getMax(i)) {
              if (data.getDat(i) != individual.getCromElem(i) && !data.getLost(Variables, 0, i)) {
                disparoFuzzy = 0
              }
            } else {
              // Variable does not take part
              numVarNoInterv += 1
            }
          } else {
            // Continuous variable
            if (individual.getCromElem(i) < Variables.getNLabelVar(i)) {
              if (!data.getLost(Variables, 0, i)) {
                if (individual.NumInterv(data.getDat(i), i, Variables) != individual.getCromElem(i))
                  disparoFuzzy = 0
              }
            } else {
              // Variable does not take part
              numVarNoInterv += 1
            }
          }
        }
        if (numVarNoInterv >= Variables.getNVars)
          disparoFuzzy = 0

        (disparoFuzzy, numVarNoInterv, clasInd(j))
      })


      // Now that we have the disparoFuzzy and numVarNoInterv, calculates the prediction.
      val normSum = new Array[Float](Variables.getNClass)
      for (i <- 0 until Variables.getNClass) {
        val reglasDisp = disp.count(x => x._1 > 0).toFloat
        val sum = disp.map(x => {
          if (x._1 > 0 && x._3 == i)
            x._1
          else
            0
        }).sum
        normSum(i) = if (reglasDisp > 0)
          sum
        else 0
      }

      // Return the prediction as the maximum as the normSum and save in a file
      (Variables.classNames(data.getClas), Variables.classNames(normSum.indexOf(normSum.max)))

    })

    if (!test) {
      predictions.saveAsTextFile(this.output_file_tra)
      // calculate precision
      val acc = predictions.map(x => {
        if (x._1.equals(x._2))
          1
        else
          0
      }).sum() / Examples.getNEx.toDouble
      println("TRAINING ACCURACY: " + acc)
    } else {
      predictions.saveAsTextFile(this.output_file_tst)
      // calculate precision
      val acc = predictions.map(x => {
        if (x._1.equals(x._2))
          1
        else
          0
      }).sum() / Examples.getNEx.toDouble
      println("TEST ACCURACY: " + acc)
    }
  }


  /**
    * It calculates the quality measures for the rules for the actual data stored in the Examples variable
    *
    * @param pop
    * @param classFinal
    */
  def calculateQualityMeasures(pop: ArrayBuffer[Individual], classFinal: ArrayBuffer[Int]): Unit = {
  val neje = Examples.getNEx
    // Evaluates all individuals in the final population against the test data
    val confusionMatrices = Examples.datosRDD.map(x => {
      var i = -1
      val data = x._2

      pop.map(individual => {
        i += 1
        val classIndiv = classFinal(i)
        var disparoCrisp = 1
        var disparoFuzzy = 1f
        var matrix = new ConfusionMatrix(neje)
        for (j <- 0 until Variables.getNVars) {
          if (!Variables.getContinuous(j)) {
            // Discrete variable
            if (individual.getCromElem(j) <= Variables.getMax(j)) {
              if (individual.getCromElem(j) != data.getDat(j) && !data.getLost(Variables, 0, j)) {
                disparoCrisp = 0
                disparoFuzzy = 0
              }
            } else {
              matrix.numVarNoInterv += 1
            }
          } else {
            // Continuous variable
            if (individual.getCromElem(j) < Variables.getNLabelVar(j)) {
              if (!data.getLost(Variables, 0, j)) {
                disparoFuzzy = Math.min(disparoFuzzy, Variables.Fuzzy(j, individual.getCromElem(j), data.getDat(j)))
                if (individual.NumInterv(data.getDat(j), j, Variables) != individual.getCromElem(j)) {
                  disparoCrisp = 0
                }
              }
            } else {
              matrix.numVarNoInterv += 1
            }
          }
        }

        matrix.gradoCompAntFuzzy += disparoFuzzy
        if (disparoFuzzy > 0) {
          //matrix.coveredExamples(x._1 toInt) += x._1
          if (data.getClas == classIndiv) {
            matrix.gradoCompAntClassFuzzy += disparoFuzzy
          }
        }

        if (disparoCrisp > 0) {
          matrix.ejAntCrisp += 1
          //matrix.coveredExamples(x._1 toInt) += x._1
          if (data.getClas == classIndiv) {
            matrix.ejAntClassCrisp += 1
            matrix.tp += 1
          } else {
            matrix.ejAntNoClassCrisp += 1
            matrix.fp += 1
          }
        } else {
          if (data.getClas == classIndiv) {
            matrix.fn += 1
          } else {
            matrix.tn += 1
          }
        }

        // Return
        matrix
      })
    }).reduce((x, y) => {
      val ret = new ArrayBuffer[ConfusionMatrix](0)
      //trials += y.length
      for (i <- 0 until y.length) {
        val toRet: ConfusionMatrix = new ConfusionMatrix(neje)
        //toRet.coveredExamples.or(x(i).coveredExamples)
        //toRet.coveredExamples.or(x(i).coveredExamples)
        //toRet.coveredExamples.appendAll(x(i).coveredExamples)
        //toRet.coveredExamples.appendAll(y(i).coveredExamples)

        toRet.ejAntClassCrisp = x(i).ejAntClassCrisp + y(i).ejAntClassCrisp
        toRet.ejAntClassNewCrisp = x(i).ejAntClassNewCrisp + y(i).ejAntClassNewCrisp
        toRet.ejAntCrisp = x(i).ejAntCrisp + y(i).ejAntCrisp
        toRet.ejAntNoClassCrisp = x(i).ejAntNoClassCrisp + y(i).ejAntNoClassCrisp
        toRet.fn = x(i).fn + y(i).fn
        toRet.fp = x(i).fp + y(i).fp
        toRet.tn = x(i).tn + y(i).tn
        toRet.tp = x(i).tp + y(i).tp
        toRet.numVarNoInterv = x(i).numVarNoInterv + y(i).numVarNoInterv
        toRet.gradoCompAntClassFuzzy = x(i).gradoCompAntClassFuzzy + y(i).gradoCompAntClassFuzzy
        toRet.gradoCompAntFuzzy = x(i).gradoCompAntFuzzy + y(i).gradoCompAntFuzzy
        ret += toRet
      }
      // Return !
      ret
    })

    var AvNVAR: Double = 0
    var AvLENG: Double = 0
    var AvUNUS: Double = 0
    var AvGAIN: Double = 0
    var AvSENS: Double = 0
    var AvDIFS: Double = 0
    var AvFCNF: Double = 0
    var AvTPr: Double = 0
    var AvFPr: Double = 0
    var AvDH: Double = 0
    var AvTF: Double = 0
    var AvGR: Double = 0

    var sixDecimals: DecimalFormat = new DecimalFormat("0.000000")
    var threeInts: DecimalFormat = new DecimalFormat("000")


    // Calculate the measures of each individual
    var i = -1
    val pw: PrintWriter = new PrintWriter(qmeasure_file)
    pw.println("Number \tClass \tSize \tNVar \tLength \tUnusualness \tGain" +
      "\tSensitivity \tSupportDif \tFConfidence \tGrowthRate \tTEFisher \tHellinger \tTPr \tFPr")

    pop.foreach(individual => {
      i += 1
      val confMatrix = confusionMatrices(i)

      // Mark covered examples
      confMatrix.coveredExamples.foreach(x => {
        individual.cubre.set(x toInt)
      })
      //individual.cubre.or(confMatrix.coveredExamples)

      val numVarNoInterv: Int = confMatrix.numVarNoInterv / Examples.getNEx

      // calulate the measures

      // variables
      val vars = Variables.getNVars - numVarNoInterv


      val medidas = if (vars > 0) {
        // SENS
        val sens: Float = if (Examples.getExamplesClass(classFinal(i)) != 0) {
          confMatrix.ejAntClassCrisp.toFloat / Examples.getExamplesClass(classFinal(i)).toFloat
        } else {
          0
        }
        individual.medidas.setSens(sens)

        // LENGTH
        val length: Float = if (confMatrix.ejAntCrisp != 0) {
          1f / confMatrix.ejAntCrisp.toFloat
        } else {
          0
        }
        individual.medidas.setLength(length)
        // COVE
        val cove: Float = if (confMatrix.ejAntCrisp != 0) {
          confMatrix.ejAntCrisp.toFloat / Examples.getNEx.toFloat
        } else {
          0
        }

        //UNUS
        val unus: Float = if (confMatrix.ejAntCrisp == 0) {
          0
        } else {
          val classPct = Examples.getExamplesClass(classFinal(i)).toFloat / Examples.getNEx.toFloat
          val initUnus = cove * ((confMatrix.ejAntClassCrisp.toFloat / confMatrix.ejAntCrisp.toFloat) - (Examples.getExamplesClass(classFinal(i)).toFloat / Examples.getNEx.toFloat))
          val minUnus = (1 - classPct) * (0 - classPct)
          val maxUnus = classPct * (1 - classPct)
          (initUnus - minUnus) / (maxUnus - minUnus)
        }
        individual.medidas.setUnus(unus)

        //GAIN
        val gain: Float = if (confMatrix.ejAntCrisp == 0 || sens == 0) {
          if (Examples.getExamplesClass(classFinal(i)) != 0) {
            sens * (0 - Math.log10(Examples.getExamplesClass(classFinal(i)).toFloat / Examples.getNEx.toFloat).toFloat)
          } else {
            0
          }
        } else {
          sens * (Math.log10(sens / cove).toFloat - (Math.log10(Examples.getExamplesClass(classFinal(i)).toFloat / Examples.getNEx.toFloat)).toFloat)
        }
        individual.medidas.setGain(gain)

        //CONF
        val fcnf: Float = if (confMatrix.gradoCompAntFuzzy != 0) {
          confMatrix.gradoCompAntClassFuzzy / confMatrix.gradoCompAntFuzzy
        } else {
          0
        }
        individual.medidas.setConf(fcnf)
        // TPR
        val tpr = if (Examples.getExamplesClass(classFinal(i)) != 0) {
          confMatrix.ejAntClassCrisp.toFloat / Examples.getExamplesClass(classFinal(i))
        } else {
          0
        }
        individual.medidas.setTPr(tpr)
        // FPr
        val fpr = if (Examples.getExamplesClass(classFinal(i)) != 0) {
          confMatrix.ejAntNoClassCrisp.toFloat / (Examples.getNEx - Examples.getExamplesClass(classFinal(i))).toFloat
        } else {
          0
        }
        individual.medidas.setFPr(fpr)

        val difs = Math.abs(tpr - fpr)
        individual.medidas.setDifS(difs)

        //GRAT
        val grat: Float = if (tpr != 0 && fpr != 0) {
          tpr / fpr
        } else if (tpr != 0 && fpr == 0) {
          Float.PositiveInfinity
        } else {
          0
        }
        individual.medidas.setGRat(grat)
        // FISHER
        val fe: FisherExact = new FisherExact(Examples.getNEx)
        val fisher: Float = fe.getTwoTailedP(confMatrix.tp, confMatrix.fp, confMatrix.fn, confMatrix.tn).toFloat
        individual.medidas.setFisher(fisher)

        // Hellinger
        val parte1: Float = if (Examples.getExamplesClass(classFinal(i)) != 0) {
          Math.sqrt(confMatrix.ejAntClassCrisp.toFloat / Examples.getExamplesClass(classFinal(i))).toFloat - Math.sqrt(confMatrix.fn / Examples.getExamplesClass(classFinal(i))).toFloat
        } else {
          0
        }

        val parte2: Float = if ((Examples.getNEx - Examples.getExamplesClass(classFinal(i))) != 0) {
          Math.sqrt(confMatrix.ejAntNoClassCrisp.toFloat / (Examples.getNEx - Examples.getExamplesClass(classFinal(i))).toFloat).toFloat - Math.sqrt(confMatrix.tn.toFloat / (Examples.getNEx - Examples.getExamplesClass(classFinal(i))).toFloat).toFloat
        } else {
          0
        }
        val dh: Float = Math.sqrt(Math.pow(parte1, 2) + Math.pow(parte2, 2)).toFloat
        individual.medidas.setHellinger(dh)

        // Return
        individual.medidas
      } else {
        // If the rule is empty, set this QM to 0
        individual.medidas = new QualityMeasures
        individual.medidas
      }


      // Now print the results in a file


      pw.print("" + threeInts.format(i) + "   ")
      pw.print("\t" + threeInts.format(classFinal(i)))
      pw.print("\t-")
      pw.print("\t" + sixDecimals.format(Variables.getNVars - numVarNoInterv))
      pw.print("\t" + sixDecimals.format(medidas.getLength))
      pw.print("\t" + sixDecimals.format(medidas.getUnus))
      if (medidas.getGain == Float.PositiveInfinity) pw.print("\tINFINITY")
      else pw.print("\t" + sixDecimals.format(medidas.getGain))
      pw.print("\t" + sixDecimals.format(medidas.getSens))
      pw.print("\t" + sixDecimals.format(medidas.getDifS))
      pw.print("\t" + sixDecimals.format(medidas.getConf))
      if (medidas.getGRat == Float.PositiveInfinity) pw.print("\tINFINITY")
      else pw.print("\t" + sixDecimals.format(medidas.getGRat))
      pw.print("\t" + sixDecimals.format(medidas.getFisher))
      pw.print("\t" + sixDecimals.format(medidas.getHellinger))
      pw.print("\t" + sixDecimals.format(medidas.getTPr))
      pw.print("\t" + sixDecimals.format(medidas.getFPr))
      pw.println()

      AvNVAR += Variables.getNVars - numVarNoInterv
      AvLENG += medidas.getLength
      AvUNUS += medidas.getUnus
      AvGAIN += medidas.getGain
      AvSENS += medidas.getSens
      AvDIFS += medidas.getSens
      AvFCNF += medidas.getConf
      AvTPr += medidas.getTPr
      AvFPr += medidas.getFPr
      AvDH += medidas.getHellinger
      if (medidas.getFisher < 0.1) AvTF += 1
      if (medidas.getGRat > 1) AvGR += 1

    })

    //Calcular soporte con Examples
    /*val exCov: Int = pop.map(x => x.cubre).reduce((x,y) => {
      (x,y).zipped.map(_ || _)
    }).count(x => x)*/

    val nrules = pop.length
    pw.print("---\t")
    pw.print("---")
    pw.print("\t" + nrules)
    pw.print("\t" + sixDecimals.format(AvNVAR / nrules))
    pw.print("\t" + sixDecimals.format(AvLENG / nrules))
    pw.print("\t" + sixDecimals.format(AvUNUS / nrules))
    pw.print("\t" + sixDecimals.format(AvGAIN / nrules))
    pw.print("\t" + sixDecimals.format(AvSENS / nrules))
    pw.print("\t" + sixDecimals.format(AvDIFS / nrules))
    pw.print("\t" + sixDecimals.format(AvFCNF / nrules))
    pw.print("\t" + sixDecimals.format(AvGR / nrules))
    pw.print("\t" + sixDecimals.format(AvTF / nrules))
    pw.print("\t" + sixDecimals.format(AvDH / nrules))
    pw.print("\t" + sixDecimals.format(AvTPr / nrules))
    pw.print("\t" + sixDecimals.format(AvFPr / nrules))
    pw.println()
    pw.close()
    // END
  }


  /**
    * <p>
    * Writes the rule and the quality measures
    * </p>
    */
  def WriteRules(pop: ArrayBuffer[Individual], nrules: Int, classFinal: ArrayBuffer[Int]) = {


    val pw = new PrintWriter(rule_file)
    var aux = -1
    pop.foreach(indiv => {
      aux += 1

      pw.println("GENERATED RULE " + aux)
      pw.println("\tAntecedent")
      val regla: CromCAN = indiv.getIndivCromCAN
      for (auxi <- 0 until Variables.getNVars) {
        if (!Variables.getContinuous(auxi)) {
          // Discrete variable
          if (regla.getCromElem(auxi) < Variables.getNLabelVar(auxi)) {
            pw.print("\t\tVariable " + Variables.getNameVar(auxi) + " = ")
            pw.println(Variables.getNameLabel(auxi, regla.getCromElem(auxi)))
          }
        }
        else {
          // Continuous variable
          if (regla.getCromElem(auxi) < Variables.getNLabelVar(auxi)) {
            pw.print("\t\tVariable " + Variables.getNameVar(auxi) + " = ")
            pw.print("Label " + regla.getCromElem(auxi))
            pw.print(" \t (" + Variables.getX0(auxi, regla.getCromElem(auxi)))
            pw.print(" " + Variables.getX1(auxi, regla.getCromElem(auxi)))
            pw.println(" " + Variables.getX3(auxi, regla.getCromElem(auxi)) + ")")
          }
        }
      }

      pw.println("\tConsecuent: " + Variables.classNames(classFinal(aux)) + "\n")
    })

    pw.close()
  }

}
