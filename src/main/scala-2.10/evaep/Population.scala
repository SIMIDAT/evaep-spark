package evaep




import utils.{ConfusionMatrix, FisherExact}

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
class Population extends Serializable{

  /**
    * <p>
    * Population of candidate rules
    * </p>
    */

  var indivi: Array[Individual]= null  // Population individuals
  private var num_indiv: Int = 0           // Max number of individuals in the population
  //var ej_cubiertos: Array[Boolean] = null;   // Covered examples of the population

  /**
    * <p>
    * Creates a population of Individual
    * </p>
    * @param numind          Number of individuals
    * @param numgen          Number of variables
    * @param neje            Number of examples
    * @param RulRep          Rules representation
    * @param Variables       Variables structure
    */
  def this (numind: Int, numgen: Int, neje: Int, RulRep: String, Variables: TableVar, trials: Int) = {
    this()
    indivi = new Array[Individual](numind)
    num_indiv = numind
    indivi = indivi.map { x => new IndCAN(numgen, neje, trials) }

    //ej_cubiertos = new Array[Boolean](neje)

    //ej_cubiertos = ej_cubiertos.map(x => {false})

  }


  /**
    * <p>
    * Biased random population initialization
    * </p>
    * @param Variables       Variables structure
    * @param Examples        Examples structure
    * @param porcVar         Percentage of variables to form the rules
    * @param porcPob         Percentage of population with biased initialisation
    * @param neje            Number of examples
    * @param nFile           File to write the population
    */
  def BsdInitPob (Variables: TableVar, Examples: TableDat, porcVar: Float, porcPob: Float, neje: Int, nFile: String): Unit = {

    val parteSesg: Int = (porcPob * num_indiv) toInt

    val biased = this.indivi.take(parteSesg).map(x => {
      x.BsdInitInd(Variables, Examples, porcVar, nFile)
      x
    })

    val rnd = this.indivi.takeRight(num_indiv - parteSesg).map(x => {
      x.RndInitInd(Variables, porcVar, neje, nFile)
      x
    })
    this.indivi = biased ++ rnd

    //ej_cubiertos = ej_cubiertos.map{x => false}

  }


  /**
    * <p>
    * Evaluates non-evaluated individuals
    * </p>
    * @param AG                   Genetic algorithm
    * @param Variables            Variables structure
    * @param Examples             Examples structure
    * @return                     Number of evaluations performed
    */
  def evalPop (AG : Genetic, Variables: TableVar, Examples : TableDat): Int = {
    val cubiertos = Examples.cubiertos.get(0,Examples.getNEx)
    val neje = Examples.getNEx

    /**
      *  EVALUATES THE POPULATION AGAINST ALL THE EXAMPLES
      *   WITH MAP-REDUCE :
      *
      *   THIS IS THE MAP PHASE. HERE WE CALCULATE FOR EACH EXAMPLE ITS CONFUSION MATRIX
      *   i.e., IF THE EXAMPLE IS A TP, TN, FP OR FN FOR ALL NON-EVALUATED INDIVIDUALS
      *   AND ADDITIONAL MEASURES LIKE WHETHER IS A NEW COVERED EXAMPLE OR NOT
      */

      val indivsToEval = indivi.filter(y => !y.evaluado)

    val confusionMatrices = Examples.datosRDD.mapPartitions(x => {
      var matrices: Array[ConfusionMatrix] = new Array[ConfusionMatrix](indivsToEval.length)
      matrices = matrices.map(x => new ConfusionMatrix(neje))
      while(x.hasNext){
        // Evaluate each example of the partitions against the whole non-evaluated population
        val d = x.next()
        val data = d._2
        val index = d._1
        for(i <- indivsToEval.indices){
          val individual = indivsToEval(i)
          val cromosoma = individual.getIndivCromCAN
          var disparoCrisp = 1
          var numVarNoInterv = 0
          // Calculates the disparoCrisp of each variable
          for(j <- 0 until Variables.getNVars){
            if(! Variables.getContinuous(j)){
              if(cromosoma.getCromElem(j) <= Variables.getMax(j)){
                if( (data.getDat(j) != cromosoma.getCromElem(j)) && (!data.getLost(Variables, 0, j))){
                  disparoCrisp = 0
                }
              } else {
                matrices(i).numVarNoInterv += 1
              }
            } else {
              if(cromosoma.getCromElem(j) < Variables.getNLabelVar(j)){
                if(!data.getLost(Variables, 0, j)){
                  if(individual.NumInterv(data.getDat(j), j, Variables) != cromosoma.getCromElem(j)){
                    disparoCrisp = 0
                  }
                }
              } else {
                matrices(i).numVarNoInterv += 1
              }
            }
          } // End for all chromosome values

          if(disparoCrisp > 0){
            individual.cubre.set(index toInt)
            matrices(i).ejAntCrisp += 1
            //matrices(i).coveredExamples += index
            if(data.getClas == Variables.getNumClassObj){
              matrices(i).ejAntClassCrisp += 1
              matrices(i).tp += 1
            } else {
              matrices(i).ejAntNoClassCrisp += 1
              matrices(i).fp += 1
            }
            // cubreClase[Examples.getClass(i)]++; // Como hago yo esto?
            // AQUI TENEMOS UN PROBLEMA CON LOS NUEVOS EJEMPLOS CUBIERTOS

            if((!cubiertos.get(index toInt)) && (data.getClas == Variables.getNumClassObj)){
              matrices(i).ejAntClassNewCrisp += 1
            }
          } else {
            if(data.getClas == Variables.getNumClassObj){
              matrices(i).fn += 1
            } else {
              matrices(i).tn += 1
            }
          }
        }
      }
      val aux = new Array[Array[ConfusionMatrix]](1)
      aux(0) = matrices
      aux.iterator
    } ,true).reduce((x,y) =>{
      val ret = new Array[ConfusionMatrix](y.length)
      //trials += y.length
      for(i <- 0 until y.length) {
        val toRet: ConfusionMatrix = new ConfusionMatrix(neje)
        x(i).coveredExamples.foreach(value => indivsToEval(i).cubre.set(value toInt))
        y(i).coveredExamples.foreach(value => indivsToEval(i).cubre.set(value toInt))
        //toRet.coveredExamples.or(x(i).coveredExamples)
        //toRet.coveredExamples.or(y(i).coveredExamples)
        //toRet.coveredExamples = x(i).coveredExamples ++ y(i).coveredExamples
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
        ret(i) = toRet
      }
      // Return !
      ret
    })

 /*   val confusionMatrices = Examples.datosRDD.map(x => {
      val data = x._2
      val index = x._1
      // Calculates for those individuals not evaluated their confusion matrices and return
     indivsToEval.map(individual => {
        val cromosoma = individual.getIndivCromCAN
        var disparoCrisp = 1
        var numVarNoInterv = 0
       val matrix = new ConfusionMatrix
        // Calculates the disparoCrisp of each variable
        for(j <- 0 until Variables.getNVars){
          if(! Variables.getContinuous(j)){
            if(cromosoma.getCromElem(j) <= Variables.getMax(j)){
              if( (data.getDat(j) != cromosoma.getCromElem(j)) && (!data.getLost(Variables, 0, j))){
                disparoCrisp = 0
              }
            } else {
              matrix.numVarNoInterv += 1
            }
          } else {
            if(cromosoma.getCromElem(j) < Variables.getNLabelVar(j)){
              if(!data.getLost(Variables, 0, j)){
                if(individual.NumInterv(data.getDat(j), j, Variables) != cromosoma.getCromElem(j)){
                  disparoCrisp = 0
                }
              }
            } else {
              matrix.numVarNoInterv += 1
            }
          }
        } // End for all chromosome values

        if(disparoCrisp > 0){
          matrix.ejAntCrisp += 1
          matrix.coveredExamples += index
          if(data.getClas == Variables.getNumClassObj){
            matrix.ejAntClassCrisp += 1
            matrix.tp += 1
          } else {
            matrix.ejAntNoClassCrisp += 1
            matrix.fp += 1
          }
          // cubreClase[Examples.getClass(i)]++; // Como hago yo esto?
          // AQUI TENEMOS UN PROBLEMA CON LOS NUEVOS EJEMPLOS CUBIERTOS

          if((!cubiertos(index toInt)) && (data.getClas == Variables.getNumClassObj)){
            matrix.ejAntClassNewCrisp += 1
          }
        } else {
          if(data.getClas == Variables.getNumClassObj){
            matrix.fn += 1
          } else {
            matrix.tn += 1
          }
        }

        // Return!
        matrix
      })

      /**
        * NOW THE REDUCE PHASE: THIS GETS TWO ARRAYS OF CONFUSION MATRICES AND SUM THEIR VALUES
        * IT ADDS THE INDEX OF COVERED VALUES AND SUM AUXILIAR VARIABLES
        * THE RESULT IS AN UNIQUE CONFUSION MATRIX WITH ALL NECCESARY DATA TO CALCULATE THE
        * QUALITY MEASURES OF THE INDIVIDUAL
        */
    }).reduce((x,y) => {
      val ret = new Array[ConfusionMatrix](y.length)
      //trials += y.length
      for(i <- 0 until y.length) {
        val toRet: ConfusionMatrix = new ConfusionMatrix
        toRet.coveredExamples.appendAll(x(i).coveredExamples)
        toRet.coveredExamples.appendAll(y(i).coveredExamples)

        toRet.ejAntClassCrisp = x(i).ejAntClassCrisp + y(i).ejAntClassCrisp
        toRet.ejAntClassNewCrisp = x(i).ejAntClassNewCrisp + y(i).ejAntClassNewCrisp
        toRet.ejAntCrisp = x(i).ejAntCrisp + y(i).ejAntCrisp
        toRet.ejAntNoClassCrisp = x(i).ejAntNoClassCrisp + y(i).ejAntNoClassCrisp
        toRet.fn = x(i).fn + y(i).fn
        toRet.fp = x(i).fp + y(i).fp
        toRet.tn = x(i).tn + y(i).tn
        toRet.tp = x(i).tp + y(i).tp
        toRet.numVarNoInterv = x(i).numVarNoInterv + y(i).numVarNoInterv
        ret(i) = toRet
      }
      // Return !
      ret
    })*/

    // Now we have the complete confusion matrices of all individuals. Calculate their measures!!
    var j = -1
    indivi.filter(y => !y.evaluado).foreach(individual =>{
      j += 1
      val confMatrix = confusionMatrices(j)
      // Mark covered examples
      confMatrix.coveredExamples.foreach( x => {
        individual.cubre.set(x toInt)
      })
      //individual.cubre.or(confMatrix.coveredExamples)
      val numVarNoInterv: Int = confMatrix.numVarNoInterv / Examples.getNEx
      //COMPUTE THE MEASURES

      //LENGTH
      val leng: Float = if(confMatrix.ejAntClassCrisp != 0){
        1f / confMatrix.ejAntClassCrisp.toFloat
      } else {
        0f
      }

      if(numVarNoInterv >= Variables.getNVars){
        individual.medidas.setLength(0)
      } else {
        individual.medidas.setLength(leng)
      }

      //SENS
      val sens: Float = if (Examples.getExamplesClassObj != 0){
        confMatrix.ejAntClassCrisp.toFloat / Examples.getExamplesClassObj.toFloat
      }else{
        0
      }

      if (numVarNoInterv >= Variables.getNVars)
        individual.medidas.setSens(0)
      else
        individual.medidas.setSens(sens)


      //CONF
      val conf: Float = if (confMatrix.ejAntCrisp != 0){
        confMatrix.ejAntClassCrisp.toFloat / confMatrix.ejAntCrisp.toFloat
      } else {
        0
      }

      if (numVarNoInterv >= Variables.getNVars)
        individual.medidas.setConf(0)
      else
        individual.medidas.setConf(conf)

      //UNUS
      val coverage: Float = confMatrix.ejAntCrisp.toFloat / Examples.getNEx.toFloat
      val unus = if (confMatrix.ejAntCrisp == 0){
        0
      } else {
        coverage * (confMatrix.ejAntClassCrisp.toFloat / confMatrix.ejAntCrisp.toFloat - Examples.getExamplesClassObj.toFloat / Examples.getNEx.toFloat)
      }

      if (numVarNoInterv >= Variables.getNVars)
        individual.medidas.setUnus(0)
      else
        individual.medidas.setUnus(unus)

      //NSUP
      val nsup: Float = if ((Examples.getExamplesClassObj - Examples.getExamplesCoveredClass) != 0)
        confMatrix.ejAntClassNewCrisp.toFloat / (Examples.getExamplesClassObj - Examples.getExamplesCoveredClass).toFloat
      else
        0


      if (numVarNoInterv >= Variables.getNVars)
        individual.medidas.setNSup(0)
      else
        individual.medidas.setNSup(nsup)

      //SUPM
      val supM: Double = if (Examples.getNEx != 0) {
        confMatrix.ejAntNoClassCrisp.toDouble / Examples.getNEx.toDouble
      }else {
        0
      }

      if (numVarNoInterv >= Variables.getNVars)
        individual.medidas.setFPr(0)
      else
        individual.medidas.setFPr(supM)

      //SUPm
      val supm: Double = if (Examples.getNEx != 0) {
        confMatrix.ejAntClassCrisp.toDouble / Examples.getNEx.toDouble
      }else {
        0
      }

      if (numVarNoInterv >= Variables.getNVars)
          individual.medidas.setTPr(0)
      else
        individual.medidas.setTPr(supm)

      //DIFF SUPPORT
      val difs = supm - supM
      if (numVarNoInterv >= Variables.getNVars) {
        individual.medidas.setDifS(0)
      }else {
        individual.medidas.setDifS(difs)
      }
      //GAIN
      val gain = if ((confMatrix.ejAntCrisp==0)||(sens==0))
        sens * (0 - Math.log10(Examples.getExamplesClassObj.toFloat / Examples.getNEx.toFloat).toFloat)
      else {
        sens * (Math.log10(sens/coverage).toFloat - Math.log10(Examples.getExamplesClassObj.toFloat / Examples.getNEx.toFloat).toFloat)
      }
      if (numVarNoInterv >= Variables.getNVars)
        individual.medidas.setGain(0)
      else
        individual.medidas.setGain(gain)

      //TPr
      val tpr: Float = if (numVarNoInterv >= Variables.getNVars){
        0
      } else {
        if ((confMatrix.tp + confMatrix.fn) !=0 )
          confMatrix.tp.toFloat / (confMatrix.tp + confMatrix.fn).toFloat
        else
          0
      }

      //FPr
      val fpr: Float = if (numVarNoInterv >= Variables.getNVars){
        0
      } else {
        if ((confMatrix.fp + confMatrix.tn) != 0)
          confMatrix.fp.toFloat / (confMatrix.fp + confMatrix.tn).toFloat
        else
          0
      }

      //TNr
      val tnr = if (numVarNoInterv >= Variables.getNVars){
        0
      } else {
        if ((confMatrix.fp + confMatrix.tn) != 0)
          confMatrix.tn.toFloat / (confMatrix.fp + confMatrix.tn).toFloat
        else
          0
      }
      /*
      val supM = fpr
      individual.medidas.setFPr(supM)
      val supm = tpr
      individual.medidas.setTPr(supm)
      val difs = Math.abs(supm - supM)
      individual.medidas.setDifS(difs)
      */
      /*
      AUC
      if (numVarNoInterv >= Variables.getNVars()){
          auc = 0;
      } else auc = (1 + tpr - fpr) / 2;
      medidas.setAUC(auc);
      */
      //MedGeo
      val medgeo = if (numVarNoInterv >= Variables.getNVars){
        0
      } else {
        Math.sqrt(tpr*tnr).toFloat
      }
      individual.medidas.setMedGeo(medgeo)

      //GROWTH RATE

      /*val grat = if(tpr!=0 && fpr!=0)
        tpr/fpr
      else if(tpr!=0 && fpr==0)
        Float.PositiveInfinity
      else
        0 */
      val grat = if(supM != 0 && supm != 0){
        supm / supM
      } else if (supM == 0 && supm != 0){
        Float.PositiveInfinity
      } else {
        0
      }

      individual.medidas.setGRat(grat)

      // FISHER
      val fe: FisherExact = new FisherExact(Examples.getNEx)
      val fisher: Double = fe.getTwoTailedP(confMatrix.tp, confMatrix.fp, confMatrix.fn, confMatrix.tn)
      individual.medidas.setFisher(fisher)

      // Hellinger
      var parte1: Float = 0
      var parte2: Float = 0
      if (Examples.getExamplesClassObj != 0) {
        parte1 = Math.sqrt((confMatrix.ejAntClassCrisp.toFloat / Examples.getExamplesClassObj)).toFloat - Math.sqrt((confMatrix.fn.toFloat / Examples.getExamplesClassObj)).toFloat
      }

      if ((Examples.getNEx - Examples.getExamplesClassObj) != 0) {
        parte2 = Math.sqrt(confMatrix.ejAntNoClassCrisp.toFloat / (Examples.getNEx - Examples.getExamplesClassObj)).toFloat - Math.sqrt(confMatrix.tn.toFloat / (Examples.getNEx - Examples.getExamplesClassObj).toFloat).toFloat
      }
      val dh: Double = Math.sqrt(Math.pow(parte1, 2) + Math.pow(parte2, 2))
      individual.medidas.setHellinger(dh)


      //Introduce in fitness the correspondent value
      if (AG.getFitness.compareTo("NSUP")==0) individual.medidas.setFitness(nsup)
      if (AG.getFitness.compareTo("SENS")==0) individual.medidas.setFitness(sens)
      if (AG.getFitness.compareTo("SUPMA")==0) individual.medidas.setFitness(supM)
      if (AG.getFitness.compareTo("SUPMI")==0) individual.medidas.setFitness(supm)
      if (AG.getFitness.compareTo("UNUS")==0) individual.medidas.setFitness(unus)
      if (AG.getFitness.compareTo("CONF")==0) individual.medidas.setFitness(conf)
      if (AG.getFitness.compareTo("MEDGEO")==0) individual.medidas.setFitness(medgeo)
      if (AG.getFitness.compareTo("GRAT")==0) individual.medidas.setFitness(grat)
      if (AG.getFitness.compareTo("GAIN")==0) individual.medidas.setFitness(gain)
    })

    // The number of trials is the number of non-evaluated individuals
    val trials = indivi.count(y => {!y.evaluado})

    // Sets all individuals as evaluated
    var k = trials

    indivi.filter(!_.evaluado).foreach(y => {
      y.evaluado = true
      y.setNEval(AG.getTrials + trials - k)
      k = k - 1
    })

  /*var trials = 0
    indivi = indivi.map(x => {
      if(!x.evaluado){
        x.evalInd(AG, Variables, Examples)
        x.evaluado = true
        x.setNEval(AG.getTrials + trials)
        trials += 1

        // mark covered examples
        //              var i = -1
        //              x.cubre.foreach(y => {
        //                i += 1
        //                x.cubre(i) = y || ej_cubiertos(i)
        //              })
      }
      x
    })*/
    //          println("Cubiertos after pop eval:")
    //           Examples.cubiertos.foreach { println }
    //        System.exit(-1)

    // Return the number of trials


    trials
  }


  /**
    * <p>
    * Returns the indicated individual of the population
    * </p>
    * @param pos             Position of the individual
    * @return                Individual
    */
  def getIndiv (pos: Int): Individual = {
    indivi(pos)
  }

  /**
    * <p>
    * Return the number of individuals of the population
    * </p>
    * @return                Number of individuals of the population
    */
  def getNumIndiv: Int = {
    num_indiv
  }

  /**
    * <p>
    * Sets the number of individuals of the population
    * </p>
    */
  def setNumIndiv(nindiv: Int) = {
    num_indiv = nindiv
  }


  /**
    * <p>
    * Copy the population
    * </p>
    * @param poblacion      Population to copy
    * @param neje           Number of examples
    */
  def CopyPopulation (poblacion : Population, neje: Int) = {

    this.setNumIndiv(poblacion.getNumIndiv)

    //this.ej_cubiertos = poblacion.ej_cubiertos.map(x => x)

    var i = -1
    poblacion.indivi.foreach( x => {
      i += 1
      this.indivi(i).copyIndiv(x, neje)
    })

  }

  /**
    * <p>
    * Copy the individual in the Individual otro
    * </p>
    * @param pos             Position of the individual to copy
    * @param neje            Number of examples
    * @param a               Individual to copy
    */
  def CopyIndiv (pos: Int, neje: Int, a: Individual) = {
    this.indivi(pos).copyIndiv(a, neje)
  }


  /**
    * <p>
    * Returns the indicated gene of the Chromosome
    * </p>
    * @param num_indiv               Position of the individual
    * @param pos                     Position of the variable
    * @param elem                    Position of the gene of the variable
    * @param RulRep                Rules representation
    * @return                        Gene of the chromosome
    */
  def getCromElem (num_indiv: Int, pos: Int, elem: Int, RulRep: String): Int = {
    this.indivi(num_indiv).getCromElem(pos)
  }


  /**
    * <p>
    * Sets the value of the indicated gene of the Chromosome
    * </p>
    * @param num_indiv               Position of the individual
    * @param pos                     Position of the variable
    * @param elem                    Position of the gene of the variable
    * @param value                     Value for the gene
    * @param RulRep                Rules representation
    */
  def setCromElem (num_indiv: Int, pos: Int, elem: Int, value: Int, RulRep: String) = {

    this.indivi(num_indiv).setCromElem(pos, value)
  }


  /**
    * <p>
    * Returns if the individual of the population has been evaluated
    * </p>
    * @param num_indiv               Position of the individual
    */
  def getIndivEvaluated (num_indiv: Int): Boolean = {
    this.indivi(num_indiv).getIndivEvaluated
  }


  /**
    * <p>
    * Sets the value for de evaluated attribute of the individual
    * </p>
    * @param num_indiv           Position of the individual
    * @param value                 Value of the individual
    */
  def setIndivEvaluated (num_indiv: Int, value: Boolean) = {
    this.indivi(num_indiv).setIndivEvaluated (value)
  }


  /**
    * <p>
    * Returns de hole cromosoma of the selected individual
    * </p>
    * @param num_indiv           Position of the individual
    * @return                    Canonical chromosome
    */
  def getIndivCromCAN (num_indiv: Int): CromCAN = {
    this.indivi(num_indiv).getIndivCromCAN
  }


  /**
    * <p>
    * Prints population individuals
    * </p>
    * @param nFile           File to write the population
    */
  def Print(nFile: String) = {
    this.indivi.foreach( x => {
      x.Print(nFile)
    })
  }

}
