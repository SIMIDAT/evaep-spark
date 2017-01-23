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
import java.util

import org.apache.ivy.plugins.parser.m2.PomModuleDescriptorBuilder.ConfMapper
import utils.{ConfusionMatrix, QualityMeasures, Randomize}

class IndCAN extends Individual with Serializable{

  /**
    * <p>
    * Defines the individual of the population
    * </p>
    */

   var cromosoma: CromCAN = null   // Individual contents

  /**
    * </p>
    * Creates new instance of Canonical individual
    * </p>
    * @param lenght          Lenght of the individual
    * @param neje              Number of examples
    */
  def this(lenght: Int, neje: Int, trials: Int) = {
    this()
    this.tamano = lenght;
    this.cromosoma = new CromCAN(lenght);
    this.medidas = new QualityMeasures();

    this.evaluado = false;
    this.cubre = new Array[Boolean](neje);

    this.n_eval = trials;

  }


  /**
    * <p>
    * Creates random instance of Canonical individual
    * </p>
    * @param Variables             Variables structure
    * @param porcVar               Percentage of variables initialised in the population
    * @param neje                  Number of exaples
    * @param nFile                 File to write the individual
    */
  def RndInitInd(Variables: TableVar, porcVar: Float, neje: Int, nFile: String) = {
    cromosoma.RndInitCrom(Variables, porcVar);  // Random initialization method
    evaluado = false;                           // Individual not evaluated
    var i = -1;
    this.cubre.foreach(x  => {
      i += 1
      this.cubre(i) = false
    })
    n_eval = 0;
  }



  /**
    * <p>
    * Creates instance of Canonical individual based on coverage
    * </p>
    * @param Variables     Variables structure
    * @param Examples      Examples structure
    * @param porcVar       Percentage of variables to form the individual
    * @param nFile         File to write the individual
    */
  def BsdInitInd(Variables: TableVar, Examples: TableDat, porcVar: Float, nFile: String): Individual = {

    cromosoma.BsdInitCrom(Variables, Examples, porcVar)

    evaluado = false

    var i = -1
    this.cubre = this.cubre.map(x => false)

    n_eval = 0
    this
  }


  /**
    * <p>
    * Returns the Chromosome
    * </p>
    * @return              Chromosome
    */
  def getIndivCrom: CromCAN = {
    cromosoma;
  }


  /**
    * <p>
    * Returns the indicated gene of the Chromosome
    * </p>
    * @param pos               Position of the gene
    * @return                  Value of the gene
    */
  def getCromElem (pos: Int): Int = {
    cromosoma.getCromElem (pos)
  }


  /**
    * <p>
    * Returns the value of the indicated gene for the variable
    * </p>
    * @param pos               Position of the variable
    * @param elem              Position of the gene
    * @return                  Value of the gene
    */
  def getCromGeneElem(pos: Int, elem: Int): Boolean = {
    false
  }


  /**
    * <p>
    * Sets the value of the indicated gene of the Chromosome
    * </p>
    * @param pos               Position of the variable
    * @param value               Value of the variable
    */
  def setCromElem (pos: Int, value: Int) = {
    cromosoma.setCromElem(pos, value)
  }

  /**
    * <p>
    * Sets the value of the indicated gene of the Chromosome
    * </p>
    * @param pos               Position of the variable
    * @param elem              Position of the gene
    * @param value               Value of the variable
    */
  def setCromGeneElem (pos: Int, elem: Int, value: Boolean){}


  /**
    * <p>
    * Returns the indicated Chromosome
    * </p>
    * @return                  The canonical Chromosome
    */
  def getIndivCromCAN: CromCAN = {
    cromosoma;
  }


  /**
    * <p>
    * Copy the indicaded individual in "this" individual
    * </p>
    * @param a              The individual to Copy
    * @param neje              Number of examples
    */
  def copyIndiv (a: Individual, neje: Int) = {

    for (i <- 0 until tamano){
      this.setCromElem(i, a.getCromElem(i))
    }

    //this.setIndivEvaluated(a.getIndivEvaluated)
    this.evaluado = a.evaluado


    for (i <- 0 until neje){
      this.cubre(i) = a.cubre(i)
    }

    this.setNEval(a.getNEval)

    this.medidas.Copy(a.getMeasures)

  }


  /**
    * <p>
    * Evaluates an individual. This function evaluates an individual.
    * </p>
    * @param AG                Genetic algorithm
    * @param Variables         Variables structure
    * @param Examples          Examples structure
    */

  def evalInd (AG: Genetic, Variables: TableVar, Examples: TableDat) = {

    //        int ejAntFuzzy=0;                // Number of compatible examples with the antecedent of any class - fuzzy version --- unused
    //        int ejAntClassFuzzy=0;           // Number of compatible examples (antecedent and class) - fuzzy version
    //        int ejAntNoClassFuzzy=0;           // Number of compatible examples (antecedent and class) - fuzzy version
    //        int ejAntClassNewFuzzy=0;        // Number of new covered compatible examples (antec and class) - fuzzy version
    //        float gradoAntFuzzy=0;           // Total compatibility degree with the antecedent - fuzzy version
    //        float gradoAntClassFuzzy=0;      // Tot compatibility degree with antecedent and class - fuzzy version
    //        float gradoAntClassNewEjFuzzy=0; // Tot compatibility degree with antecedent and class of new covered examples - fuzzy version
    //        float disparoFuzzy;    // Final compatibility degree of the example with the individual - fuzzy version

    var ejAntCrisp: Int = 0;                // Number of compatible examples with the antecedent of any class - crisp version
    var ejAntClassCrisp: Int = 0;           // Number of compatible examples (antecedent and class) - crisp version
    var ejAntNoClassCrisp: Int = 0;         // Number of compatible examples (antecedent and NOT class) - crisp version
    var ejAntClassNewCrisp: Int = 0;        // Number of new covered compatible examples (antec and class) - crisp version
    var disparoCrisp: Float = 1;    // Final compatibility or not of the example with the individual - crisp version

    var tp: Float = 0;
    var tn: Float = 0;
    var fp: Float = 0;
    var fn: Float = 0;
    //        var tpr: Float = 0;
    //        var fpr: Float = 0;
    //        var tnr: Float = 0;


    //float leng, supM, supm, unus, gain, difs, sens, nsup, conf, medgeo;

    var ejClase: Array[Int] = new Array[Int](Variables.getNClass)
    var cubreClase: Array[Int] = new Array[Int](Variables.getNClass)
    for (i <- 0 until Variables.getNClass) {
      cubreClase(i)=0;
      ejClase(i) = Examples.getExamplesClass (i);
    }

    //int numVarNoInterv=0;  // Number of variables not taking part in the individual



    /**
      *  EVALUATES THE INDIVUAL AGAINST ALL THE EXAMPLES
      *   WITH MAP-REDUCE :
      *
      *   THIS IS THE MAP PHASE. HERE WE CALCULATE FOR EACH EXAMPLE ITS CONFUSION MATRIX
      *   i.e., IF THE EXAMPLE IS A TP, TN, FP OR FN
      *   AND ADDITIONAL MEASURES LIKE WHETHER IS A NEW COVERED EXAMPLE OR NOT
      */
    val confMatrix: ConfusionMatrix = Examples.datosRDD.map(x => {
      val data = x._2
      val index = x._1
      var numVarNoInterv: Int = 0;
      var disparoCrisp: Int = 1;
      val matrix = new ConfusionMatrix

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
              if(NumInterv(data.getDat(j), j, Variables) != cromosoma.getCromElem(j)){
                disparoCrisp = 0;
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

        if((!Examples.cubiertos(index toInt)) && (data.getClas == Variables.getNumClassObj)){
          matrix.ejAntClassNewCrisp += 1;
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

      /**
        * NOW THE REDUCE PHASE: THIS GETS TWO CONFUSION MATRICES AND SUM THEIR VALUES
        * IT ADDS THE INDEX OF COVERED VALUES AND SUM AUXILIAR VARIABLES
        * THE RESULT IS AN UNIQUE CONFUSION MATRIX WITH ALL NECCESARY DATA TO CALCULATE THE
        * QUALITY MEASURES OF THE INDIVIDUAL
        */
    }).reduce((x,y) => {
      val toRet: ConfusionMatrix = new ConfusionMatrix
      toRet.coveredExamples.appendAll(x.coveredExamples)
      toRet.coveredExamples.appendAll(y.coveredExamples)

      toRet.ejAntClassCrisp = x.ejAntClassCrisp + y.ejAntClassCrisp
      toRet.ejAntClassNewCrisp = x.ejAntClassNewCrisp + y.ejAntClassNewCrisp
      toRet.ejAntCrisp = x.ejAntCrisp + y.ejAntCrisp
      toRet.ejAntNoClassCrisp = x.ejAntNoClassCrisp + y.ejAntNoClassCrisp
      toRet.fn = x.fn + y.fn
      toRet.fp = x.fp + y.fp
      toRet.tn = x.tn + y.tn
      toRet.tp = x.tp + y.tp
      toRet.numVarNoInterv = x.numVarNoInterv + y.numVarNoInterv // No tiene sentido.
      // Return !
      toRet
    })

    // Mark covered examples
    confMatrix.coveredExamples.foreach( x => {
      cubre(x toInt) = true
    })



    //println("tp: " + confMatrix.tp + "  tn: " + confMatrix.tn + " fp: " + confMatrix.fp + " fn: " + confMatrix.fn + " CovNEW: " + confMatrix.ejAntClassNewCrisp)

    val numVarNoInterv: Int = confMatrix.numVarNoInterv / Examples.getNEx

    //COMPUTE THE MEASURES
    //LENGTH
    val leng: Float = if(confMatrix.ejAntClassCrisp != 0){
      1f / confMatrix.ejAntClassCrisp.toFloat
    } else {
      0f
    }

    if(numVarNoInterv >= Variables.getNVars){
      medidas.setLength(0)
    } else {
      medidas.setLength(leng)
    }

    //SENS
    val sens: Float = if (Examples.getExamplesClassObj != 0){
      confMatrix.ejAntClassCrisp.toFloat / Examples.getExamplesClassObj.toFloat;
    }else{
      0
    }

    if (numVarNoInterv >= Variables.getNVars)
      medidas.setSens(0);
    else
      medidas.setSens(sens);


    //CONF
    val conf: Float = if (confMatrix.ejAntCrisp != 0){
      confMatrix.ejAntClassCrisp.toFloat / confMatrix.ejAntCrisp.toFloat;
    } else {
      0
    }

    if (numVarNoInterv >= Variables.getNVars)
      medidas.setConf(0)
    else
      medidas.setConf(conf)

    //UNUS
    val coverage: Float = confMatrix.ejAntCrisp.toFloat / Examples.getNEx.toFloat;
    var unus = if (confMatrix.ejAntCrisp == 0){
      0
    } else {
      coverage * (confMatrix.ejAntClassCrisp.toFloat / confMatrix.ejAntCrisp.toFloat - Examples.getExamplesClassObj.toFloat / Examples.getNEx.toFloat)
    }
    // Normalise unus
    val minUnus:Float = (Examples.getExamplesClassObj.toFloat / Examples.getNEx.toFloat) * (0 - (Examples.getExamplesClassObj.toFloat / Examples.getNEx.toFloat))
    val maxUnus:Float = (Examples.getExamplesClassObj.toFloat / Examples.getNEx.toFloat) * (1-(Examples.getExamplesClassObj.toFloat / Examples.getNEx.toFloat))
    unus = (unus - minUnus) / (maxUnus - minUnus)

    if (numVarNoInterv >= Variables.getNVars)
      medidas.setUnus(0)
    else
      medidas.setUnus(unus)

    //NSUP
    val nsup: Float = if ((Examples.getExamplesClassObj - Examples.getExamplesCoveredClass) != 0)
      confMatrix.ejAntClassNewCrisp.toFloat / (Examples.getExamplesClassObj - Examples.getExamplesCoveredClass).toFloat
    else
      0


    if (numVarNoInterv >= Variables.getNVars)
      medidas.setNSup(0)
    else
      medidas.setNSup(nsup)

    //GAIN
    val gain = if ((confMatrix.ejAntCrisp==0)||(sens==0))
      sens * (0 - (Math.log10(Examples.getExamplesClassObj.toFloat / Examples.getNEx.toFloat).toFloat));
    else {
      sens * ((Math.log10(sens/coverage).toFloat) - (Math.log10(Examples.getExamplesClassObj.toFloat / Examples.getNEx.toFloat).toFloat));
    }
    if (numVarNoInterv >= Variables.getNVars)
      medidas.setGain(0);
    else
      medidas.setGain(gain);

    //TPr
    val tpr: Float = if (numVarNoInterv >= Variables.getNVars){
      0
    } else {
      if ((confMatrix.tp + confMatrix.fn) !=0 )
        confMatrix.tp.toFloat / (confMatrix.tp + confMatrix.fn).toFloat;
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

    val supM = fpr;
    medidas.setFPr(supM);
    val supm = tpr;
    medidas.setTPr(supm);
    val difs = Math.abs(supm - supM);
    medidas.setDifS(difs);

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
    medidas.setMedGeo(medgeo);

    //GROWTH RATE

    val grat = if(tpr!=0 && fpr!=0)
      tpr/fpr
    else if(tpr!=0 && fpr==0)
      Float.PositiveInfinity
    else
      0

    medidas.setGRat(grat);

    //Introduce in fitness the correspondent value
    if (AG.getFitness.compareTo("NSUP")==0) medidas.setFitness(nsup);
    if (AG.getFitness.compareTo("SENS")==0) medidas.setFitness(sens);
    if (AG.getFitness.compareTo("SUPMA")==0) medidas.setFitness(supM);
    if (AG.getFitness.compareTo("SUPMI")==0) medidas.setFitness(supm);
    if (AG.getFitness.compareTo("UNUS")==0) medidas.setFitness(unus);
    if (AG.getFitness.compareTo("CONF")==0) medidas.setFitness(conf);
    if (AG.getFitness.compareTo("MEDGEO")==0) medidas.setFitness(medgeo);
    if (AG.getFitness.compareTo("GRAT")==0) medidas.setFitness(grat);
    if (AG.getFitness.compareTo("GAIN")==0) medidas.setFitness(gain);

    // Set the individual as evaluated
    evaluado = true;

  }


  /**
    * <p>
    * Returns the number of the interval of the indicated variable to which belongs
    * the value. It is performed seeking the greater belonging degree of the
    * value to the fuzzy sets defined for the variable
    * </p>
    * @param value                 Value to calculate
    * @param num_var               Number of the variable
    * @param Variables             Variables structure
    * @return                      Number of the interval
    */
  def NumInterv (value: Float, num_var: Int, Variables: TableVar): Int = {
    var pertenencia: Float = 0
    var interv: Int = -1;

    for (i <- 0 until Variables.getNLabelVar(num_var)) {
      val new_pert = Variables.Fuzzy(num_var, i, value);
      if (new_pert>pertenencia) {
        interv = i;
        pertenencia = new_pert;
      }
    }
    return interv;

  }



  def getClassIndividual (Variables: TableVar, Examples: TableDat, eje: Int): Float = {
  1f
    //float disparo = 1;
 /*
    for (j <- 0 until Variables.getNVars) {
      if (!Variables.getContinuous(j)) {  // Discrete Variable
        if (cromosoma.getCromElem(j)<=Variables.getMax(j)){
          // Variable j takes part in the rule
          if ((Examples.getDat(eje,j) != cromosoma.getCromElem(j)) && (!Examples.getLost(Variables,eje,j))) {
            // If chromosome value <> example value, and example value is not a lost value
            return 0;
          }
        }
      } else {	// Continuous variable
        if (cromosoma.getCromElem(j)<Variables.getNLabelVar(j)) {
          // Variable takes part in the rule
          // Crisp computation
          if (!Examples.getLost(Variables,eje,j)) {
            if (NumInterv (Examples.getDat(eje,j),j, Variables)!= cromosoma.getCromElem(j))
              return 0;
          }
          // Fuzzy computation
          //                    if (!Examples.getLost(Variables,eje,j)) {
          //                        // If the value is not a lost value
          //                        float pertenencia = Variables.Fuzzy(j, cromosoma.getCromElem(j), Examples.getDat(eje,j));
          //                        disparo = Utils.Minimum (disparo, pertenencia);
          //                    }
        }
      }
    } // End FOR all chromosome values

    return 1;
 */
  }


  /**
    * <p>
    * Method to Print the contents of the individual
    * </p>
    * @param nFile             File to write the individual
    */
  def Print(nFile: String) = {
    cromosoma.Print(nFile)

    val contents: String = "Evaluated - ".concat(evaluado  toString).concat("\n") concat "Evaluation generated " concat n_eval.toString concat "\n" concat "Fitness: " concat getMeasures.getFitness.toString concat "\n" concat "Growth Rate: " concat getMeasures.getGRat.toString concat "\n"

    if (nFile=="")
      println (contents)
    /*else
       File.AddtoFile(nFile, contents);*/
  }


  //override var tamano: Int = _
  //override var evaluado: Boolean = _
  override var cubre: Array[Boolean] = _
  override var cubr: Float = _
  override var n_eval: Int = _
  override var medidas: QualityMeasures = _

  /*
    override var tamano: Int = _
    override var rank: Int = _
    override var cubr: Float = _
    override var crowdingDistance: Double = _
    override var evaluado: Boolean = _
    override var medidas: QualityMeasures = _
    override var n_eval: Int = _
    override var overallConstraintViolation: Double = _
    override var numberOfViolatedConstraints: Int = _
    override var cubre: Array[Boolean] = _
    override var cubiertos: util.ArrayList[Int] = new util.ArrayList[Int]()
    override var unusualness: Float = _
    override var sensitivity: Float = _
    override var coverage: Float = _
    override var confidence: Float = _
    override var var1: Float = _
    override var var2: Float = _
    override var var3: Float = _
    override var var4: Float = _*/
}
