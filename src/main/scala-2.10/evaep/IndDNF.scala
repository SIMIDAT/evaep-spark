package evaep
import java.util

import utils.{ConfusionMatrix, QualityMeasures}
import java.util.BitSet

/**
  * <p>
  *
  * @author Written by Cristobal J. Carmona (University of Jaen) 11/08/2008
  * @version 1.0
  * @since JDK1.5
  *        </p>
  */


class IndDNF extends Individual with Serializable {
  /**
    * <p>
    * Defines the DNF individual of the population
    * </p>
    */
  var cromosoma: CromDNF = null // Individual contents
  override var cubre: java.util.BitSet = _
  override var cubr: Float = _
  override var n_eval: Int = _
  override var medidas: QualityMeasures = _

  /**
    * <p>
    * Creates new instance of Individual
    * </p>
    *
    * @param lenght    Lenght of the individual
    * @param neje      Number of examples
    * @param Variables Variables structure
    */
  def this(lenght: Int, neje: Int, Variables: TableVar, trials: Int) {
    this()
    tamano = lenght
    cromosoma = new CromDNF(lenght, Variables)
    evaluado = false
    cubre = new BitSet(neje)
    n_eval = trials
    medidas = new QualityMeasures
  }

  /**
    * <p>
    * Creates rangom instance of DNF individual
    * </p>
    *
    * @param Variables Variables structure
    * @param porcVar   Percentage of variables initialised in the population
    * @param neje      Number of exaples
    * @param nFile     File to write the individual
    */
  override def RndInitInd(Variables: TableVar, porcVar: Float, neje: Int, nFile: String): Unit = {
    cromosoma.RndInitCrom(Variables, porcVar) // Random initialization method

    evaluado = false // Individual not evaluated
    cubre.clear(0,neje)

    n_eval = 0
  }

  /**
    * <p>
    * Creates nstance of DNF individual based on coverage
    * </p>
    *
    * @param Variables Variables structure
    * @param Examples  Examples structure
    * @param porcVar   Percentage of variables to form the individual
    * @param nFile     File to write the individual
    */
  override def BsdInitInd(Variables: TableVar, Examples: TableDat, porcVar: Float, nFile: String): Individual = {
    cromosoma.BsdInitCrom(Variables, Examples, porcVar)
    evaluado = false
    cubre.clear(0,Examples.getNEx)
    n_eval = 0

    this
  }

  /**
    * <p>
    * Returns the Chromosome
    * </p>
    *
    * @return Chromosome
    */
  def getIndivCrom: CromDNF = cromosoma

  /**
    * <p>
    * Returns the indicated gene of the Chromosome
    * </p>
    *
    * @param pos  Position of the variable
    * @param elem Position of the gene
    * @return Value of the gene
    */
  override def getCromGeneElem(pos: Int, elem: Int): Boolean = cromosoma.getCromGeneElem(pos, elem)

  /**
    * <p>
    * Returns the indicated gene of the Chromosome
    * </p>
    *
    * @param pos Position of the gene
    * @return Value of the gene
    */
  override def getCromElem(pos: Int) = 0

  /**
    * <p>
    * Sets the value of the indicated gene of the Chromosome
    * </p>
    *
    * @param pos  Position of the variable
    * @param elem Position of the gene
    * @param val  Value of the variable
    */
  override def setCromGeneElem(pos: Int, elem: Int, `val`: Boolean): Unit = {
    cromosoma.setCromGeneElem(pos, elem, `val`)
  }

  /**
    * <p>
    * Sets the value of the indicated gene of the Chromosome
    * </p>
    *
    * @param pos Position of the variable
    * @param val Value of the variable
    */
  override def setCromElem(pos: Int, `val`: Int): Unit = {
  }

  /**
    * <p>
    * Returns the indicated Chromosome
    * </p>
    *
    * @return The DNF Chromosome
    */
  override def getIndivCromDNF: CromDNF = cromosoma

  /**
    * <p>
    * Returns the indicated Chromosome
    * </p>
    *
    * @return The canonical Chromosome
    */
  override def getIndivCromCAN = null

  /**
    * <p>
    * Copy the indicaded individual in "this" individual
    * </p>
    *
    * @param a    The individual to Copy
    * @param neje Number of examples
    */
  override def copyIndiv(a: Individual, neje: Int): Unit = {
    var number = 0
   for(i <- 0 until tamano){
      number = a.getIndivCromDNF.getCromGeneLenght(i)
      for(j <- 0 to number) {
        this.setCromGeneElem(i, j, a.getCromGeneElem(i, j))
      }
    }
    this.setIndivEvaluated(a.getIndivEvaluated)
    this.cubre.clear(0,neje)
    cubre.or(a.cubre)

    this.setNEval(a.getNEval)
    this.medidas.Copy(a.getMeasures)
  }

  /**
    * <p>
    * Evaluate a individual. This function evaluates an individual.
    * </p>
    *
    * @param AG        Genetic algorithm
    * @param Variables Variables structure
    * @param Examples  Examples structure
    */
  override def evalInd(AG: Genetic, Variables: TableVar, Examples: TableDat): Unit = {
//    var ejAntCrisp = 0          // Number of compatible examples with the antecedent of any class - crisp version
//    var ejAntClassCrisp = 0     // Number of compatible examples (antecedent and class) - crisp version
//    var ejAntNoClassCrisp = 0   // Number of compatible examples (antecedent and NOT class) - crisp version
//    var ejAntClassNewCrisp = 0  // Number of new covered compatible examples (antec and class) - crisp version
//    var disparoCrisp = 1        // Final compatibility or not of the example with the individual - crisp version
//    var tp = 0
//    var tn = 0
//    var fp = 0
//    var fn = 0
//    var tpr = 0
//    var fpr = 0
//    var tnr = 0
//    var leng = .0
//    var supM = .0
//    var supm = .0
//    var unus = .0
//    var gain = .0
//    var difs = .0
//    var sens = .0
//    var nsup = .0
//    var conf = .0
//    var medgeo = .0
//    val ejClase = new Array[Int](Variables.getNClass)
//    val cubreClase = new Array[Int](Variables.getNClass)
//    var i = 0
//
//    for(i <- 0 until Variables.getNClass){
//     cubreClase(i) = 0
//      ejClase(i) = Examples.getExamplesClass(i)
//    }
//
//    //int por_cubrir;        // Number of examples of the class not covered yet - for fuzzy version
//    var numVarNoInterv = 0
//    // Number of variables not taking part in the individual
//    var i = 0
//    while ( {
//      i < Examples.getNEx
//    }) { // For each example of the dataset
//      // Initialisation
//      //disparoFuzzy = 1;
//      disparoCrisp = 1
//      numVarNoInterv = 0
//      // Compute all chromosome values
//      var j = 0
//      while ( {
//        j < Variables.getNVars
//      }) {
//        if (!Variables.getContinuous(j)) { // Discrete Variable
//          if (cromosoma.getCromGeneElem(j, Variables.getNLabelVar(j)) == true) { // Variable j does not take part in the rule
//            if ((cromosoma.getCromGeneElem(j, Examples.getDat(i, j).asInstanceOf[Int]) == false) && (!Examples.getLost(Variables, i, j))) { //disparoFuzzy = 0;
//              disparoCrisp = 0
//            }
//          }
//          else numVarNoInterv += 1
//        }
//        else { // Continuous variable
//          if (cromosoma.getCromGeneElem(j, Variables.getNLabelVar(j)) == true) { // Variable takes part in the rule
//            // Fuzzy computation
//            //                        if (!Examples.getLost(Variables,i,j)) {
//            //                            float pertenencia = 0;
//            //                            float pert;
//            //                            for (int k=0; k<Variables.getNLabelVar(j); k++) {
//            //                                if (cromosoma.getCromGeneElem(j,k)==true)
//            //                                    pert = Variables.Fuzzy (j, k, Examples.getDat(i,j));
//            //                                else pert = 0;
//            //                                pertenencia = Utils.Maximum (pertenencia, pert);
//            //                            }
//            //                            disparoFuzzy = Utils.Minimum (disparoFuzzy, pertenencia);
//            //                        }
//            // Crisp computation
//            if (!Examples.getLost(Variables, i, j)) {
//              if (cromosoma.getCromGeneElem(j, NumInterv(Examples.getDat(i, j), j, Variables)) == false) disparoCrisp = 0
//              // If chromosome value <> example value, and example value != lost value (lost value are COMPATIBLES */
//            }
//          }
//          else numVarNoInterv += 1 // Variable does not take part
//        }
//        // End FOR all chromosome values
//        {
//          j += 1; j - 1
//        }
//      }
//      // Update globals counters
//      //            gradoCompAntFuzzy += disparoFuzzy;
//      //            if (disparoFuzzy>0) {
//      //            	ejCompAntFuzzy++;
//      //                if (Examples.getClass(i) == Variables.getNumClassObj()) {
//      //                    gradoCompAntClassFuzzy +=disparoFuzzy;
//      //                    ejCompAntClassFuzzy ++;
//      //                }
//      //                if ((!Examples.getCovered(i)) &&  (Examples.getClass(i) == Variables.getNumClassObj())) {
//      //                    ejCompAntClassNewFuzzy++;
//      //                    gradoCompAntClassNewEjFuzzy += disparoFuzzy;
//      //                    cubre[i]=true;
//      //                    Examples.setCovered(i, true);
//      //                }
//      //                //Calculate the AUC of the rule
//      //                if (Examples.getClass(i)==Variables.getNumClassObj()){
//      //                    tp++;
//      //                } else fp++;
//      //            }
//      if (disparoCrisp > 0) {
//        ejAntCrisp += 1
//        cubre(i) = true
//        if (Examples.getClass(i) == Variables.getNumClassObj) { //                    gradoAntClassFuzzy +=disparoFuzzy;
//          ejAntClassCrisp += 1
//          tp += 1
//        }
//        else {
//          ejAntNoClassCrisp += 1
//          fp += 1
//        }
//        cubreClase(Examples.getClass(i)) += 1
//        if ((!Examples.getCovered(i)) && (Examples.getClass(i) == Variables.getNumClassObj)) { // If example was not previusly covered and belongs to the target class increments the number of covered examples
//          ejAntClassNewCrisp += 1
//          //                    gradoAntClassNewEjFuzzy += disparoFuzzy;
//        }
//      }
//      else {
//        cubre(i) = false
//        if (Examples.getClass(i) == Variables.getNumClassObj) fn += 1
//        else tn += 1
//      }
//      // End of cycle for each example
//      {
//        i += 1; i - 1
//      }
//    }
//    // Compute the measures
//    //LENGTH
//    if (ejAntClassCrisp != 0) leng = 1.toFloat / ejAntClassCrisp
//    else leng = 0
//    if (numVarNoInterv >= Variables.getNVars) medidas.setLength(0)
//    else medidas.setLength(leng)
//    //SENS
//    if (Examples.getExamplesClassObj != 0) sens = ejAntClassCrisp.toFloat / Examples.getExamplesClassObj
//    else sens = 0
//    if (numVarNoInterv >= Variables.getNVars) medidas.setSens(0)
//    else medidas.setSens(sens)
//    //CONF
//    if (ejAntCrisp != 0) conf = ejAntClassCrisp.toFloat / ejAntCrisp
//    else conf = 0
//    if (numVarNoInterv >= Variables.getNVars) medidas.setConf(0)
//    else medidas.setConf(conf)
//    //UNUS
//    val coverage = ejAntCrisp.toFloat / Examples.getNEx
//    if (ejAntCrisp == 0) unus = 0
//    else unus = coverage * (ejAntClassCrisp.toFloat / ejAntCrisp - Examples.getExamplesClassObj.toFloat / Examples.getNEx)
//    if (numVarNoInterv >= Variables.getNVars) medidas.setUnus(0)
//    else medidas.setUnus(unus)
//    val classPCT = Examples.getExamplesClass(Variables.getNumClassObj).toFloat / Examples.getNEx.toFloat
//    val maxWRACC = classPCT * (1 - classPCT)
//    val minWRACC = (1 - classPCT) * (0 - classPCT)
//    if (maxWRACC - minWRACC != 0) unus = (unus - minWRACC) / (maxWRACC - minWRACC)
//    else unus = 0
//    medidas.setUnus(unus)
//    //NSUP
//    if (Examples.getExamplesClassObj - Examples.getExamplesCoveredClass != 0) nsup = (ejAntClassNewCrisp).toFloat / (Examples.getExamplesClassObj - Examples.getExamplesCoveredClass)
//    else nsup = 0
//    if (numVarNoInterv >= Variables.getNVars) medidas.setNSup(0)
//    else medidas.setNSup(nsup)
//    //SUPM
//    if (Examples.getNEx != 0) supM = ejAntNoClassCrisp.toFloat / Examples.getNEx
//    else supM = 0
//    if (numVarNoInterv >= Variables.getNVars) medidas.setSupM(0)
//    else medidas.setSupM(supM)
//    //SUPm
//    if (Examples.getNEx != 0) supm = ejAntClassCrisp.toFloat / Examples.getNEx
//    else supm = 0
//    if (numVarNoInterv >= Variables.getNVars) medidas.setSupm(0)
//    else medidas.setSupm(supm)
//    //DIFF SUPPORT
//    difs = supm - supM
//    if (numVarNoInterv >= Variables.getNVars) medidas.setDifS(0)
//    else medidas.setDifS(difs)
//    //GAIN
//    if ((ejAntCrisp == 0) || (sens == 0)) gain = sens * (0 - Math.log10(Examples.getExamplesClassObj.toFloat / Examples.getNEx).toFloat)
//    else gain = sens * (Math.log10(sens / coverage).toFloat - Math.log10(Examples.getExamplesClassObj.toFloat / Examples.getNEx).toFloat)
//    if (numVarNoInterv >= Variables.getNVars) medidas.setGain(0)
//    else medidas.setGain(gain)
//    //TPr
//    if (numVarNoInterv >= Variables.getNVars) tpr = 0
//    else if ((tp + fn) != 0) tpr = tp / (tp + fn)
//    else tpr = 0
//    //FPr
//    if (numVarNoInterv >= Variables.getNVars) fpr = 0
//    else if ((fp + tn) != 0) fpr = fp / (fp + tn)
//    else fpr = 0
//    //TNr
//    if (numVarNoInterv >= Variables.getNVars) tnr = 0
//    else if ((fp + tn) != 0) tnr = tn / (fp + tn)
//    else tnr = 0
//    //MedGeo
//    if (numVarNoInterv >= Variables.getNVars) medgeo = 0
//    else medgeo = Math.sqrt(tpr * tnr).toFloat
//    medidas.setMedGeo(medgeo)
//    //GRAT
//    var grat = .0
//    if (supM != 0 && supm != 0) grat = supm / supM
//    else if (supM == 0 && supm != 0) grat = Float.POSITIVE_INFINITY
//    else grat = 0
//    medidas.setGRat(grat)
//    //Introduce in fitness the correspondent value
//    if (AG.getFitness.compareTo("NSUP") == 0) medidas.setFitness(nsup)
//    if (AG.getFitness.compareTo("SENS") == 0) medidas.setFitness(sens)
//    if (AG.getFitness.compareTo("SUPMA") == 0) medidas.setFitness(supM)
//    if (AG.getFitness.compareTo("SUPMI") == 0) medidas.setFitness(supm)
//    if (AG.getFitness.compareTo("UNUS") == 0) medidas.setFitness(unus)
//    if (AG.getFitness.compareTo("CONF") == 0) medidas.setFitness(conf)
//    if (AG.getFitness.compareTo("MEDGEO") == 0) medidas.setFitness(medgeo)
//    if (AG.getFitness.compareTo("GRAT") == 0) medidas.setFitness(grat)
//    if (AG.getFitness.compareTo("GAIN") == 0) medidas.setFitness(gain)
//    // Set the individual as evaluated
//    evaluado = true
  }

  /**
    * <p>
    * Returns the number of the interval of the indicated variable to which
    * belongs the value. It is performed seeking the greater belonging degree
    * of the value to the fuzzy sets defined for the variable
    * </p>
    *
    * @param valor     Value to calculate
    * @param num_var   Number of the variable
    * @param Variables Variables structure
    * @return Number of the interval
    */
  override def NumInterv(valor: Float, num_var: Int, Variables: TableVar): Int = {
    var pertenencia: Float = 0
    var new_pert: Float = 0
    var interv = -1
    for (i <- 0 until Variables.getNLabelVar(num_var)){
      new_pert = Variables.Fuzzy(num_var, i, valor)
      if (new_pert > pertenencia) {
        interv = i
        pertenencia = new_pert
      }
    }
    interv
  }

  override def getClassIndividual(Variables: TableVar, Examples: TableDat, eje: Int): Float = {
    val compatibility = 0
    compatibility
  }

  /**
    * <p>
    * Method to Print the contents of the individual
    * </p>
    *
    * @param nFile File to write the individual
    */
  override def Print(nFile: String): Unit = {
    var contents = ""
    cromosoma.Print(nFile)
    contents = "Evaluated - " + evaluado + "\n"
    contents += "Evaluaci?n Generado " + n_eval + "\n\n"
    contents += "Fitness: " + getMeasures.getFitness + "\n"
    contents += "Growth Rate: " + getMeasures.getGRat + "\n"
    if (nFile eq "") System.out.print(contents)
  }

  override def evalExample(Variables: TableVar, data: TypeDat, index: Long, cubiertos: util.BitSet): ConfusionMatrix = {
    val mat = new ConfusionMatrix(1)
    var disparoCrisp = 1
    for (i <- 0 until Variables.getNVars) {
      if (!Variables.getContinuous(i)) {
        // Discrete variables
        if (cromosoma.getCromGeneElem(i, Variables.getNLabelVar(i))) {
          if (!cromosoma.getCromGeneElem(i, data.getDat(i).toInt) && data.getLost(Variables, 0, i)) {
            disparoCrisp = 0
          }
        } else {
          mat.numVarNoInterv += 1
        }
      } else {
        // Continuous variable
        if (cromosoma.getCromGeneElem(i, Variables.getNLabelVar(i))) {
          if(!data.getLost(Variables,0,i)){
            if(!cromosoma.getCromGeneElem(i, NumInterv(data.getDat(i),i,Variables))){
              disparoCrisp = 0
            }
          }
        } else {
          mat.numVarNoInterv += 1
        }
      }
    }

    if(disparoCrisp > 0){
      cubre.set(index toInt)
      mat.ejAntCrisp += 1
      mat.coveredExamples += index
      if(data.getClas == Variables.getNumClassObj){
        mat.ejAntClassCrisp += 1
        mat.tp += 1
      } else {
        mat.ejAntNoClassCrisp += 1
        mat.fp += 1
      }
      // cubreClase[Examples.getClass(i)]++; // Como hago yo esto?
      // AQUI TENEMOS UN PROBLEMA CON LOS NUEVOS EJEMPLOS CUBIERTOS

      if((!cubiertos.get(index toInt)) && (data.getClas == Variables.getNumClassObj)){
        mat.ejAntClassNewCrisp += 1
      }
    } else {
      if(data.getClas == Variables.getNumClassObj){
        mat.fn += 1
      } else {
        mat.tn += 1
      }
    }

    mat
  }
}
