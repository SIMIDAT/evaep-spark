package evaep


import java.util

import utils.Randomize
import java.util.BitSet

/**
  * <p>
  *
  * @author Written by Cristobal J. Carmona (University of Jaen) 11/08/2008
  * @version 1.0
  * @since JDK1.5
  *        </p>
  */


class CromDNF(var num_genes: Int // Number of genes
              , val Variables: TableVar) extends Serializable{

  private val cromosoma = new Array[Gene](num_genes)

  for(i <- 0 until num_genes){
    cromosoma(i) = new Gene(Variables.getNLabelVar(i))
  }


  /**
    * <p>
    * Random initialization of an existing chromosome
    * </p>
    *
    * @param Variables Structure of variables of the dataset
    * @param porcVar   Participating variables in the chromosom
    */
  def RndInitCrom(Variables: TableVar, porcVar: Float): Unit = {
    var num_var = 0
    // This array indicates if every chromosome has been initialised
    val crom_inic = new BitSet(num_genes)
    crom_inic.clear(0,num_genes)

    // Firtly, we obtain the numbero of variable which are in the chromosome
    val numInterv = Randomize.Randint(1, Math.round(porcVar * Variables.getNVars))
    var `var` = 0
    while (`var` < numInterv) {
      num_var = Randomize.Randint(0, num_genes - 1)
      // If the variable is not in the chromosome
      if (crom_inic.get(num_var) == false) {
        cromosoma(num_var).RndInitGene()
        crom_inic.set(num_var)
        `var` += 1
      }
    }
  }

  /**
    * <p>
    * Initialization based on coverage
    * </p>
    *
    * @param Variables Contents the type of the variable, and the number of labels.
    * @param Examples  Dataset
    * @param porcVar   Percentage of participating variables
    */
  def BsdInitCrom(Variables: TableVar, Examples: TableDat, porcVar: Float): Unit = {
    var num_var = 0
    val crom_inic = new BitSet(num_genes)
    crom_inic.clear(0,num_genes)
    // Number of participating variables in the chromosome
    val numInterv = Randomize.Randint(1, Math.round(porcVar * Variables.getNVars))
    var centi = false
    var aleatorio = 0
    var ii = 0
    //Search an example not covered and for the objective class
    while ((!centi) && (ii < Examples.getNEx)) {
      aleatorio = Randomize.Randint(0, Examples.getNEx - 1)
      if (!(Examples.getCovered(aleatorio)) && (Examples.getClass(aleatorio) == Variables.getNumClassObj)){
        centi = true
      }
      ii += 1
    }
    //In aleatorio we store the example to initiate the chromosome
    var `var` = 0
    val data: TypeDat = Examples.datosRDD.lookup(aleatorio.toLong).apply(0)
    while ( `var` < numInterv) {
      num_var = Randomize.Randint(0, num_genes - 1)
      if (! crom_inic.get(num_var)) {
        if (Variables.getContinuous(num_var)) { //Continuous variable
          // Put in the correspondent interval //
          var pertenencia: Float = 0
          var new_pert: Float = 0
          var interv: Int = Variables.getNLabelVar(num_var)

          for (i <- 0 until Variables.getNLabelVar(num_var)){
            new_pert = Variables.Fuzzy(num_var, i, data.getDat(num_var).toInt)
            if (new_pert > pertenencia) {
              interv = i
              pertenencia = new_pert
            }
          }
          val number = Variables.getNLabelVar(num_var)
          for(l <- 0 to number){
            if (l != num_var)
              setCromGeneElem(num_var, l, false)
          }
          setCromGeneElem(num_var, interv, true)
          setCromGeneElem(num_var, number, true)

        } else { //Discrete variable
          // Put in the correspondent value //
          val number = Variables.getNLabelVar(num_var)
          for(l <- 0 to number){
            if (l != num_var) setCromGeneElem(num_var, l, false)
          }
          setCromGeneElem(num_var, data.getDat(num_var).toInt, true)
          setCromGeneElem(num_var, number, true)
        }
        crom_inic.set(num_var)
        `var` += 1
      }
    }
    // Initialise the rest variables
    for(i <- 0 until num_genes){
      if(!crom_inic.get(i)){
        cromosoma(i).NoTakeInitGene()
      }
    }

  }

  /**
    * <p>
    * Retuns the lenght of the chromosome
    * </p>
    *
    * @return Lenght of the chromosome
    */
  def getCromLenght: Int = num_genes

  /**
    * <p>
    * Retuns the gene lenght of the chromosome
    * </p>
    *
    * @return Lenght of the gene
    */
  def getCromGeneLenght(pos: Int): Int = cromosoma(pos).getGeneLenght

  /**
    * <p>
    * Retuns the value of the gene indicated
    * </p>
    *
    * @param pos  Position of the variable
    * @param elem Position of the gene
    */
  def getCromGeneElem(pos: Int, elem: Int): Boolean = cromosoma(pos).getGeneElem(elem)

  /**
    * <p>
    * Sets the value of the indicated gene of the Chromosome
    * </p>
    *
    * @param pos  Position of the variable
    * @param elem Position of the gene
    * @param val  Value to insert
    */
  def setCromGeneElem(pos: Int, elem: Int, `val`: Boolean): Unit = {
    cromosoma(pos).setGeneElem(elem, `val`)
  }

  /**
    * <p>
    * Prints the chromosome genes
    * </p>
    *
    * @param nFile File to write the chromosome
    */
  def Print(nFile: String): Unit = {
    var contents = "Chromosome: \n"
   for(i <- 0 until num_genes) {
      contents += "Var " + i + ": "
      val neti = getCromGeneLenght(i)
      for(l <- 0 to neti){
        contents += this.getCromGeneElem(i, l) + " "
      }
      contents += "\n"
    }
    if (nFile eq "") System.out.print(contents)
  }
}

