package evaep

import utils.Randomize

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
class CromCAN (lenght: Int) extends Serializable {

  private var num_genes:Int = lenght
  private var cromosoma:Array[Int] = new Array[Int](lenght)

  /**
    * Random initialization of an existing chromosome
    * @param Variables		Contents the type of the variable, and the number of labels.
    */
  def RndInitCrom(Variables: TableVar, porcVar: Float): Unit = {

    /*var i= -1
    cromosoma.foreach(g => {i+=1
      cromosoma(i) = Randomize.Randint(0,Variables.getNLabelVar(i))
    })*/

    //int num_var: Int = 0

    // This array indicates if every chromosome has been initialised
    var crom_inic : Array[Boolean] = new Array[Boolean](num_genes)
    crom_inic = crom_inic.map { x => false }

    // Firtly, we obtain the numbero of variable which are in the chromosome
    Randomize.Randint (1, Math.round(porcVar * Variables.getNVars));
    val numInterv = Math.round(porcVar * Variables.getNVars);

    var vari=0
    while (vari<numInterv) {
      val num_var = Randomize.Randint (0, num_genes-1);
      // If the variable is not in the chromosome
      if (crom_inic(num_var) == false) {
        cromosoma(num_var) = Randomize.Randint (0, Variables.getNLabelVar(num_var)-1);
        crom_inic(num_var) = true;
        vari += 1
      }
    }

    // Initialise the rest variables
    var i= -1
    crom_inic.foreach(x =>{ i += 1
      if(!x){
        cromosoma(i) = Variables.getNLabelVar(i)
      }
    })
  }

  /**
    * Biased Random initialization of an existing chromosome
    * @param Variables		Contents the type of the variable, and the number of labels.
    * @param porcVar           Percentage of participating variables
    */
  def BsdInitCrom (Variables: TableVar, Examples: TableDat, porcVar: Float) = {

    //int num_var;

    var crom_inic : Array[Boolean] = new Array[Boolean](num_genes)
    var i = -1
    crom_inic.foreach( x => {i += 1
      crom_inic(i) = false
    })
    i = -1

    // Number of participating variables in the chromosome
    //int numInterv = Randomize.Randint (1, Math.round(porcVar*Variables.getNVars()));
    Randomize.Randint (1, Math.round(porcVar*Variables.getNVars))
    val numInterv = Math.round(porcVar*Variables.getNVars)


    var centi = false
    var aleatorio = 0
    var ii=0
    //Search an example not covered and for the objective class
    while((!centi) && (ii < Examples.getNEx)){
      aleatorio = Randomize.Randint(0, Examples.getNEx - 1)
      if( !Examples.getCovered(aleatorio) && (Examples.getClass(aleatorio) == Variables.getNumClassObj)) {
        centi = true
      }
      ii += 1
    }

    val data: TypeDat = Examples.datosRDD.lookup(aleatorio.toLong).apply(0)
    //In aleatorio we store the example to initiate the chromosome
    var vari = 0
    while (vari < numInterv) {
      val num_var = Randomize.Randint (0, num_genes-1)
      // If the variable is not in the chromosome
      if (!crom_inic(num_var)) {
        if (Variables.getContinuous(num_var)) { //Continuous variable
        // Put in the correspondent interval
        var pertenencia: Float = 0
          var new_pert: Float = 0
          var interv: Int = Variables.getNLabelVar(num_var)-1
          for (i <- 0 until Variables.getNLabelVar(num_var)) {
            new_pert = Variables.Fuzzy(num_var,i, data.getDat(num_var).toInt)
            if (new_pert > pertenencia) {
              interv = i
              pertenencia = new_pert
            }
          }
          cromosoma(num_var) = interv
        } else { //Discrete variable
          // Put in the correspondent value //
          cromosoma(num_var) = data.getDat(num_var).toInt
        }
        crom_inic(num_var) = true
        vari += 1
      }
    }

    i = -1
    // Initialise the rest variables
    crom_inic.foreach(x => {i += 1
      if(!x){
        cromosoma(i) = Variables.getNLabelVar(i)
      }
    })

  }



  /**
    * Retuns the value of the gene indicated
    * @param pos      Position of the gene
    * @return              Value of the gene
    */
  def getCromElem(pos: Int): Int = cromosoma(pos)


  /**
    * Sets the value of the indicated gene of the chromosome
    * @param pos      Position of the gene
    * @param value         Value of the gene
    */
  def setCromElem (pos: Int, value: Int): Unit = {
    cromosoma(pos) = value
  }


  /**
    * Retuns the gene lenght of the chromosome
    * @return          Gets the lenght of the chromosome
    */
  def getCromLength: Int = {
    num_genes
  }


  /** NOTA: Falta escribir en fichero cuando se pase por parámetro
    * Prints the chromosome genes
    * @param nFile         File to write the cromosome
    */
  def Print(nFile:String): Unit = {
    var contents: String = "Chromosome: "

    cromosoma.foreach(g => contents+= g + " ")
    contents+= "\n"

    if (nFile=="") println(contents)
    //else //File.AddtoFile(nFile, contents)
  }

  /** NOTA: Falta escribir en fichero cuando se pase por parámetro
    * Prints the chromosome genes
    * @param nFile         File to write the cromosome
    */
  def Print2(nFile:String): String = {
    var contents: String = "Chromosome: "

    cromosoma.foreach(g => contents+= g + " ")
    contents+= "\n"

    contents
    //else //File.AddtoFile(nFile, contents)
  }

}
