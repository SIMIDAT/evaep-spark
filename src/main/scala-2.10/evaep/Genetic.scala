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
class Genetic extends Serializable {

  private var poblac: Population = null;     // Main Population
  private var auxiliar: Population = null;   // Auxiliar population

  private var long_poblacion: Int = 0;   // Number of individuals of the population
  private var n_eval: Int = 0;           // Number of evaluations per ejecution
  private var prob_cruce: Float = 0;     // Cross probability
  private var prob_mutacion: Float = 0;  // Mutation probability
  private var Gen: Int = 0;		  // Number of generations performed by the GA
  private var Trials: Int = 0;		  // Number of evaluated chromosomes

  private var best_guy: Int = 0;         // Position of the best buy
  private var fitness: String = "";       // Name of the fitness function

  private var RulesRep: String = "CAN";
  private var Initialisation: String = "";      // Type of initialisation random or biased
  private var unballanced: Boolean = true;
  private var roundrobin: Boolean = false;


  /**
    * <p>
    * Sets the lenght of the population
    * </p>
    * @param value             Lenght of the population
    */
  def setLengthPopulation (value: Int) = {
    long_poblacion = value;
  }

  /**
    * <p>
    * Gets the lenght of the population
    * </p>
    * @return                  Lenght of the population
    */
  def getLengthPopulation : Int = {
    return long_poblacion;
  }


  /**
    * <p>
    * Sets the number of evaluations of the algorithm
    * </p>
    * @param value             Number of evaluations
    */
  def setNEval (value: Int) = {
    n_eval = value;
  }

  /**
    * <p>
    * Gets the number of evalutions of the algorithms
    * </p>
    * @return                  Number of evaluations
    */
  def getNEval: Int = {
    return n_eval;
  }

  /**
    * <p>
    * Sets the position of the best guy of population
    * </p>
    * @param value             Position
    */
  def setBestGuy (value: Int) = {
    best_guy = value;
  }

  /**
    * <p>
    * Gets the position of the best guy in population
    * </p>
    * @return                  Number of evaluations
    */
  def getBestGuy: Int = {
    return best_guy;
  }


  /**
    * <p>
    * Sets the cross probability in the algorithm
    * </p>
    * @param value             Cross probability
    */
  def setProbCross (value: Float) = {
    prob_cruce = value;
  }

  /**
    * <p>
    * Gets the cross probability
    * </p>
    * @return          Cross probability
    */
  def getProbCross: Float = {
    return prob_cruce;
  }


  /**
    * <p>
    * Sets the mutation probability
    * </p>
    * @param value             Mutation probability
    */
  def setProbMutation (value: Float) = {
    prob_mutacion = value;
  }

  /**
    * <p>
    * Gets the mutation probability
    * </p>
    * @return                  Mutation probability
    */
  def getProbMutation: Float = {
    return prob_mutacion;
  }


  /**
    * <p>
    * Sets the value of a gene
    * </p>
    * @param value             Value of the gene
    */
  def setGen(value: Int) = {
    Gen = value;
  }

  /**
    * <p>
    * Gets the value of a gene
    * </p>
    * @return                  Value of the gene
    */
  def getGen: Int = {
    return Gen;
  }


  /**
    * <p>
    * Sets the number of trials in the algorithm
    * </p>
    * @param value             Number of trials
    */
  def setTrials(value: Int) = {
    Trials = value;
  }

  /**
    * <p>
    * Gets the number of trials in the algorithm
    * </p>
    * @return                  Number of trials
    */
  def getTrials: Int = {
    return Trials;
  }


  /**
    * <p>
    * Gets the name of fitness function
    * </p>
    * @return
    */
  def getFitness: String = {
    return fitness;
  }

  /**
    * <p>
    * Sets the value of the fitness function
    * </p>
    * @param value
    */
  def setFitness(value: String) = {
    fitness = value;
  }


  /**
    * <p>
    * Gets the name of round robin
    * </p>
    * @return
    */
  def getRoundRobin: Boolean = {
    return roundrobin;
  }

  /**
    * <p>
    * Sets the value of round robin
    * </p>
    * @param value
    */
  def setRoundRobin(value: Boolean) = {
    roundrobin = value;
  }


  /**
    * <p>
    * Gets if the algorithm uses re-initialisation based on coverage
    * </p>
    * @return              The uses of re-initialisation based on coverage
    */
  def getInitialisation: String = {
    return Initialisation;
  }

  /**
    * <p>
    * Sets the value of re-initialisation based on coverage
    * </p>
    * @param value         Value of the re-inisitalisation based on coverage
    */
  def setInitialisation(value: String) = {
    Initialisation = value;
  }


  /**
    * <p>
    * Gets the rules representation of the algorithm
    * </p>
    * @return                  Representation of the rules
    */
  def getRulesRep: String = {
    return RulesRep;
  }


  /**
    * <p>
    * Sets the rules representation of the algorithm
    * </p>
    * @param value             Representation of the rule
    */
  def setRulesRep(value: String) = {
    RulesRep = value;
  }


  /**
    * <p>
    * Applies the selection schema of the genetic algorithm.
    * k-Tournament selection
    * </p>
    * @return              Position of the individual selected
    */
  def Select: Int = {

    val opponent1: Int = Randomize.Randint(0,long_poblacion-1)
    var opponent2: Int = opponent1

    while (opponent2 == opponent1) {
      opponent2 = Randomize.Randint(0, long_poblacion - 1)
    }

    val fit1 = poblac.getIndiv(opponent1).getMeasures.getNSup * 0.5 + poblac.getIndiv(opponent1).getMeasures.getFitness * 0.5
    val fit2 = poblac.getIndiv(opponent2).getMeasures.getNSup * 0.5 + poblac.getIndiv(opponent2).getMeasures.getFitness * 0.5

    val winner = if(fit1 > fit2){
     opponent1
    } else if (fit1 == fit2){
      if(poblac.getIndiv(opponent1).getMeasures.getNSup > poblac.getIndiv(opponent2).getMeasures.getNSup){
        opponent1
      } else {
        opponent2
      }
    } else {
      opponent2
    }

    winner

  }


  /**
    * <p>
    * Cross operator for the genetic algorithm
    * </p>
    * @param Variables         Variables structure
    * @param dad               Position of the daddy
    * @param mom               Position of the mummy
    * @param contador          Position to insert the son
    * @param neje              Number of examples
    */
  def CrossMultipoint (Variables: TableVar, dad: Int, mom: Int, contador: Int, neje: Int) = {

    // Copy the individuals to cross
    for (i <- 0 until Variables.getNVars) {
        auxiliar.setCromElem((contador*2)-1, i, 0, poblac.getCromElem(mom,i,0,RulesRep), RulesRep)
        auxiliar.setCromElem((contador*2), i, 0, poblac.getCromElem(dad,i,0,RulesRep), RulesRep)
    }

    val cruce: Double = Randomize.Randdouble(0.0,1.0)


    if (cruce <= getProbCross){
      // Generation of the two point of cross
      if(RulesRep equalsIgnoreCase "can") {
        // Can RULES
        val xpoint1: Int = Randomize.Randint(0, Variables.getNVars - 1)
        val xpoint2 = if (xpoint1 != Variables.getNVars - 1) {
          Randomize.Randint(xpoint1 + 1, Variables.getNVars - 1)
        } else {
          Variables.getNVars - 1
        }
        // Cross the parts between both points
        for (i <- xpoint1 to xpoint2) {
          auxiliar.setCromElem((contador * 2) - 1, i, 0, poblac.getCromElem(dad, i, 0, RulesRep), RulesRep)
          auxiliar.setCromElem(contador * 2, i, 0, poblac.getCromElem(mom, i, 0, RulesRep), RulesRep)
        }
      } else {
        // DNF Rules

      }
    } else {
      auxiliar.CopyIndiv((contador*2)-1, neje, poblac.getIndiv(dad))
      auxiliar.CopyIndiv((contador*2), neje, poblac.getIndiv(mom))
    }

  }


  /**
    * <p>
    * Mutates an individual
    * </p>
    * @param Variables             Variables structure
    * @param pos                   Position of the individual to mutate
    */
  def Mutation (Variables: TableVar, pos: Int): Unit = {

    //posiciones = Variables.getNVars();

    if (this.getProbMutation > 0) {
      for(i <- 0 until Variables.getNVars){

        val mutar = Randomize.Randdouble(0.00,1.00)
        if(mutar <= getProbMutation){
          val eliminar = Randomize.Randint (0,10)
          if (eliminar <= 5){
            if (!Variables.getContinuous(i)) {
              auxiliar.setCromElem(pos, i, 0, (Variables.getMax(i) toInt) + 1, RulesRep)
            } else {
              auxiliar.setCromElem(pos, i, 0, Variables.getNLabelVar(i), RulesRep)
            }
          } else {
            if (!Variables.getContinuous(i)) {
              auxiliar.setCromElem(pos, i, 0, Randomize.Randint(0, Variables.getMax(i) toInt), RulesRep)
            } else {
              auxiliar.setCromElem(pos, i, 0, Randomize.Randint(0, Variables.getNLabelVar(i) - 1), RulesRep)
            }
          }

          // Marks the chromosome as not evaluated
          //auxiliar.indivi(pos).evaluado = false
          auxiliar.setIndivEvaluated(pos,false)
        }
      }
    }

  }


  /**
    * <p>
    * Composes the genetic algorithm applying the operators
    * </p>
    * @param Variables         Variables structure
    * @param Examples          Examples structure
    * @param nFile             File to write the process
    * @return                  Final Pareto population
    */
  def GeneticAlgorithm (Variables: TableVar, Examples: TableDat, nFile: String): Individual = {

    var contents: String = ""
    val porcPob: Float =  0.5f  //Percentage of population: biased and random
    val porcVar: Float = 0.8f          //Maximum number of variables in the individuals
    var best_guy: Int = 0            //Position of the best guy

    poblac = new Population(long_poblacion, Variables.getNVars, Examples.getNEx, RulesRep, Variables, Trials)
    val t_ini = System.currentTimeMillis()
    poblac.BsdInitPob(Variables, Examples, porcVar, porcPob, Examples.getNEx, nFile)
    println("TIME: " + (System.currentTimeMillis() - t_ini)  )
    //println("CALLS To RANDOMIZE: " + Randomize.calls)

    Trials = 0
    Gen = 0

    //Evaluates the population
    Trials += poblac.evalPop(this, Variables, Examples)

    val t_ini2 = System.currentTimeMillis()
    do { // GA General cycle

      Gen += 1

      // Initialise auxiliar
      auxiliar = null
      auxiliar = new Population(long_poblacion, Variables.getNVars, Examples.getNEx, RulesRep, Variables, Trials)
      // Introduce the best individual of population in auxiliar

      best_guy = BestIndividual(poblac)

      auxiliar.CopyIndiv(0, Examples.getNEx, poblac.getIndiv(best_guy))

      for(conta <- 1 until (long_poblacion/2)){

        // Select the daddy and mummy
        val dad: Int = Select
        var mum: Int = Select

        while (mum == dad) {
          mum = Select
        }

        // Crosses
        CrossMultipoint(Variables, dad, mum, conta, Examples.getNEx)

        // Mutates
        Mutation(Variables, (conta*2)-1)
        Mutation(Variables, (conta*2))
      }


      if(long_poblacion % 2 == 0){
        val dad = Select
        auxiliar.CopyIndiv(long_poblacion-1, Examples.getNEx, poblac.getIndiv(dad))
      }

      //Copy auxiliar population in poblac population
      poblac.CopyPopulation(auxiliar,Examples.getNEx)

      Trials += poblac.evalPop(this, Variables, Examples)

    } while (this.Trials <= this.n_eval)
    println("TIME GA: " + (System.currentTimeMillis() - t_ini2)  )
    this.poblac.getIndiv(best_guy)

  }

  /**
    * It gets the best individual in the population
    */
  def BestIndividual (p: Population): Int = {

    var best: Int = 0
    var maximo: Double = Double.NegativeInfinity

    var j = -1

    p.indivi.foreach( i => { j += 1
      if(i.getMeasures.getGRat > 1){
        if((i.getMeasures.getNSup * 0.5 + i.getMeasures.getFitness * 0.5) >= maximo && i.getMeasures.getConf >= 0.6){
          best = j
          maximo = i.getMeasures.getNSup * 0.5 + i.getMeasures.getFitness * 0.5
        }
      }
    })

    best
  }

}
