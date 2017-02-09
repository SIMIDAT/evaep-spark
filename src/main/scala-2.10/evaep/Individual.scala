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

import utils.QualityMeasures

abstract class Individual extends Serializable{
  var tamano: Int = 0
  var evaluado: Boolean = false
  var cubre: java.util.BitSet

  var cubr : Float
  var n_eval: Int

  var medidas: QualityMeasures


  def RndInitInd(Variables: TableVar, porcVar: Float, neje: Int, nFile: String)


  def BsdInitInd(Variables: TableVar, Examples: TableDat, porcVar: Float, nFile: String): Individual


  /**
    * <p>
    * Returns the position i of the array cubre
    * </p>
    * @param pos               Position of example
    * @return                  Value of the example
    */
  def getIndivCovered (pos: Int): Boolean = {
    cubre.get(pos)
  }

  /**
    * <p>
    * Returns if the individual has been evaluated
    * </p>
    * @return                  Value of the example
    */
  def getIndivEvaluated: Boolean = {
    evaluado
  }

  /**
    * <p>
    * Sets that the individual has been evaluated
    * </p>
    * @param value               Value of the state of the individual
    */
  def setIndivEvaluated (value: Boolean): Unit =  {
    evaluado = value
  }

  /**
    * <p>
    * Returns the number of evaluation when the individual was created
    * </p>
    * @return                  Number of evalution when the individual was created
    */
  def getNEval: Int = {
    n_eval
  }

  /**
    * <p>
    * Sets the number of evaluation when the individual was created
    * </p>
    * @param eval              Number of evaluation when the individual was created
    */
  def setNEval (eval: Int): Unit = {
    n_eval = eval
  }

  /**
    * <p>
    * Return the quality measure of the individual
    * </p>
    * @return                  Quality measures of the individual
    */
  def getMeasures : QualityMeasures = {
    medidas
  }

  def getCromElem(pos: Int): Int
  def setCromElem (pos: Int, value: Int)

  def getCromGeneElem(pos: Int, elem: Int): Boolean
  def setCromGeneElem(pos: Int, elem: Int, value: Boolean)

  def getIndivCromCAN: CromCAN

  def copyIndiv (indi: Individual, neje: Int)

  def evalInd (AG: Genetic, Variables: TableVar, Examples: TableDat)

  def NumInterv (valor: Float, num_var: Int, Variables: TableVar): Int

  def getClassIndividual (Variables: TableVar, Examples: TableDat, eje: Int): Float

  def Print(nFile: String)


}
