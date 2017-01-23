package utils

import evaep.Genetic

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
class QualityMeasures extends Serializable{
  /**
    * <p>
    * Defines the quality measures of the individual
    * </p>
    */

  private var leng: Double = 0.0
  private var unus: Double = 0.0
  private var gain: Double = 0.0
  private var sens: Double = 0.0
  private var tpr: Double = 0.0
  private var fpr: Double = 0.0
  private var difs: Double = 0.0
  private var conf: Double = 0.0
  private var grat: Double = 0.0
  private var nsup: Double = 0.0    //Support based on examples pending to cover
  private var medgeo: Double = 0.0
  private var fitness: Double = 0.0
  private var fisher: Double = 0.0
  private var hellinger: Double = 0.0

  def getFisher: Double = {
    fisher
  }

  def setFisher(value: Double) = {
    this.fisher = value
  }

  def getHellinger: Double = {
    hellinger
  }

  def setHellinger(value: Double) = {
    this.hellinger = value
  }


  /**
    * <p>
    * Gets the value of the confidence
    * </p>
    * @return                  Value of the confidence
    */
  def getConf : Double = {
    conf;
  }

  /**
    * <p>
    * Sets the value of the confidence
    * </p>
    * @param aconf              Value of the confidence
    */
  def  setConf (aconf: Double) = {
    conf = aconf;
  }


  /**
    * <p>
    * Gets the value of the sensitivity
    * </p>
    * @return                  Value of the sensitivity
    */
  def getSens : Double = {
    sens;
  }

  /**
    * <p>
    * Sets the value of the confidence
    * </p>
    * @param asens              Value of the sensitivity
    */
  def setSens (asens : Double) = {
    sens = asens;
  }


  /**
    * <p>
    * Gets the value of the new support
    * </p>
    * @return                  Value of the new support
    */
  def getNSup : Double = {
    nsup;
  }

  /**
    * <p>
    * Sets the value of the new support
    * </p>
    * @param asupp              Value of the new support
    */
  def setNSup (asupp : Double) = {
    nsup = asupp;
  }


  /**
    * <p>
    * Gets the value of the support for majority class
    * </p>
    * @return                  Value of the support for majority class
    */
  def getFPr () : Double = {
    fpr
  }

  /**
    * <p>
    * Sets the value of the support for majority class
    * </p>
    * @param afpr             Value of the support for majority class
    */
  def setFPr (afpr : Double) = {
    fpr = afpr
  }


  /**
    * <p>
    * Gets the value of the support for minority class
    * </p>
    * @return                  Value of the support for minority class
    */
  def getTPr : Double = {
    tpr
  }

  /**
    * <p>
    * Sets the value of the support for minority class
    * </p>
    * @param atpr             Value of the support for minority class
    */
  def setTPr (atpr : Double) = {
    tpr = atpr
  }


  /**
    * <p>
    * Gets the value of the unusualness
    * </p>
    * @return                  Value of the unusualness
    */
  def  getUnus : Double = {
    unus;
  }

  /**
    * <p>
    * Sets the value of the unusualness
    * </p>
    * @param aunus              Value of the unusualness
    */
  def setUnus (aunus : Double) = {
    unus = aunus;
  }

  /**
    * <p>
    * Gets the value of the different support
    * </p>
    * @return                  Value of the difference support
    */
  def  getDifS : Double = {
    difs;
  }

  /**
    * <p>
    * Sets the value of the difference support
    * </p>
    * @param asupp              Value of the support for majority class
    */
  def setDifS (asupp : Double) = {
    difs = asupp;
  }

  /**
    * <p>
    * Gets the value of the growth rate
    * </p>
    * @return                  Value of the growth rate
    */
  def getGRat : Double = {
    grat;
  }

  /**
    * <p>
    * Sets the value of the growth rate
    * </p>
    * @param grate              Value of the growth rate
    */
  def setGRat (grate : Double) = {
    grat = grate;
  }


  /**
    * <p>
    * Gets the value of the med geo
    * </p>
    * @return                  Value of the med geo
    */
  def getMedGeo : Double = {
    medgeo;
  }

  /**
    * <p>
    * Sets the value of the med geo
    * </p>
    * @param amedgeo              Value of the med geo
    */
  def setMedGeo (amedgeo : Double){
    medgeo = amedgeo;
  }

  /**
    * <p>
    * Gets the value of the gain
    * </p>
    * @return                  Value of the gain
    */
  def getGain : Double = {
    gain;
  }

  /**
    * <p>
    * Sets the value of the gain
    * </p>
    * @param again              Value of the gain
    */
  def setGain (again : Double) = {
    gain = again;
  }


  /**
    * <p>
    * Gets the value of the length
    * </p>
    * @return                  Value of the length
    */
  def getLength : Double = {
    leng
  }

  /**
    * <p>
    * Sets the value of the lenght
    * </p>
    * @param alen              Value of the length
    */
  def setLength (alen : Double) = {
    leng = alen;
  }


  /**
    * <p>
    * Gets the value of the fitness
    * </p>
    * @return                  Value of the fitness
    */
  def getFitness : Double = {
    fitness
  }

  /**
    * <p>
    * Sets the value of the fitness
    * </p>
    * @param afitness              Value of the fitness
    */
  def setFitness (afitness : Double){
    fitness = afitness;
  }

  /**
    * <p>
    * Copy in this object the values of qmeasures
    * </p>
    * @param qmeasures           Quality measures
    */
  def Copy (qmeasures : QualityMeasures ) {
    this.setLength(qmeasures.getLength);
    this.setUnus(qmeasures.getUnus);
    this.setGain(qmeasures.getGain);
    this.setNSup(qmeasures.getNSup);
    this.setSens(qmeasures.getSens);
    this.setFPr(qmeasures.getFPr);
    this.setTPr(qmeasures.getTPr);
    this.setDifS(qmeasures.getDifS);
    this.setConf(qmeasures.getConf);
    this.setGRat(qmeasures.getGRat);
    this.setMedGeo(qmeasures.getMedGeo);
    this.setFitness(qmeasures.getFitness);
  }


  /**
    * <p>
    * Prints the measures
    * </p>
    * @param nFile             File to write the quality measures
    * @param AG                Genetic algorithm
    */
  def Print(nFile : String, AG : Genetic) {

    val contents = "\tLength: ".concat(this.getLength.toString()).concat("\n").
      concat("\tUnus: ").concat(this.getUnus.toString()).concat("\n").
      concat("\tGain: ").concat(this.getGain.toString()).concat("\n")
      .concat("\tNSup: ").concat(this.getNSup.toString()).concat("\n")
      .concat("\tTPr: ").concat( this.getTPr.toString()).concat("\n")
      .concat ("\tFPr: ").concat( this.getFPr.toString()).concat("\n")
      .concat( "\tSens: ").concat(this.getSens.toString()).concat("\n")
      .concat("\tMedGeo: ").concat(this.getMedGeo.toString()).concat("\n")
      .concat("\tConf: ").concat(this.getConf.toString()).concat("\n")
    if (nFile=="")
      System.out.print (contents);
    //else
    // File.AddtoFile(nFile, contents);
  }


}
