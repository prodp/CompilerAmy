package amyc.test

import amyc.utils.Pipeline
import java.io.File

abstract class TestSuite extends CompilerTest {
  val pipeline: Pipeline[List[File], Unit]

  val baseDir: String

  val passing = "passing"
  val failing = "failing"
  val outputs = "outputs"

  val outputExt: String

  def shouldOutput(inputFiles: List[String], outputFile: String, input: String = ""): Unit = {
    compareOutputs(
      pipeline,
      inputFiles map (f => s"$baseDir/$passing/$f.scala"),
      s"$baseDir/$outputs/$outputFile.$outputExt",
      input
    )
  }

  def shouldOutput(inputFile: String): Unit = {
    shouldOutput(List(inputFile), inputFile)
  }

  def shouldFail(inputFiles: List[String], input: String = ""): Unit = {
    demandFailure(
      pipeline,
      inputFiles map (f => s"$baseDir/$failing/$f.scala"),
      input
    )
  }

  def shouldFail(inputFile: String): Unit = {
    shouldFail(List(inputFile))
  }

  def shouldPass(inputFiles: List[String], input: String = ""): Unit = {
    demandPass(pipeline, inputFiles map (f => s"$baseDir/$passing/$f.scala"), input)
  }

  def shouldPass(inputFile: String): Unit = {
    shouldPass(List(inputFile))
  }

}
