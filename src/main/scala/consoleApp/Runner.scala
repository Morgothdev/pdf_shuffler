package consoleApp

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream

import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.pdmodel.PDPage

import core.Book
import core.PDFBoxHelpers._
import core.PDFLoader._
import pl.morgoth.scala.utils.Log
import pl.morgoth.scala.utils.StreamHelpers._
import scopt.OptionParser

object Runner {

  case class Config(
    pagesOnPage: Int = 1,
    sheetsInCrop: Int = 4,
    blankPage: Int = -1,
    verbose: Boolean = false,
    numbers: Boolean = false,
    files: Seq[File] = Seq())

  def main(args: Array[String]): Unit = {

    val parser = new OptionParser[Config]("shuffler") {
      opt[Int]('p', "pages") validate { x =>
        if (x > 0) success else failure("pages on page must be more than 0")
      } action { (x, c) =>
        c.copy(pagesOnPage = x)
      } valueName ("<int>") text ("number of pages on page, default: 1")

      opt[Int]('s', "sheets") validate { x =>
        if (x > 0) success else failure("sheets in crop must be more than 0")
      } action { (x, c) =>
        c.copy(sheetsInCrop = x)
      } valueName ("<int>") text ("number of sheets in one crop, default: 4")

      arg[File]("<file>...") minOccurs (1) unbounded () required () action { (x, c) =>
        c.copy(files = c.files :+ x)
      } text ("input files")

      opt[Unit]('v', "verbose") action { (_, c) =>
        c.copy(verbose = true)
      } text ("tell me more!")

      opt[Unit]('n', "numbers") action { (_, c) =>
        c.copy(numbers = true)
      } text ("only numbers, don't write ouput file")

      help("help") text ("prints this usage text")
    }

    parser.parse(args, Config()) match {
      case Some(x) => doWithConfig(x)
      case None => parser.showUsage
    }
  }

  def doWithConfig(config: Config) {
    val logger = Log(config.verbose)
    val correctFiles = filterFiles(config.files, logger)
    correctFiles foreach (doWithFile (_, config, logger))
    logger.log("Thank You for some work with me!")
  }

  def makeOutputFile(input: File) =
    new File(input.getAbsoluteFile().getParent() + File.separator+"shuffled_"+input.getName())

  def newPDPage = {
    val ret = new PDPage(PDPage.PAGE_SIZE_A4)
    ret
  }

  def doWithFile(file: File, config: Config, logger: Log) {
    val document = withOpenStream(new FileInputStream(file)) {
      PDDocument load _
    }
    withOpenDocument(document){ document =>
      {
        val pages = loadPages(document)
        val tuples = pages.zipWithIndex
        val pagesInCrop = config.pagesOnPage * config.sheetsInCrop * 4
        val blanks = ((for (i <- 1 to pagesInCrop) yield newPDPage) toList) zip (List.fill(pagesInCrop - 1)(-1))
        require(blanks.length == (pagesInCrop - 1))
        val book: List[(PDPage, Int)] = Book(tuples, config.pagesOnPage, config.sheetsInCrop, blanks) toList ()

        val outputNumbers = book.map(_._2).mkString(file.getName()+": \n", ", ", "\n\n")
        if (!config.verbose && config.numbers) {
          println(outputNumbers)
        } else {
          logger.log(outputNumbers)
        }

        if (!config.numbers) {

          val outFile = makeOutputFile(file)
          logger.log("zapis w pliku: "+outFile.getAbsolutePath())
          outFile.delete()
          outFile.createNewFile()
          withOpenDocument(new PDDocument()){ document =>
            {
              book foreach (document importPage _._1)
              withOpenStream(new FileOutputStream(outFile)){ stream =>
                document save stream
              }
            }
          }
        }
      }
    }
  }

  def filterFiles(files: Seq[File], logger: Log) =
    filterFilesNotDirs(filterExisting(files, logger), logger)

  private def filterFilesNotDirs(files: Seq[java.io.File], logger: Log): Seq[java.io.File] =
    files filter { f =>
      if (!f.isFile()) {
        logger.log("file "+f.getPath()+" isn't a file!")
        false
      } else true
    }

  private def filterExisting(files: Seq[java.io.File], logger: Log): Seq[java.io.File] =
    files filter { f =>
      if (!f.exists()) {
        logger.log("file "+f.getPath()+" doesn't exists!")
        false
      } else true
    }

}