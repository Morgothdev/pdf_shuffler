package core

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream

import scala.collection.JavaConverters.asScalaBufferConverter

import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.pdmodel.PDPage

import pl.morgoth.scala.utils.StreamHelpers.withOpenStream

object PDFLoader {

  def loadPages(document: PDDocument): List[PDPage] = {
    val sPages = document.getDocumentCatalog().getAllPages().asScala
    val casted = (sPages toList) map (_.asInstanceOf[PDPage])
    casted
  }
}