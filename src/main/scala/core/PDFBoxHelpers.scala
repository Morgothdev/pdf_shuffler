package core

import org.apache.pdfbox.pdmodel.PDDocument

import pl.morgoth.scala.utils.Helpers.riturn

object PDFBoxHelpers {

  def withOpenDocument[T <: PDDocument, R](document: T)(fun: T => R) =
    try {
      fun(document)
    } finally {
      document.close()
    }
}