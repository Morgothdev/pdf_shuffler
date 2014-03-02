package core

class Book[U](zbitki: List[CropOfPages[U]]) {
  override def toString() = zbitki.foldLeft("")(_ + _)

  def toList() =
    if (zbitki isEmpty) List[U]() else zbitki.flatMap(_ toList)
}

object Book {

  private def scalStrony[U](strony: List[U], dzielnik: Int): List[MergedToOnePage[U]] =
    strony.length match {
      case 0 => List[MergedToOnePage[U]]()
      case _ => new MergedToOnePage(strony.slice(0, dzielnik)) :: scalStrony(strony.drop(dzielnik), dzielnik)
    }

  private def uzupelnijDoZbitki[U](strony: List[U], pagesOnPage: Int, blanks: List[U]) = {
    val maBycPelnychSCalonych = ((strony.length + pagesOnPage - 1) / pagesOnPage)
    val potrzebaKartek = (maBycPelnychSCalonych + 3) / 4
    val potrzebaStron = potrzebaKartek * 4 * pagesOnPage
    val brakujeStron = potrzebaStron - strony.length
    println("potrzeba: " + potrzebaStron + ", brakuje " + brakujeStron + ", potrzeba kartek " + potrzebaKartek)
    val ret = strony ::: blanks.take(brakujeStron)
    println(ret)
    ret
  }

  private def zbitki[U](strony: List[MergedToOnePage[U]], dzielnik: Int): List[CropOfPages[U]] = {
    if ((dzielnik % 2) != 0) throw new IllegalArgumentException("nie parzyste!" + dzielnik)
    strony.length match {
      case 0 => List()
      case _ => new CropOfPages(strony.slice(0, dzielnik)) :: zbitki(strony.drop(dzielnik), dzielnik)
    }
  }

  private def stronWZbitce(pagesInCrop: Int, pagesOnPage: Int) = pagesInCrop * 2 * 2 * pagesOnPage

  def apply[U](pages: List[U], pagesOnPage: Int, sheetsInCrop: Int, blanks: List[U]) = {
    val stronPierwotnychWZbitces = stronWZbitce(sheetsInCrop, pagesOnPage)
    val stronScalonychWZbitce = stronPierwotnychWZbitces / pagesOnPage
    println("stronWZbitces = " + stronPierwotnychWZbitces)

    if (blanks.length < (stronPierwotnychWZbitces - 1)) {
      throw new IllegalArgumentException("to few blank units")
    }
    println("pages = " + pages.length)
    val fullCropsPagesCount = (pages.length / stronPierwotnychWZbitces) * stronPierwotnychWZbitces
    println("fullCropsPagesCount = " + fullCropsPagesCount)
    val pagesForFullCrops = pages.take(fullCropsPagesCount)
    println("pagesForFullCrops = " + pagesForFullCrops.length)
    val restOfPages = pages drop fullCropsPagesCount
    println("zostało " + restOfPages.length + " stron")
    val fullCropsPages = scalStrony(pagesForFullCrops, pagesOnPage)
    val fullCrops = zbitki(fullCropsPages, stronScalonychWZbitce)
    println("będize pełnych zbitek " + fullCrops.length)
    val uzupelnioneDoZbitki = uzupelnijDoZbitki[U](restOfPages, pagesOnPage, blanks)

    val scalonaResztka = scalStrony(uzupelnioneDoZbitki, pagesOnPage)
    val restCrop = new CropOfPages(scalonaResztka)
    new Book(fullCrops ::: List(restCrop))
  }
}

class MergedToOnePage[T](pages: List[T]) {
  override def toString = pages.foldLeft("")(_ + _ + ",")

  def toList() = pages
}

class CropOfPages[T](x: List[MergedToOnePage[T]]) {
  val strony = pierwszaStrona(x)

  def pierwszaStrona(zbitka: List[MergedToOnePage[T]]): List[MergedToOnePage[T]] =
    zbitka.length match {
      case 0 => List()
      case _ => zbitka.last :: zbitka.head :: drugaStrona(zbitka.drop(1).dropRight(1))
    }

  def drugaStrona(zbitka: List[MergedToOnePage[T]]): List[MergedToOnePage[T]] =
    zbitka.length match {
      case 0 => List()
      case _ => zbitka.head :: zbitka.last :: pierwszaStrona(zbitka.drop(1).dropRight(1))
    }

  override def toString() = strony.foldLeft("")(_ + _)

  def toList() =
    strony flatMap (_ toList)

}