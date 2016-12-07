package unof.cv.base.charLib

import org.scalajs.dom
import org.scalajs.dom.raw.Event
import org.scalajs.dom.raw.HTMLImageElement
import scala.scalajs.js.Any.fromFunction1

object ImageRef {
  def loadAll(images: Seq[ImageRef], imageHome: String)(onload: (Seq[HTMLImageElement]) => Unit): Unit = {

    val unloaded = images.filterNot { _.htmlImage.isDefined }
    val imageToLoad = unloaded.size
    var loadedImage = 0;
    def onImageLoaded(evt: Event) = this.synchronized {
      loadedImage += 1
      println("Image loading : "+(100 * loadedImage) / imageToLoad+"%")
      if (imageToLoad  == loadedImage) {
        onload(images.map(_.htmlImage.get))
      }

    }
    if(!unloaded.isEmpty)
      println("Image ref, loading : "+unloaded.mkString("[",",\n","\n]"));
    unloaded.foreach {
      ref =>
        val img = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
        img.onload = onImageLoaded _
        ref.htmlImage = Some(img)
        if (ref.hRef.startsWith("http://") || ref.hRef.startsWith("https://") || ref.hRef.startsWith("www."))
          img.src = ref.hRef
        else
          img.src = imageHome + ref.hRef

    }

    if (imageToLoad == loadedImage)
      onload(images.map(_.htmlImage.get))

  }
}
class ImageRef(val hRef: String) {
  var htmlImage: Option[HTMLImageElement] = None
  def dimensions: (Double, Double) = htmlImage match {
    case None          => (0, 0)
    case Some(element) => (element.width, element.height)
  }
  override def toString = htmlImage match {
    case Some(i) => "( hRef : " + hRef + " " + i.src + " )"
    case None    => "( hRef : " + hRef + " " + None + " )"
  }
}