package unof.cv.base.charmaker

object CMCategory{
  private var link = 0;
  def newLinkKey = {
    val usedLinkKey = link
    link += 1
    usedLinkKey
  }
}
class CMCategory(
    val categoryName: String,
    val possibleParts: Seq[CMPart]) {
  if (categoryName.isEmpty())
    throw new IllegalArgumentException("Categories must have a name")
  def setName(newName : String) = new CMCategory(newName,possibleParts)
  def setPart(newParts : Seq[CMPart]) = new CMCategory(categoryName,newParts)
}