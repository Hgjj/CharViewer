package unof.cv.tools
import scala.scalajs.js

@js.native
trait CvSetting extends js.Object{
  def characterCanvas : String
  def partMenuComponent : String
  def colorsMenuComponent : String
  def partsMenuComponent : String
  def partMenuContainer : String
  def imgMenuComponent : String
  def partsMenuFullWidth : Number
  def currentCatSpanId : String
  def currentPartSpanId : String
  def currentRefSpanId : String
  def xTranslationField : String
  def yTranslationField : String
  def zField : String
  def xScaleField : String
  def yScaleField : String
  def rotationField : String
  def partSelectionField : String
  def categorySelectionField : String
  def sourceSelectionField : String
  def colorBindingField : String
  def relocateImageButton : String
  def deletImageButton : String
  def copyImageButton : String
	def categorySuggestionList : String
	def partSuggestionLsit : String
	def saveButton : String
	def saveFileName : String
	def colorsSuggestionList : String
	def partMenuLayerList : String
	def partMenuImportInput : String
	def imageSourceDiv : String
	def imagesImportDiv : String
	def layerControlDiv : String
	def categoriesTabContainer : String
	def newCategoryButton : String
	def importLayersButton : String
	def changeImageButton : String
	def fileSuggestionList : String
	def zLayerSorterCanva : String
	def partLocationInputDiv : String
	def conditionsDiv : String
  def boundableCatList : String
  def boundablePartList : String
  def conditionTypeSelect : String
  def catBindingInput : String
  def partBindingInput : String
  def validateCondtion : String
	def elementComponentDiv : String
	def cookieSaveButton : String
	def undoButton : String
	def redoButton : String
	def colorPanel : String
  def shapeColorPanel : String
  def lineColorDiv : String
  def surfaceColorDiv : String
  def shapeColorsButton : String
  def lineBoundColorInput : String
  def surfaceBoundColorInput : String
  def surfaceColorInput : String
  def lineColorInput : String
  def lineAlpha : String
  def surfaceAlpha : String
  def lineJoinSelect : String
  def fillShapeSwitch : String
  def closeShapeShwitch : String
  def lineWidth : String
  def shapeParamsPannel : String
  def shapeEditPannel : String
  def shapeEditButton : String
  def shapeEditButtonOn : String
  def shapeEditButtonOff : String
  def slidersDiv : String
  def elementName : String
  def renameInput : String
  def renameButton : String
  def newShapeButton : String
  def newImageButton : String
  def newPartButton : String
  def sliderBindingInput : String
  def oppSelect : String
  def conditionCeil : String
  def deltasDiv : String
  def tranformationList : String
  def sliderList : String
  def tranformationSliderSelect : String
  def tranformationPosition : String
  def tranformationSourceList : String
  def bindDeltaButton : String
	def colorSlotsCount : Number
	def maxSliders : Number
	def cookiSavePeriode : Number
	def shapeHandleSize : Number
	def wheelZoomSpeed : Number
	def colorBindingButton : String
	def devMod : Boolean
	def verbose : Boolean
}
//^ +"(.+)" : .*
//  def $1 : String