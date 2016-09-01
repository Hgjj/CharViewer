package unof.cv.tools

import unof.cv.base.charmaker.CharMaker
import unof.cv.base.DrawingContext
import unof.cv.base.charmaker.CMPart
import unof.cv.base.Algebra._
import unof.cv.base.AllKnownColors
import unof.cv.base.charmaker.CMImage
import unof.cv.base.charmaker.CMLayer

object ZSorterDrawer {
  val spaceForLayer : Int = 10
  val zIconDim : Vec = (20,20)
  val selectedPartLineWidth : Int = 2
  val partLineWidth : Int =  1
  val textGap : Vec = (1,1)
  val iconFontSize : Int = 12
  val iconBorderWidth : Int = 2
  val selectedIconFontEffect : String = "bold"
  val selectedIconFont: String = "Arial"
  val iconFont : String = "Arial"
  val defaultStachesLenght : Int= 10
  val magins : Vec = ( 10,20)
  val arrowBaseHalfWidth = zIconDim.y/2
  val arrowHeadLength = zIconDim.y
  def draw( cm : CharMaker, on : DrawingContext, zRatio : Double) {/*
    
    val ctx = on.ctx
    val drawingZoneStart = magins
    val drawingZoneDim = on.dimensions - magins*2
    val z0 = cm.categories.map(c => c.possibleParts.map(p=>p.partZ min(p.partZ + p.components.map(_.z).min)).min).min 
    val scale : Vec = if(zRatio > 0 ) {
      (1,zRatio)
    } else {
      val zMax = cm.categories.map(c => c.possibleParts.map(p=>p.partZ max(p.partZ + p.components.map(_.z).max)).max).max 
      val range = zMax - z0
     println("drawingZoneDim._2 "+drawingZoneDim._2+" range "+range)
      if(range == 0)
        (1,1)
      else{
        (1,drawingZoneDim._2 / range)
      }
     
        
    }
    def drawPartComponent(part : CMPart,partColor : String, contentWidth : Double, isSelected : Boolean, z0 : Double, x0 : Double) = {
       
      
      val lazerZs = part.components.map(_.z)
      val top = lazerZs.max
      val height = (top- lazerZs.min) * zRatio + zIconDim.y
      val lineWidth = if(isSelected)selectedPartLineWidth else partLineWidth
      drawPartBox((x0,top-z0+zIconDim._2/2),(contentWidth,height),partColor,lineWidth)
      drawZIcon((x0,part.partZ - z0)* scale, part.partName, colorFromName(part.partName),  isSelected, contentWidth.toInt)
    }
    def drawLayerComponent(parent : CMPart, layer : CMLayer, isSelected : Boolean, z0 : Double, x0 : Double) = {
     
      drawZIcon((x0,parent.partZ + layer.z - z0) * scale, layer.toString(), colorFromName(parent.partName), isSelected, -1)
    }
    def colorFromName(name :String) = {
      val hash = name.hashCode()
      AllKnownColors.list(hash.abs % AllKnownColors.list.size).code
    }
    def drawPartBox(topLeft : Vec, dim : Vec, color : String, lineWidth : Int){
      
      line((topLeft.x, 0)+drawingZoneStart,(topLeft.x, drawingZoneDim._2)+drawingZoneStart,lineWidth,color)
      line((topLeft.x + dim.x, 0)+drawingZoneStart,(topLeft.x + dim.x, drawingZoneDim._2)+drawingZoneStart,lineWidth,color)
      
      
    }
    def line(from : Vec, to : Vec, width : Int = 1, style : String = "black"){
      ctx.strokeStyle = style
      ctx.lineWidth = width
      ctx.beginPath();
      ctx.moveTo(from.x, from.y);
      ctx.lineTo(to.x, to.y);
      ctx.stroke();
      ctx.closePath();
    }
    def drawZIcon(leftPointPos : Vec,name : String, color : String,  isSelected : Boolean,stachLenght : Int):Double = {
      val displayedName = name
      if(isSelected){
        ctx.font = selectedIconFontEffect+" "+iconFontSize+"px "+selectedIconFont
      }else {
        
       ctx.font = iconFontSize+"px "+iconFont
      }
      val nameMetrics = ctx.measureText(displayedName)
      val boxDim = textGap * 2 + (nameMetrics.width,iconFontSize)
      val boxTopLeft = (leftPointPos) +
        (arrowHeadLength,-arrowBaseHalfWidth) +
        textGap +
        drawingZoneStart
     
      val borderDim = boxDim + (iconBorderWidth,iconBorderWidth)
      val borderTopLeft = boxTopLeft + (iconBorderWidth,iconBorderWidth)/2
      
      val textPos = boxTopLeft + textGap + (0,iconFontSize)
      
      if(isSelected){
        line((0, leftPointPos.y), (on.dimensions.x, leftPointPos.y), 1, color)
        
        drawArrowHead((borderTopLeft.x,leftPointPos.y), (-1,0), color)
        drawArrowHead((borderTopLeft.x + borderDim.x,leftPointPos.y), (1,0), color)
        
      }else {
        if(stachLenght < 0){
          val stachesStart = boxTopLeft + (-defaultStachesLenght,boxDim._2/2);
          val stachesEnd = stachesStart + (boxDim._1 + defaultStachesLenght,0)
          line(stachesStart,stachesEnd, 1, color)
          
        }else{
          val center = boxTopLeft + borderDim/2;
          val stachesStart = center - (stachLenght/2,0)
          val stachesEnd = stachesStart  + (stachLenght,0)
          line(stachesStart,stachesEnd, 1, color)
        }
      }
      ctx.fillStyle = "white"
      ctx.fillRect(boxTopLeft.x, boxTopLeft.y, boxDim.x, boxDim.y)
      ctx.fillStyle = "black"
      ctx.fillText(name, textPos.x, textPos.y)
      ctx.strokeStyle = color
      ctx.lineWidth = iconBorderWidth
      ctx.strokeRect(borderTopLeft.x, borderTopLeft.y, borderDim.x, borderDim.y)
      ctx.fillStyle = "black"
      
      (borderDim.x + (defaultStachesLenght max arrowHeadLength) *2) max stachLenght
      
      
    }
    def drawArrowHead(
      start: (Double, Double),
      dir: (Double, Double),
      color: String): Unit = {
      
      val direction = dir.direction
      ctx.beginPath()
      val pt1 = start + (direction.piRotate * arrowBaseHalfWidth)
      ctx.moveTo(pt1.x, pt1.y)

      val pt2 = start + (direction * arrowHeadLength)
      ctx.lineTo(pt2.x, pt2.y)

      val pt3 = start - (direction.piRotate * arrowBaseHalfWidth)

      ctx.lineTo(pt3.x, pt3.y)
      ctx.fillStyle = color;
      ctx.fill();

      ctx.closePath();

    }
    ctx.fillStyle = "white"
    ctx.fillRect(0, 0, on.dimensions._1, on.dimensions._2)
    
    cm.categories.foldLeft(0.0){
      (cX0, cat)=>
        cat.possibleParts.foldLeft(cX0){
          (pX0, part)=>
            
            val width = part.components.foldLeft(pX0){
              (lX0, layer)=>
                lX0 + drawLayerComponent(part, layer, false, z0, lX0)
            }
            pX0 + drawPartComponent(part, colorFromName(part.partName),width, false, z0, pX0)
        }
    }*/
  }
}