// const WORLD_WIDTH = 2000
//const WORLD_HEIGHT = 2000
const mydata = JSON.parse(data)
//    alert(mydata[0].points[0].name)
const theHeight = window.innerHeight
const theWidth = window.innerWidth

const app = new PIXI.Application(
    {antialias: true,
     width: theWidth,
     height: theHeight,
     backgroundColor: 0xffffff,
     resolution: window.devicePixelRatio || 1,})
document.body.appendChild(app.view)

// create viewport
const viewport = new pixi_viewport.Viewport({
    screenWidth: window.innerWidth,
    screenHeight: window.innerHeight,
    //  worldWidth: WORLD_WIDTH,                        // world width used by viewport (automatically calculated based on container width)
    //  worldHeight: WORLD_HEIGHT,
    passiveWheel: false,
    interaction: app.renderer.plugins.interaction // the interaction module is important for wheel to work properly when renderer.view is placed qor scaled
})

// add the viewport to the stage
app.stage.addChild(viewport)
// activate plugins
viewport
    .drag()
    .pinch()
    .wheel()
    .decelerate()

const graphics = viewport.addChild(new PIXI.Graphics());
const points = mydata.points
deltaHeight = theHeight/points.length
let deltaWidth = points.map((layer) => theWidth/layer.length)
let  = points.map((layer) => layer.length)
let MaxSizeIndx = points.map(x=>x.length).indexOf(Math.max(...points.map(x=>x.length)));
deltaWidth =  Math.min(...deltaWidth)

const coordinates = new Map()

for (let j =0; j < points.length; j++) {
    for (let i = 0; i < points[j].length; i++) {
        //console.log(points[j][i],"->", points[j][i].x1,points[j][i].y1 )
        coordinates.set(points[j][i].x1+"_"+points[j][i].y1,
                        [deltaWidth*(points[j][i].x1-0.9+ 0.5*Math.abs(MaxSizeIndx - j)),
                         deltaHeight * (points[j][i].y1-0.5)])
    }
}
let maxX = -Infinity
let maxY = -Infinity
let minX = Infinity
let minY = Infinity

for (let j =0; j < points.length; j++) {
    for (let i = 0; i < points[j].length; i++) {
        graphics.lineStyle(0); // draw a circle, set the lineStyle to zero so the circle doesn't have an outline
        graphics.beginFill(0x000000, 1);
        let key = points[j][i].x1+"_"+points[j][i].y1
        //console.log(key);

        maxX = Math.max(maxX, coordinates.get(key)[0])
        maxY = Math.max(maxY, coordinates.get(key)[1])
        minX = Math.min(minX, coordinates.get(key)[0])
        minY = Math.min(minY, coordinates.get(key)[1])

        graphics.drawCircle(coordinates.get(key)[0], coordinates.get(key)[1], 5)
        let text = new PIXI.Text(points[j][i].name, {fontFamily : 'Arial', fontSize: 22, fill : 0x000000, align : 'center'});
        graphics.addChild(text)
        text.position = { x : coordinates.get(key)[0]-15, y: coordinates.get(key)[1]-30},
        graphics.endFill();
    }
}

const lines = mydata.lines
const  lineGraph = new Array(lines.length)


for (let j =0; j < lines.length; j++) {

    lineGraph[j] = new PIXI.Graphics()
    lineGraph[j].lineStyle(2, 0x000000)
    let key = lines[j].x1+"_"+lines[j].y1
    let key2 = lines[j].x2+"_"+lines[j].y2

    lineGraph[j].moveTo(coordinates.get(key)[0], coordinates.get(key)[1]);
    lineGraph[j].lineTo(coordinates.get(key2)[0], coordinates.get(key2)[1]);
    graphics.addChild(lineGraph[j])
    graphics.beginFill(0x000000, 1)
    graphics.endFill();
}

//console.log(viewport.worldWidth)
// console.log(minX, maxX, minY, maxY)
// console.log(viewport.findCover())
viewport.ensureVisible(minX-50,minY-30, maxX-minX+50, maxY-minY+80, true)

function getPicture(){
let win = window.open(app.renderer.plugins.extract.base64(app.stage), '_blank');
    win.focus()}
