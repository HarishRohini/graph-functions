import Graphics.Blank
import Control.Applicative

message :: Float -> Float -> Canvas ()
message msg msg2= do
        (width,height) <- size
        font "30pt Calibri"
        textAlign "left"
        fillStyle "#8090a0"
        fillText(show(msg) ++ " " ++ show(msg2), 10, height - 10)


positivex :: Int -> Float -> Canvas()
positivex n x = 
        if n<6 then
                do
                (width,height) <- size
                moveTo(x+100,height/20*10-10)
                lineTo(x+100,height/20*10+10)
                lineWidth 5
                strokeStyle "black"
                stroke()
                font "20pt Calibri"
                fillStyle "#8090a0"
                fillText(show(n),x+100,height/20*10+30)
                positivex (n+1) (x+100)
        else
                do
                (width,height) <- size
                fillStyle "#FFFFFF"
                --fillText("",x+100,height/20*10+30)


positivey :: Int -> Float -> Canvas()
positivey n y = 
        if n<6 then
                do
                (width,height) <- size
                moveTo(width/20*10-10,y-100)
                lineTo(width/20*10+10,y-100)
                lineWidth 5
                strokeStyle "black"
                stroke()
                font "20pt Calibri"
                fillStyle "#8090a0"
                fillText(show(n),width/20*10+20,y-100)
                positivey (n+1) (y-100)
        else
                do
                (width,height) <- size
                fillStyle "#FFFFFF"



negativex :: Int -> Float -> Canvas()
negativex n x = 
        if n<6 then
                do
                (width,height) <- size
                moveTo(x-100,height/20*10-10)
                lineTo(x-100,height/20*10+10)
                lineWidth 5
                strokeStyle "black"
                stroke()
                font "20pt Calibri"
                fillStyle "#8090a0"
                fillText("-"++show(n),x-100,height/20*10+30)
                negativex (n+1) (x-100)
        else
                do
                (width,height) <- size
                fillStyle "#FFFFFF"




negativey :: Int -> Float -> Canvas()
negativey n y = 
        if n<6 then
                do
                (width,height) <- size
                moveTo(width/20*10-10,y+100)
                lineTo(width/20*10+10,y+100)
                lineWidth 5
                strokeStyle "black"
                stroke()
                font "20pt Calibri"
                fillStyle "#8090a0"
                fillText("-"++show(n),width/20*10+20,y+100)
                negativey (n+1) (y+100)
        else
                do
                (width,height) <- size
                fillStyle "#FFFFFF"



{-
drawq :: Context -> IO()
drawq context = send context $ do

                        (width,height) <- size
                        --print width
                        moveTo(width/20*10,0)
                        lineTo(width/20*10,height)
                        lineWidth 8
                        strokeStyle "black"
                        stroke()

                        moveTo(0,height/20*10)
                        lineTo(width,height/20*10)
                        lineWidth 8
                        strokeStyle "black"
                        stroke()

                        message width height

                        let x = 0

                        let originx = width/20*10
                        let originy = height/20*10

                        moveTo(x,0)
                        lineTo(50,50)
                        lineWidth 8
                        strokeStyle "black"
                        stroke()

                        positivex 1 originx
                        positivey 1 originy
                        negativex 1 originx
                        negativey 1 originy

                        --plot 2 2



-}



drawquadrant :: Canvas()
drawquadrant = do
        (width,height) <- size
        --print width
        moveTo(width/20*10,0)
        lineTo(width/20*10,height)
        lineWidth 8
        strokeStyle "black"
        stroke()

        moveTo(0,height/20*10)
        lineTo(width,height/20*10)
        lineWidth 8
        strokeStyle "black"
        stroke()

        message width height

        let x = 0

        let originx = width/20*10
        let originy = height/20*10

        moveTo(x,0)
        lineTo(50,50)
        lineWidth 8
        strokeStyle "black"
        stroke()

        positivex 1 originx
        positivey 1 originy
        negativex 1 originx
        negativey 1 originy



--plot :: (Float,Float) -> [(Float,Float)] -> Canvas()
plot :: (Float,Float) -> (Float,Float) -> Canvas()
--plot (x1,y1) ((x2,y2):xs) = do
plot (x1,y1) (x2,y2) = do
        (width,height) <- size
        --(x2,y2) <- x
        moveTo((width/20*10) + (x1*100), (height/20*10) - (y1*100))
        lineTo((width/20*10) + (x2*100), (height/20*10) - (y2*100))
        lineWidth 8
        strokeStyle "black"
        stroke()




sendpoints :: [(Float,Float)] -> Canvas()
sendpoints (x:xs) = do
        if length(xs) > 2 then
                do
                let y = head(xs)
                plot x y
                sendpoints xs
                fillStyle "#FFFFFF"
        else
                fillStyle "#FFFFFF"



plotgraph :: Float -> Float -> Canvas()
plotgraph m c = do
        (width,height) <- size
        let xco = [-2.01,-2.00..2]
        --let xco2 = -1

        --let yco1 = m * xco1 + c
        let yco = [m*x + c | x <- [-2.01,-2.00..2]]
        --let yco2 = m * xco2 + c
        let cords = zip xco yco
        sendpoints cords

        --moveTo((width/20*10) + (xco1*100), (height/20*10) - (yco1*100))
        --lineTo((width/20*10) + (xco2*100), (height/20*10) - (yco2*100))
        --lineWidth 8
        --strokeStyle "black"
        --stroke()



plotgraphsin :: Canvas()
plotgraphsin = do
        let xco = [-5.01,-5.00..5]
        let yco = [sin(x) | x <- [-5.01,-5.00..5]]
        let cords = zip xco yco
        sendpoints cords


plotgraphcos :: Canvas()
plotgraphcos = do
        let xco = [-5.01,-5.00..5]
        let yco = [ cos(x)  | x <- [-5.01,-5.00..5]]
        let cords = zip xco yco
        sendpoints cords

plotgraphtan :: Canvas()
plotgraphtan = do
        let xco = [-5.01,-5.00..5]
        let yco = [ tan(x)  | x <- [-5.01,-5.00..5]]
        let cords = zip xco yco
        sendpoints cords

plotgraphparab :: Canvas()
plotgraphparab = do
        let xco = [-5.01,-5.00..5]
        let yco = [ x^2  | x <- [-5.01,-5.00..5]]
        let cords = zip xco yco
        sendpoints cords


plotgraphhyp :: Canvas()
plotgraphhyp = do
        let xco = [-5.01,-5.00..5]
        let yco = [ sqrt(1+(x^2))  | x <- [-5.01,-5.00..5]]
        let cords = zip xco yco
        sendpoints cords


plotgraphelip :: Canvas()
plotgraphelip = do
        let xco = [-5.01,-5.00..5]
        let yco = [ sqrt(4-((x/3)^2))  | x <- [-5.01,-5.00..5]]
        let cords = zip xco yco
        sendpoints cords

plotgraphquad :: Canvas()
plotgraphquad = do
        let xco = [-5.01,-5.00..5]
        let yco = [ -(x^4)+(x^3)+(x^2)+(x)+1  | x <- [-5.01,-5.00..5]]
        let cords = zip xco yco
        sendpoints cords






plotgraphtriquad :: Canvas()
plotgraphtriquad = do
        let xco = [-5.01,-5.00..5]
        let yco = [ ((x^2)*sin(x))+(x*cos(x)) | x <- [-5.01,-5.00..5]]
        let cords = zip xco yco
        sendpoints cords

plotmain1 :: Context -> IO()
plotmain1 context = send context $ do
        drawquadrant
        plotgraphsin



plotmain2 :: Context -> IO()
plotmain2 context = send context $ do
        drawquadrant
        plotgraphcos
        
plotmain3 :: Context -> IO()
plotmain3 context = send context $ do
        drawquadrant
        plotgraphtan                    

plotmain4 :: Context -> IO()
plotmain4 context = send context $ do
        drawquadrant
        plotgraphparab

plotmain5 :: Context -> IO()
plotmain5 context = send context $ do
        drawquadrant
        plotgraphhyp

plotmain6 :: Context -> IO()
plotmain6 context = send context $ do
        drawquadrant
        plotgraphelip
plotmain7 :: Context -> IO()
plotmain7 context = send context $ do
        drawquadrant
        plotgraphquad

plotmain8 :: Context -> IO()
plotmain8 context = send context $ do
        drawquadrant
        plotgraphtriquad


main = do
        putStrLn "Enter 1 to plot a Sine Curve"
        putStrLn "Enter 2 to plot a Cosine Curve"
        putStrLn "Enter 3 to plot a Tangent Curve"
        putStrLn "Enter 4 to plot an Parabola"
        putStrLn "Enter 5 to plot an Hyperbola"
        putStrLn "Enter 6 to plot an Ellipse"
        putStrLn "Enter 7 to plot an Quadratic eq of Degree 4"
        putStrLn "Enter 8 to plot x2*sinx+xcosx"
        putStrLn "Enter your choice now"
        z <- getLine
        putStrLn "Hang on while plot is generated"
        case z of
                "1" -> blankCanvas 3000 plotmain1
                "2" -> blankCanvas 3000 plotmain2
                "3" -> blankCanvas 3000 plotmain3
                "4" -> blankCanvas 3000 plotmain4
                "5" -> blankCanvas 3000 plotmain5
                "6" -> blankCanvas 3000 plotmain6
                "7" -> blankCanvas 3000 plotmain7
                "8" -> blankCanvas 3000 plotmain8








