-- Nome: Giordana Camargo

import Text.Printf

type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Circle    = (Point,Float)

-------------------------------------------------------------------------------
-- Paletas
-------------------------------------------------------------------------------

-- Paleta (R, G, B) só com tons de verde
-- Gera n tons de verde
greenPalette :: Int -> [(Int,Int,Int)]
greenPalette n = [(r,g,b) | r<-[0], g<-take n (cycle[255,245..10]), b<-[0]]

-- Gera n tons de azul
bluePalette :: Int -> [(Int,Int,Int)]
bluePalette n = [(r,g,b) | r<-[0], g<-[0], b<-take n (cycle[255,245..10])]

-- Gera n tons de vermelho
redPalette :: Int -> [(Int,Int,Int)]
redPalette n = [(r,g,b) | r<-take n(cycle[255,245..10]), g<-[0], b<-[0]]


-- Paleta com n valores retirados de uma lista com sequências de R, G e B 
-- Cria uma cor aleatória
rgbPalette :: Int -> [(Int,Int,Int)]
rgbPalette n = take n $ cycle [(255,0,0),(0,255,0),(0,0,255)]

-- Cria uma cores aleatórias quentes no meio
rgbPaletteHot :: Int -> [(Int,Int,Int)]
rgbPaletteHot n = [(r,g,b) | r<-[255,240..0], g<-[0,15..255], b<-[0,15..255]]

-- Cria uma cores aleatórias frias no meio
rgbPaletteForest :: Int -> [(Int,Int,Int)]
rgbPaletteForest n = [(r,g,b) | r<-[0,15..255], g<-[0,15..255], b<-[0,15..255]]



-------------------------------------------------------------------------------
-- Geração de espiral na tela
-------------------------------------------------------------------------------


circleCircle2 :: Float -> Float ->Float ->Float -> Float-> [Circle]
circleCircle2 c ch rd rt rdd = [((centerX + cos (theta + rotation)*(awayStep * theta),centerY + sin (theta + rotation)*(awayStep * theta)), r)| theta <- [chord/awayStep,(chord/awayStep+1)..(thetaMax)]]
    where (centerX,centerY) = (750,750)
          thetaMax = coils * 2*pi
          coils = c -- número de "ramificações"
          awayStep = radius / thetaMax
          chord = ch
          radius = rd -- raio da espiral
          rotation = rt * pi -- quantas voltas dá
          r = rdd -- tamanho das bolinhas


-------------------------------------------------------------------------------
-- Strings SVG
-------------------------------------------------------------------------------


-- Gera string representando círculo SVG 

svgCircle :: Circle -> String -> String 
svgCircle ((x,y),r) style = 
  printf "<circle cx='%.3f' cy='%.3f' r='%.2f' style='%s' />\n" x y r style

-- String inicial do SVG
svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"

-- Gera string com atributos de estilo para uma dada cor
-- Atributo mix-blend-mode permite misturar cores
svgStyle :: (Int,Int,Int) -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: screen;" r g b

-- Gera strings SVG para uma dada lista de figuras e seus atributos de estilo
-- Recebe uma função geradora de strings SVG, uma lista de círculos/retângulos e strings de estilo
svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles

-------------------------------------------------------------------------------
-- Função principal que gera arquivo com imagem SVG
-------------------------------------------------------------------------------

main :: IO ()
main = do
  writeFile "espiral.svg" $ svgstrs
  where svgstrs = svgBegin w h ++ svgfigs ++ svgEnd
        
        svgfigs = svgElements svgCircle circs (map svgStyle palette)

        --  circleCircle2 (número_de_"ramificações", espaçamento_do_meio, raio_da_espiral,n_de_rotações, tam_das_bolinhas)
        circs =  circleCircle2 180 34 900 10 15
        
        palette = redPalette 2000
        
        (w,h) = (1500,1500) -- width,height da imagem SVG