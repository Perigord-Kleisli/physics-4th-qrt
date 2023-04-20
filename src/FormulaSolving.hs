{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
module FormulaSolving where


import Reanimate
import Reanimate.LaTeX
import Reanimate.Scene
import Reanimate.Morph.Common
import Reanimate.Morph.Linear
import Reanimate.Morph.Rotational
import Control.Lens
import Control.Monad
import Graphics.SvgTree
import qualified Data.Text as T
import Data.List
import Data.Maybe
import Data.Char
import Codec.Picture.Types

import Debug.Trace (trace)
import Reanimate.Transition

data GroupKind = Old | New
    deriving (Eq, Show)

data GroupedTex = Sep GroupKind
                | NormalTex String
                deriving Show

quickRender = False

dbg x = trace (show x) x

strokeByFill = mapTree (\t -> set strokeColor (t^.fillColor) t)

nonIdentChars :: String
nonIdentChars = "()$.|/!"

colorGroupedTex' :: ([([Char], Int)], [[([Char], Int)]]) -> String -> (Int, String)
colorGroupedTex' (vs, ps)    ('$':s)           = (max n (length vs + 1), src)
    where (ident, s') = break (== '.') s
          (n, rest)   = colorGroupedTex' ((ident, length vs) : vs, ps) (tail s')
          src         = "\\textcolor{col" ++ show (length vs) ++ "}{\\lambda " ++ ident ++ ".}" ++ rest

colorGroupedTex' (vs, ps)    ('(':s)           = (max n (length vs + 1), src)
    where (n, rest) = colorGroupedTex' (("", length vs) : vs, vs : ps) s
          src       = "\\textcolor{col" ++ show (length vs) ++ "}{(}" ++ rest

colorGroupedTex' (_,  vs:ps) (')':s)           = (n, src)
    where (n, rest) = colorGroupedTex' (vs, ps) s
          src       = "\\textcolor{col" ++ show (length vs) ++ "}{)}" ++ rest

colorGroupedTex' (vs, [])    ""                = (length vs, "")

colorGroupedTex' (vs, ps)    (c:s) | isSpace c = (n, '\\' : c : src)
    where (n, src) = colorGroupedTex' (vs, ps) s

colorGroupedTex' (vs, ps)    (c:s) | c `elem` nonIdentChars = (n, c : src)
    where (n, src) = colorGroupedTex' (vs, ps) s

colorGroupedTex' (vs, ps)    s                 = (n, src)
    where (ident, s') = break (\c -> isSpace c || c `elem` nonIdentChars) s
          (n, rest)   = colorGroupedTex' (vs, ps) s'
          src         = case lookup ident vs of
                          Nothing -> ident ++ rest
                          Just x  -> "\\textcolor{col" ++ show x ++ "}{" ++ ident ++ "}" ++ rest

colorGroupedTex = colorGroupedTex' ([], [])

parseToGroupedTex' ('|':s) = Sep New : parseToGroupedTex' s
parseToGroupedTex' ('/':s) = Sep Old : parseToGroupedTex' s
parseToGroupedTex' ('!':s) = Sep Old : Sep New : parseToGroupedTex' s
parseToGroupedTex' (c:s)   = NormalTex [c] : parseToGroupedTex' s
parseToGroupedTex' ""      = []

simplifyGroupedTex (NormalTex l : NormalTex r : rest) = simplifyGroupedTex (NormalTex (l ++ r) : rest)
simplifyGroupedTex (x : rest) = x : simplifyGroupedTex rest
simplifyGroupedTex [] = []

parseToGroupedTex s = (n, simplifyGroupedTex $ parseToGroupedTex' $ colored)
    where (n, colored) = colorGroupedTex s

getGroups kind (Sep here    : rest) | kind == here = [] : getGroups kind rest
getGroups kind (Sep _       : rest) | otherwise    = getGroups kind rest
getGroups kind (NormalTex t : rest)                = case getGroups kind rest of
                                                       [] -> [t]
                                                       x:xs -> (t ++ x) : xs
getGroups kind []                                  = []

getFullSource l = l >>= \x -> case x of
                                Sep _       -> []
                                NormalTex t -> t

prefixes kind grouped = case scanl (++) "" (getGroups kind grouped) of
                          [""] -> [""]
                          "" : out -> out

intColor c k n = c $ 1 - k ^ n

gtlConf n = TexConfig
    { texConfigEngine = LaTeX
    , texConfigHeaders = ["\\usepackage{helvet}", "\\usepackage{xcolor}"] ++ colors
    , texConfigPostScript = []
    }
        where colors = map (T.pack . colorByNum) [0..n]
              colorByNum n' = "\\definecolor{col" ++ show n' ++ "}{RGB}{" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ "}"
                  where (PixelRGB8 r g b) = viridis (fromIntegral n' / fromIntegral n)

renderSub n s = scale 0.5 $ pathify $ lowerTransformations $ center $ simplify $ latexCfg (gtlConf n) s
renderLaTeX n s = renderSub n $ T.unlines ["\\begin{align*}", T.pack s, "\\end{align*}"]
renderText = renderSub (-1) . T.pack

withGlyphIndices kind (n, grouped) = (map (length . svgGlyphs) rendered, last rendered)
    where rendered = map (renderLaTeX n . ("\\color{white}" ++)) (prefixes kind grouped)

viewGtl :: (Int, [GroupedTex]) -> SVG
viewGtl (n, s) = renderLaTeX n $ "\\color{white}" ++ getFullSource s

-- write :: Duration -> SVG -> 

data WriteConfig = WriteConfig
    { tempStrokeWidth :: Double
    , stagger :: Duration
    , strokeDur :: Duration
    , strokeDecayDelay :: Duration
    , strokeDecayDur :: Duration
    , fillDelay :: Duration
    , fillDur :: Duration
    , mapGlyphs :: [(SVG -> SVG, DrawAttributes, SVG)] -> [(SVG -> SVG, DrawAttributes, SVG)]
    }

write' :: WriteConfig -> SVG -> Animation
write' cfg svg = chainT (overlapT 0.5 fadeT) $ map forGlyph $ zip [0..] $ mapGlyphs cfg $ svgGlyphs $ strokeByFill svg
    where forGlyph (n, (ctx, attr, node)) = pauseAtBeginning (n * stagger cfg) $ parA strokeGlyph $ pauseAtBeginning (fillDelay cfg) fillGlyph
              where strokeGlyph      = mkAnimation strokeTime $ \t ->
                                         ctx $ partialSvg (strokeProgress t) $ withFillOpacity 0 $ withStrokeWidth (decayedSWidth t) node
                    fillGlyph        = mkAnimation (fillDur cfg) (\t -> ctx $ withFillOpacity t $ withStrokeWidth 0 node)
                    strokeTime       = max (strokeDur cfg) (strokeDecayDelay cfg + strokeDecayDur cfg)
                    clamp minV maxV  = min maxV . max minV
                    decayedSWidth t  = clamp 0 1 (1 - (t * strokeTime - strokeDecayDelay cfg) / strokeDecayDur cfg) * tempStrokeWidth cfg
                    strokeProgress t = clamp 0 1 (t * strokeTime / strokeDur cfg)

defaultConfig sw stagger glyphTime = WriteConfig
    { tempStrokeWidth = sw
    , stagger = stagger
    , strokeDur = glyphTime * 0.75
    , strokeDecayDelay = glyphTime * 0.5
    , strokeDecayDur = glyphTime * 0.5
    , fillDelay = glyphTime * 0.25
    , fillDur = glyphTime * 0.75
    , mapGlyphs = id
    }

defaultestConfig = defaultConfig defaultStrokeWidth 0.05 0.6

write = if quickRender then staticFrame 1 else setDuration 1 . write' defaultestConfig
unwrite' cfg svg = reverseA $ write' cfg' svg
    where cfg' = cfg { mapGlyphs = reverse . mapGlyphs cfg }
unwrite = if quickRender then staticFrame 1 else setDuration 1 . unwrite' defaultestConfig

writeGtl = write . viewGtl
unwriteGtl = unwrite . viewGtl

transformGtl :: Duration -> (Int, [GroupedTex]) -> (Int, [GroupedTex]) -> Animation
transformGtl dur l r = mkAnimation dur forTime
    where (glyphIndicesL, fullL)        = withGlyphIndices New l
          (glyphIndicesR, fullR)        = withGlyphIndices Old r
          len                           = max (length glyphIndicesL) (length glyphIndicesR)
          window l full                 = map (windowInner full) $ take (len + 1) $ (zip (0:(l ++ repeat (last l))) (l ++ repeat (last l)))
          windowInner full (start, end) = if start == end then Nothing else (Just $ snd $ splitGlyphs [start..end - 1] full)
          forTime t                     = mkGroup $ map forGroup $ zip (window glyphIndicesL fullL) (window glyphIndicesR fullR)
              where forGroup (Nothing, Nothing) = mkGroup []
                    forGroup (Just l,  Nothing) = frameIn $ unwrite l
                    forGroup (Nothing, Just r)  = frameIn $ write r
                    forGroup (Just l,  Just r)  = morph linear { morphTrajectory=rotationalTrajectory (-1, -1) } l r (curveS 2 t)
                    frameIn anim                = frameAt (t * duration anim) anim

simplifyExprs' :: Duration -> Duration -> [String] -> Animation
simplifyExprs' transDur pauseDur l = (foldl' seqA initial transitions) `seqA` final
    where initial     = pauseAtEnd pauseDur $ setDuration transDur $ writeGtl $ head l'
          transitions = map (pauseAtEnd pauseDur . uncurry (transformGtl transDur)) (zip l' (tail l'))
          final       = setDuration transDur $ unwriteGtl $ last l'
          l'          = map parseToGroupedTex l

simplifyExprs transDur pauseDur l =
    if quickRender
       then foldl' seqA (pause transDur) (map (staticFrame (transDur + pauseDur) . viewGtl . parseToGroupedTex) l)
       else simplifyExprs' transDur pauseDur l

lineHeight = 1.2 * (svgHeight $ viewGtl $ parseToGroupedTex "($x. x)")

newText :: Double -> Double -> String -> SVG
newText s y = withFillColor "white" . translate 0 y . scale s . renderText

introduction = waitOn $ do
    heading <- oNew $ newText 1 0 "\\textbf{Lambda calculus is broadly built on only these two laws:}"
    oShowWith heading write
    wait 2
    functionText1 <- oNew $ newText 1 2 "Functions are denoted by a $\\color{orange} \\lambda$,"
    functionText2 <- oNew $ newText 1 1.5 "followed by a variable, a dot, and the body"
    let fnGtl = parseToGroupedTex "|$x. a x b x c|"
    let exampleCfg = translate 0 (-5 * lineHeight) . scale 2
    exampleFn <- oNew $ exampleCfg $ viewGtl $ fnGtl
    fork $ wait 0.75 *> oShowWith functionText1 write
    fork $ wait 1    *> oShowWith functionText2 write
    fork $ wait 1.25 *> oShowWith exampleFn write
    oTween heading 1.5 (\t -> oTranslateY %~ (+ 5 * lineHeight * curveS 2 t))
    wait 2
    let apGtl1 = parseToGroupedTex "(/$x. |a |x| b |x| c!) y"
    let apGtl2 = parseToGroupedTex "/a /y/ b /y/ c/"
    oHide exampleFn
    apText1 <- oNew $ newText 1 0.5  "Function applications are denoted by a space: $\\color{orange}f\\ \\color{yellow}x$"
    apText2 <- oNew $ newText 1 0    "where $\\color{orange}f$ gets applied to $\\color{yellow}x$"
    apText3 <- oNew $ newText 1 (-1) "To do this, all occurences of the inner variable are replaced:"
    fork $ wait 0    *> oShowWith apText1 write
    fork $ wait 0.5  *> oShowWith apText2 write
    fork $ wait 2    *> oShowWith apText3 write
    play $ pauseAtEnd 3 $ mapA exampleCfg $ transformGtl 1.5 fnGtl apGtl1
    play $ pauseAtEnd 2 $ mapA exampleCfg $ transformGtl 1 apGtl1 apGtl2
    fork $ wait 0    *> oHideWith heading       unwrite
    fork $ wait 0.25 *> oHideWith functionText1 unwrite
    fork $ wait 0.5  *> oHideWith functionText2 unwrite
    fork $ wait 0.75 *> oHideWith apText1       unwrite
    fork $ wait 1    *> oHideWith apText2       unwrite
    fork $ wait 1.25 *> oHideWith apText3       unwrite
    play $ pauseAtBeginning 1.5 $ mapA exampleCfg $ unwriteGtl apGtl2
    enough <- oNew $ newText 2 0 "\\begin{center}\nThis is actually enough to compute\n\n\\textit{\\textbf{everything}}\n\nthat can be computed!\n\\end{center}"
    oShowWith enough write *> wait 1 *> oHideWith enough unwrite
    ieNumbers <- oNew $ newText 2 0 "\\begin{center}\nSo to start out, let's take a look\n\nat how one might calculate\n\nthings with numbers.\n\\end{center}"
    oShowWith ieNumbers write *> wait 1 *> oHideWith ieNumbers unwrite

numbers = waitOn $ do
    let numbers =
            [ "|0| = $f. $x. ||x"
            , "!1! = $f. $x. /f /||x|"
            , "!2! = $f. $x. f /(f !|x!|)"
            , "!3! = $f. $x. f (f /(f !|x!|)/)"
            , "!4! = $f. $x. f (f (f /(f !|x!|)/))"
            ]
    let plusOne = 
            [ "+1 = $n. $f. $x. n f (f x)"
            , "+1| = $n. $f. $x. n f (f x)"
            , "+1! 2"
            , "($n. |$f. $x. |n| f (f x)|)/ 2"
            , "/$f. $x. !2! f (f x)/"
            , "$f. $x. /(|$f. |$x. f (f x))/ |f |(f x)"
            , "$f. $x. |(//$x. |f (f |x|)|) //(f x)"
            , "$f. $x. //f (f /(f x)/)"
            , "3"
            ]
    let add1 =
            [ "+ = $a. $b. $f. $x. a f (b f x)"
            , "+| = $a. $b. $f. $x. a f (b f x)"
            , "+! 1 2"
            , "($a. $b. |$f. $x. |a| f (|b| f x)|)/ 1 2"
            , "/$f. $x. !1/ f (/2/ f| x|)/"
            , "$f. $x. !3| f/ x/"
            , "/3/"
            ]
    let add2 = scale 2 $ viewGtl $ parseToGroupedTex "+ = $a. $b. a (+1) b"
    let canYouGuess = newText 1 (-1.5) "\\textit{Can you guess how it works?}"
    let withHeading t i = do
        heading <- oNew $ newText 1 2 t
        oShowWith heading write
        i
        oHideWith heading unwrite
    let playSimplify = play . mapA (translate 0 (-1.5 * lineHeight) . scale 2) . simplifyExprs 1 1
    withHeading "\\begin{center}\\textbf{In the Church-Encoding,}\n\nthe number $n$ applies its first argument $n$ times to its second one:\\end{center}" $ do
        playSimplify numbers
    withHeading "The following function adds $1$ to a given number:" $ do
        playSimplify plusOne
    withHeading "We can also add numbers:" $ do
        playSimplify add1
    withHeading "\\begin{center}\nThis is another way to add numbers, to give you a taste of\n\nhow this concept of $\\lambda$-functions is incredibly powerful:\\end{center}" $ do
        fork $ play $ pauseAtEnd 3 $ write add2
        wait 1
        play $ (pauseAtEnd 1 $ write canYouGuess) `seqA` unwrite canYouGuess
        play $ unwrite add2

sample :: IO ()
sample = reanimate $ addStatic (mkBackground "black") $ scene $ do
    introduction
    wait 1
    numbers
