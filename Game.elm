module ZzZz where

import Window
import Signal
import Keyboard
import Svg exposing (..)
import Svg.Attributes as SVGA
import Html exposing (Html)
import Time exposing (..)
import List
import String
import Random
import Keyboard
import Color exposing (toRgb, toHsl, hsl, Color)

-- CONSTANTS
gameWidth = 400
gameHeight = 300
melatoninRadius = 10
objectDropGenerator = Random.int melatoninRadius (gameWidth-melatoninRadius)

type alias Mode =
  { startLightSourcesAt: Float
  , lightSourceDelay: Time
  , melatoninDelay: Time
  , gameDuration: Time
  , melatoninScore: Int
  , lightSourceScore: Int
  , scoreTarget: Int
  }

baseMode : Mode
baseMode =
  { melatoninDelay = 1.5*second
  , lightSourceDelay = 2*second
  , gameDuration = 2*minute
  , scoreTarget = 100
  , melatoninScore = 1
  , lightSourceScore = -3
  , startLightSourcesAt = 0.3
  }

main = 
  Signal.map2 view Window.dimensions game

game : Signal Game
game = Signal.foldp updateGame defaultGame input

type alias Input =
  { x: Int
  , space: Bool
  }

input : Signal (Time, Input)
input = 
    Time.timestamp (
      Signal.sampleOn (fps 60) <|
        Signal.map2 Input
          (Signal.map .x Keyboard.arrows)
          Keyboard.space
    )

-- UPDATE

updateGame : (Time,Input) -> Game -> Game
updateGame (t,input) game =
  let
      spacePressed = ( (not game.space) && input.space )
      nextGame = case game.scene of
        PlayScreen -> updatePlay t input game
        SelectionScreen -> updateSelection t input game
        InformationScreen -> if spacePressed then startGame t game else game
        _ -> if spacePressed then resetGame game else game
  in { nextGame | space = input.space }

updatePlay : Time -> Input -> Game -> Game
updatePlay t input game = game
  |> updatePlayer input.x
  |> updateMelatonins
  |> dropMelatonins t
  |> dropLightSources t
  |> captureMelatonins
  |> updateClock t
  |> finishIfEnded

finishIfEnded : Game -> Game
finishIfEnded game = 
  if game.score >= game.mode.scoreTarget then
    { game | scene = SuccessScreen, space = False }
  else if game.timeProgress >= 1 then
    { game | scene = FailureScreen, space = False }
  else
    game

updateClock : Time -> Game -> Game
updateClock t game = { game | timeProgress = (t-game.startedAt)/game.mode.gameDuration }

updateSelection : Time -> Input -> Game -> Game
updateSelection time input game =
  if (not game.space) && input.space then
    { game | scene = InformationScreen }
  else
    (case input.x of
      1 -> { game | player = newPlayer Male }
      (-1) -> { game | player = newPlayer Female }
      _ -> game
    )

movePlayer : Float -> Player -> Player
movePlayer dx player =
  let newX = player.x+dx
  in
      if newX > gameWidth then
        { player | x = newX-gameWidth }
      else if newX < 0 then
        { player | x = newX+gameWidth }
      else 
        { player | x = newX }

dropMelatonins : Time -> Game -> Game
dropMelatonins t game =
  if (t-game.dropMelatoninAt) >= 0 then
    case Random.generate objectDropGenerator game.melatoninSeed of
      (nextX,nextSeed) ->
        { game
        | melatoninSeed = nextSeed
        , melatonins = (Melatonin (nextX,0)) :: game.melatonins
        , dropMelatoninAt = t+game.mode.melatoninDelay*(1-game.timeProgress)
        }
  else game


dropLightSources : Time -> Game -> Game
dropLightSources t game =
  if (t-game.dropLightSourceAt) >= 0 then
    case Random.generate objectDropGenerator game.lightSourceSeed of
      (nextX,nextSeed) ->
        { game
        | lightSourceSeed = nextSeed
        , melatonins = (LightSource (nextX,0)) :: game.melatonins
        , dropLightSourceAt = t+game.mode.lightSourceDelay*(1-game.timeProgress)
        }
  else game

updatePlayer : Int -> Game -> Game
updatePlayer dx game =
  let
      speed = 1+3*(1.0-(toFloat game.score)/(toFloat game.mode.scoreTarget))
      player = movePlayer ((toFloat dx)*speed) game.player
  in
      { game | player = player }

updateMelatonins : Game -> Game
updateMelatonins game =
  let
      updateMelatonine object = case object of
        Melatonin (x,y) -> Melatonin (x,y+3)
        LightSource (x,y) -> LightSource (x,y+3)

      passed (x,y) = y < gameHeight

      melatonins = game.melatonins
        |> List.map updateMelatonine
        |> List.filter (objectPosition >> passed)
  in
      { game | melatonins = melatonins }

isCollidingWith : Player -> Point -> Bool
isCollidingWith player (x,y) =
  let
      checkX px ox = (toFloat ox + 5 > px) && ( toFloat ox < px+40 )
  in
      if y < 225 then
        False
      else if (checkX player.x x) || (checkX (player.x - gameWidth) x) then
        True
      else
        False

captureMelatonins : Game -> Game
captureMelatonins game =
  let
      isColliding = objectPosition >> (isCollidingWith game.player)
      isNotColliding = not << isColliding
      scoreForObject object = case object of
        LightSource _ -> -3
        Melatonin   _ -> 1

      scored = game.melatonins
        |> List.filter isColliding
        |> List.map scoreForObject
        |> List.sum

      newMelatonins = game.melatonins
        |> List.filter isNotColliding

      newScore' = game.score+scored
      newScore = if newScore' < 0 then 0 else newScore'
  in
      { game | melatonins = newMelatonins, score = newScore }

-- MODEL
type alias Point = (Int,Int)

type Sex = Male | Female
type Object = Melatonin Point | LightSource Point
type Scene = SelectionScreen | PlayScreen | SuccessScreen | FailureScreen | InformationScreen

objectPosition : Object -> Point
objectPosition object = case object of
  Melatonin p -> p
  LightSource p -> p

type alias Player =
  { sex: Sex
  , x: Float
  , y: Float
  }

newPlayer : Sex -> Player
newPlayer sex =
  { sex = sex
  , x = (gameWidth/2)-35
  , y = 10
  }

type alias Game = 
  { player: Player
  , melatonins: List Object
  , score: Int
  , scene: Scene
  , melatoninSeed: Random.Seed
  , dropMelatoninAt: Time
  , lightSourceSeed: Random.Seed
  , dropLightSourceAt: Time
  , startedAt: Time
  , timeProgress: Float
  , space: Bool
  , mode: Mode
  }

defaultGame : Game
defaultGame =
  { player = newPlayer Male
  , melatonins = []
  , score = 0
  , scene = SelectionScreen
  , melatoninSeed = Random.initialSeed 1234
  , dropMelatoninAt = 0
  , lightSourceSeed = Random.initialSeed 5678
  , dropLightSourceAt = 0
  , startedAt = 0
  , timeProgress = 0
  , space = False
  , mode = baseMode
  }


startGame : Time -> Game -> Game
startGame t game =
  { game
  | scene = PlayScreen
  , startedAt = t
  , dropLightSourceAt = t+game.mode.startLightSourcesAt*game.mode.gameDuration
  }

resetGame : Game -> Game
resetGame game =
  { game
  | score = 0
  , scene = SelectionScreen
  , timeProgress = 0
  , melatonins = []
  }

-- VIEW

view : (Int,Int) -> Game -> Html
view (w,h) game = svg
  [ SVGA.version "1.1"
  , SVGA.x "0"
  , SVGA.y "0"
  , SVGA.viewBox (String.join " "
    [ "0"
    , "0"
    , gameWidth |> toString
    , gameHeight |> toString
    ]
  )
  , SVGA.preserveAspectRatio "xMidYMin meet"
  , SVGA.width (toString w)
  , SVGA.height (toString h)
  ] 
  (case game.scene of
    PlayScreen -> renderPlay game
    SelectionScreen -> renderSelection game
    SuccessScreen -> renderSuccess game
    FailureScreen -> renderFailure game
    InformationScreen -> renderInformation game
  )

-- View utils

bgAttrs : List Attribute -> List Attribute
bgAttrs attrs = (List.concat [
    [ SVGA.x "0"
    , SVGA.y "0"
    , SVGA.height (gameHeight |> toString)
    , SVGA.width (gameWidth |> toString)
    ],
    attrs
  ])

colorToString : Color -> String
colorToString color = 
  let c = toRgb color
  in String.concat [ "rgb("
    , (toString c.red)
    , ","
    , (toString c.green)
    , ","
    , toString c.blue
    , ")"
    ]

-- Final screens

centeredText : Int -> String -> Svg
centeredText y str = centeredTextWithOptions y [] str

centeredTextWithOptions : Int -> List Attribute -> String -> Svg
centeredTextWithOptions y attrs str = text' (List.concat
  [ [ SVGA.y (toString y) 
    , SVGA.x "200"
    , SVGA.fill "white"
    , SVGA.textAnchor "middle"
    , SVGA.stroke "black"
    , SVGA.strokeWidth "0.3pt"
    ]
  , attrs ])
  [ text str ]

renderSuccess : Game -> List Svg
renderSuccess game =
  [ renderPlayBG game.timeProgress
  , centeredText 120 "Você ganhou!"
  , centeredText 160 "Seu ciclo dia/noite está regulado."
  , centeredText 180 "Continue assim. :D"
  ]

renderFailure : Game -> List Svg
renderFailure game =
  [ renderPlayBG game.timeProgress
  , centeredText 90 "Que Pena."
  , centeredText 120 "Seu ciclo dia/noite está alterado, preste atenção nisso!"
  , centeredText 150 "Você sabia que essa alteração pode ocasionar insônia,"
  , centeredText 165 "obesidade, depressão e até mesmo câncer?"
  , centeredText 200 "Melhor evitar excesso de fontes de luz à noite."
  ]

renderInformation : Game -> List Svg
renderInformation game =
  [ renderPlayBG game.timeProgress
  , ( image
    [ SVGA.x "50"
    , SVGA.y "75"
    , SVGA.width "250"
    , SVGA.height "125"
    , SVGA.xlinkHref "tutorial.svg"
    ] [])
  , renderObjectScaled 2.0 <| Melatonin (330,100)
  , renderObjectScaled 1.8 <| LightSource (330,170)
  ]
      
-- Selection Scene
renderSelection : Game -> List Svg
renderSelection game =
  let
      female = { sex = Female, x = 100, y = 120 }
      male = { sex = Male, x = 260, y = 120 }
      selectedX = case game.player.sex of
        Female -> 90
        Male   -> 250 

  in
    [ renderSelectionBG
    , ( centeredTextWithOptions 30 
        [ SVGA.fontSize "1.5em"
        ]
        "Melato...ZzZz"
      )
    , centeredText 230 "Pressione <espaço> para começar"
    , renderCharacter male
    , renderCharacter female
    , renderSelectionRect selectedX
    ]

renderSelectionRect : Int -> Svg
renderSelectionRect x = rect
  [ SVGA.x (toString x)
  , SVGA.y "110"
  , SVGA.width "55"
  , SVGA.height "80"
  , SVGA.strokeWidth "2"
  , SVGA.stroke "black"
  , SVGA.fill "transparent"
  ]
  []
-- Play Scene
renderPlay : Game -> List Svg
renderPlay game = List.concat
  [ [ renderPlayBG game.timeProgress ]
  , renderMountains
  , [ renderFloor ]
  , renderPlayer game.player
  , List.map renderObject game.melatonins
  , [ renderScore game.score game.mode.scoreTarget
    , renderTime game.timeProgress
    , rect [SVGA.x "-1000", SVGA.y "-1000", SVGA.width "1000", SVGA.height "3000", SVGA.fill "white"] []
    , rect [SVGA.x (toString gameWidth), SVGA.y "-1000", SVGA.width "1000", SVGA.height "3000", SVGA.fill "white"] []
    , rect [SVGA.x "-1000", SVGA.y (toString gameHeight), SVGA.width "5000", SVGA.height "1000", SVGA.fill "white"] []
    ]
  ]

renderPlayer : Player -> List Svg
renderPlayer player =
  [ renderCharacter { player | x = player.x-gameWidth }
  , renderCharacter player
  ]

renderFloor : Svg
renderFloor = rect
  [ SVGA.x "0"
  , SVGA.y "280"
  , SVGA.height "20"
  , SVGA.width "400"
  , SVGA.fill "saddlebrown"
  ] []

renderMountains : List Svg
renderMountains =
  [ circle [ SVGA.cx "47", SVGA.cy "275", SVGA.r "70", SVGA.fill "green" ] []
  , circle [ SVGA.cx "290", SVGA.cy "380", SVGA.r "200", SVGA.fill "green" ] []
  , circle [ SVGA.cx "145", SVGA.cy "270", SVGA.r "90", SVGA.fill "green" ] []
  ]

renderScore : Int -> Int -> Svg
renderScore score scoreTarget = renderBar 10 Color.purple ((toFloat score)/(toFloat scoreTarget))

renderTime : Float -> Svg
renderTime progress = renderBar 25 Color.blue progress

renderBar : Int -> Color -> Float -> Svg
renderBar y c p = g []
  [ rect
    [ SVGA.x "10"
    , SVGA.y (toString y)
    , SVGA.height "10"
    , SVGA.width "380"
    , SVGA.fill "black"
    ][]
  , rect
    [ SVGA.x "10"
    , SVGA.y (toString y)
    , SVGA.height "10"
    , SVGA.width (toString (380*p))
    , SVGA.fill (colorToString c)
    ][]
  ]

combineColor : Float -> Color -> Color -> Color
combineColor r c1 c2 = let
    comb r a b = (r*a)+((1-r)*b)
    c1' = toHsl c1
    c2' = toHsl c2
    h = comb r c1'.hue c2'.hue
    s = comb r c1'.saturation c2'.saturation
    l = comb r c1'.lightness c2'.lightness
  in
    hsl h s l

renderPlayBG : Float -> Svg
renderPlayBG progress = 
  let
      dayColor = hsl (degrees 176) 0.75 0.5
      nightColor = hsl (degrees 233) 0.95 0.1
      color = combineColor progress nightColor dayColor 
  in
    rect (bgAttrs
      [ SVGA.fill (colorToString color)
      ]) []

renderSelectionBG : Svg
renderSelectionBG = image (bgAttrs
  [ SVGA.xlinkHref "./fundo.jpg"
  ]) []

renderObjectScaled : Float -> Object -> Svg
renderObjectScaled scale object =
  let
      renderImg x y sprite = image
        [ SVGA.xlinkHref sprite
        , SVGA.width (toString (24*scale))
        , SVGA.height (toString (24*scale))
        , SVGA.x (toString (x-12*scale))
        , SVGA.y (toString (y-12*scale))
        ] []
  in
      case object of
        Melatonin (x,y) -> renderImg x y "molecule.svg"
        LightSource (x,y) -> renderImg x y "owl-notebook.svg"

renderObject : Object -> Svg
renderObject object = renderObjectScaled 1 object

renderCharacter : Player -> Svg
renderCharacter player = 
  let 
    sprite = case player.sex of
      Male -> "homem.png"
      Female -> "mulher.png"
  in image
    [ SVGA.xlinkHref sprite
    , SVGA.width "35"
    , SVGA.height "60"
    , SVGA.y (
      (gameHeight - 60 - (round player.y))
        |> toString
    )
    , SVGA.x (
      player.x 
        |> toString
      )
    ]
    []
