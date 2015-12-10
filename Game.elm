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
melatoninGenerator = Random.int melatoninRadius (gameWidth-melatoninRadius)
melatoninDelay = 1500
gameDuration = 120000.0

main = 
  Signal.map2 view Window.dimensions game

game : Signal Game
game = Signal.foldp updateGame defaultGame input

delta : Signal Time
delta =
      Signal.map inSeconds (fps 60)

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
updateGame (t,input) game = case game.scene of
  PlayScreen -> game
    |> updatePlayer input.x
    |> updateMelatonins
    |> dropMelatonins t
    |> captureMelatonins
    |> updateClock t
    |> finishIfEnded
  _ -> updateOnSelect t input game

finishIfEnded : Game -> Game
finishIfEnded game = 
  if (game.clock - game.startedAt) > gameDuration then
    { game | scene = SelectionScreen }
  else
    game

updateClock : Time -> Game -> Game
updateClock t game = { game | clock = t }

updateOnSelect : Time -> Input -> Game -> Game
updateOnSelect time input game =
  if input.space then
    { game | scene = PlayScreen, startedAt = time }
  else
    (case input.x of
      1 -> { game | player = defaultMale }
      (-1) -> { game | player = defaultFemale }
      _ -> game
    )

movePlayer : Float -> Player -> Player
movePlayer dx player =
  let newX = player.x+dx
  in
      if newX > gameWidth-35 then
        { player | x = gameWidth-35 }
      else if newX < 0 then
        { player | x = 0 }
      else 
        { player | x = newX }

dropMelatonins : Time -> Game -> Game
dropMelatonins t game =
  if (t-game.dropMelatoninAt) >= 0 then
    case Random.generate melatoninGenerator game.melatoninSeed of
      (nextX,nextSeed) ->
        { game | melatoninSeed = nextSeed, melatonins = (nextX,0) :: game.melatonins, dropMelatoninAt = t+melatoninDelay }
  else game

updatePlayer : Int -> Game -> Game
updatePlayer dx game =
  let
      speed = 2.5
      player = movePlayer ((toFloat dx)*speed) game.player
  in
      { game | player = player }

updateMelatonins : Game -> Game
updateMelatonins game =
  let
      updateMelatonine (x,y) = (x,y+3)
      melatonins = game.melatonins
        |> List.map updateMelatonine
        |> List.filter (\(x,y) -> y < gameHeight)
  in
      { game | melatonins = melatonins }

isCollidingWith : Player -> (Int,Int) -> Bool
isCollidingWith player (x,y) =
  if y < 240 then
    False
  else if ( toFloat x + 5 > player.x ) && ( toFloat x < player.x+40 ) then
    True
  else
    False

captureMelatonins : Game -> Game
captureMelatonins game =
  let
      isColliding = isCollidingWith game.player
      isNotColliding = not << isColliding
      scored = game.melatonins
        |> List.filter isColliding
        |> List.length
        |> (*) 10
      newMelatonins = game.melatonins
        |> List.filter isNotColliding

      newScore = game.score+scored
  in
      { game | melatonins = newMelatonins, score = newScore }

-- MODEL
type alias Point = (Int,Int)

type Sex = Male | Female
type Scene = SelectionScreen | PlayScreen

type alias Player =
  { sex: Sex
  , x: Float
  , y: Float
  }

defaultFemale : Player
defaultFemale =
  { sex = Female
  , x = 165
  , y = 0
  }

defaultMale : Player
defaultMale =
  { sex = Male
  , x = 165
  , y = 0
  }

  

type alias Game = 
  { player: Player
  , melatonins: List Point
  , score: Int
  , scene: Scene
  , melatoninSeed: Random.Seed
  , dropMelatoninAt: Time
  , startedAt: Time
  , clock: Time
  }

defaultGame : Game
defaultGame =
  { player = defaultMale
  , melatonins = []
  , score = 0
  , scene = SelectionScreen
  , melatoninSeed = Random.initialSeed 1234
  , dropMelatoninAt = 0
  , startedAt = 0
  , clock = 0
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
  )
      
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
    , renderPlayer male
    , renderPlayer female
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
  [ [ renderPlayBG (game.clock - game.startedAt)
    , renderPlayer game.player
    , renderScore game.score
    ]
  , List.map renderMelatonine game.melatonins
  ]

renderScore : Int -> Svg
renderScore score = text'
  [ SVGA.x "395"
  , SVGA.y "20"
  , SVGA.textAnchor "end"
  ]
  [ text (toString score) ]

bgAttrs : List Attribute -> List Attribute
bgAttrs attrs = (List.concat [
    [ SVGA.x "0"
    , SVGA.y "0"
    , SVGA.height (gameHeight |> toString)
    , SVGA.width (gameWidth |> toString)
    ],
    attrs
  ])

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

renderPlayBG : Time -> Svg
renderPlayBG time = 
  let
      night = 1000
      timeRatio = time/gameDuration
      dayColor = hsl (degrees 176) 0.75 0.5
      nightColor = hsl (degrees 233) 0.95 0.1
      color = combineColor timeRatio nightColor dayColor 
      c = toRgb color
      colorStr = String.concat [ "rgb("
      , (toString c.red)
      , ","
      , (toString c.green)
      , ","
      , toString c.blue
      , ")"
      ]
  in
    --text' [SVGA.y "100"] [ text colorStr ]
    rect (bgAttrs
      [ SVGA.fill colorStr
      ]) []

renderSelectionBG : Svg
renderSelectionBG = image (bgAttrs
  [ SVGA.xlinkHref "./fundo.jpg"
  ]) []

renderMelatonine : Point -> Svg
renderMelatonine (x,y) = g []
  [ circle
    [ SVGA.cx (toString x)
    , SVGA.cy (toString y)
    , SVGA.r  (toString melatoninRadius)
    , SVGA.fill "#8533C4"
    ] [] 
  , text'
    [ SVGA.x (toString (x-5))
    , SVGA.y (toString (y+5))
    ] [ text "Z" ]
  ]

renderPlayer : Player -> Svg
renderPlayer player = 
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
