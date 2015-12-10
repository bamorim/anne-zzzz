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

-- CONSTANTS
gameWidth = 400
gameHeight = 300
melatonineRadius = 10
melatonineGenerator = Random.int melatonineRadius (gameWidth-melatonineRadius)

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

input : Signal Input
input = Signal.sampleOn delta <|
      Signal.map2 Input
        (Signal.map .x Keyboard.arrows)
        Keyboard.space

-- UPDATE

updateGame : Input -> Game -> Game
updateGame input game = case game.scene of
  PlayScreen -> game
    |> updatePlayer input.x
    |> updateMelatonines
    |> addMelatoninesIfApplicable
    |> captureMelatonines
  _ -> updateOnSelect input game

updateOnSelect : Input -> Game -> Game
updateOnSelect input game =
  if input.space then
    { game | scene = PlayScreen }
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

addMelatoninesIfApplicable : Game -> Game
addMelatoninesIfApplicable game = case (List.length game.melatonines) of
        0 -> case Random.generate melatonineGenerator game.melatonineSeed of
          (nextX,nextSeed) ->
            { game | melatonineSeed = nextSeed, melatonines = [(nextX,0)] }
        _ -> game

updatePlayer : Int -> Game -> Game
updatePlayer dx game =
  let
      speed = 2.5
      player = movePlayer ((toFloat dx)*speed) game.player
  in
      { game | player = player }

updateMelatonines : Game -> Game
updateMelatonines game =
  let
      updateMelatonine (x,y) = (x,y+3)
      melatonines = game.melatonines
        |> List.map updateMelatonine
        |> List.filter (\(x,y) -> y < gameHeight)
  in
      { game | melatonines = melatonines }

isCollidingWith : Player -> (Int,Int) -> Bool
isCollidingWith player (x,y) =
  if y < 240 then
    False
  else if ( toFloat x + 5 > player.x ) && ( toFloat x < player.x+40 ) then
    True
  else
    False

captureMelatonines : Game -> Game
captureMelatonines game =
  let
      isColliding = isCollidingWith game.player
      isNotColliding = not << isColliding
      scored = game.melatonines
        |> List.filter isColliding
        |> List.length
        |> (*) 10
      newMelatonines = game.melatonines
        |> List.filter isNotColliding

      newScore = game.score+scored
  in
      { game | melatonines = newMelatonines, score = newScore }

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
  , melatonines: List Point
  , score: Int
  , scene: Scene
  , melatonineSeed: Random.Seed
  }

defaultGame : Game
defaultGame =
  { player = defaultMale
  , melatonines = []
  , score = 0
  , scene = SelectionScreen
  , melatonineSeed = Random.initialSeed 1234
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
    [ renderBackground
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
  [ [ renderBackground
    , renderPlayer game.player
    , renderScore game.score
    ]
  , List.map renderMelatonine game.melatonines
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

renderMelatonine : Point -> Svg
renderMelatonine (x,y) = circle
  [ SVGA.cx (toString x)
  , SVGA.cy (toString y)
  , SVGA.r  (toString melatonineRadius)
  , SVGA.fill "red"
  ] []

renderBackground : Svg
renderBackground = image (bgAttrs
  [ SVGA.xlinkHref "./fundo.jpg"
  ]) []

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

-- ACTIONS
