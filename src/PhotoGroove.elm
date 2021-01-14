module PhotoGroove exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

initialModel : { photos : List { url : String }, selectedUrl : String }
initialModel =
  { photos = 
      [ { url = "1.jpeg" }
      , { url = "2.jpeg" }
      , { url = "3.jpeg" }
      ]
  , selectedUrl = "1.jpeg"
  }

update msg model =
  if msg.description == "ClickedPhoto" then
    { model | selectedUrl = msg.data }

  else
    model

urlPrefix : String
urlPrefix =
  "http://elm-in-action.com/"

view model =
  div [ class "content" ]
      [ h1 [] [ text "Photo Groove" ] 
      , div [ id "thumbnails" ] <|
          List.map (thumbnail model.selectedUrl) model.photos
      , img
          [ class "large" 
          , src (urlPrefix ++ "large/" ++ model.selectedUrl)
          ]
          []
      ]

thumbnail selectedUrl image =
  img [ src (urlPrefix ++ image.url)
      , classList [ ( "selected", selectedUrl == image.url ) ]
      , onClick { description = "ClickedPhoto", data = image.url }
      ]
      []

main =
  Browser.sandbox
    { init = initialModel 
    , view = view
    , update = update
    }
