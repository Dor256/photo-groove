module PhotoGroove exposing (main, update)

import Browser
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (onClick, on)
import Random
import Http
import Json.Decode as Decode exposing (Decoder, string, int, succeed, at)
import Json.Encode as Encode
import Json.Decode.Pipeline as Pipeline

type ThumbnailSize
  = Small
  | Medium
  | Large

type alias Photo =
  { url : String
  , size : Int
  , title : String
  }


type Status
  = Loading
  | Loaded (List Photo) String
  | Errored String

type alias Model =
  { status : Status
  , chosenSize : ThumbnailSize
  , hue : Int
  , ripple : Int
  , noise : Int
  }

initialModel : Model
initialModel =
  { status = Loading
  , chosenSize = Medium
  , hue = 0
  , ripple = 0
  , noise = 0
  }


fetchPhotos : Cmd Msg
fetchPhotos =
  Http.get
    { url = urlPrefix ++ "photos/list.json"
    , expect = Http.expectJson GotPhotos (Decode.list photoDecoder)
    }


photoDecoder : Decoder Photo
photoDecoder =
  succeed Photo
    |> Pipeline.required "url" string
    |> Pipeline.required "size" int
    |> Pipeline.optional "title" string "(untitled)"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ClickedPhoto url ->
        ( { model | status = selectUrl url model.status }, Cmd.none )
    
    ClickedSurpriseMe ->
        case model.status of
            Loaded (firstPhoto :: otherPhotos) _ ->
              Random.uniform firstPhoto otherPhotos
                |> Random.generate GotRandomPhoto
                |> Tuple.pair model

            Loaded [] _ ->
              ( model, Cmd.none )

            Loading ->
              ( model, Cmd.none )

            Errored _ ->
              ( model, Cmd.none )


    ClickedSize size ->
        ( { model | chosenSize = size }, Cmd.none )

    GotRandomPhoto photo ->
        ( { model | status = selectUrl photo.url model.status }, Cmd.none )

    GotPhotos (Ok photos) ->
        case photos of
          (firstPhoto :: _) ->
            ( { model | status = Loaded photos firstPhoto.url }, Cmd.none )
          
          [] ->
            ( { model | status = Errored "0 photos found" }, Cmd.none )
        
    GotPhotos (Err _) ->
        ( { model | status = Errored "Server error!" } , Cmd.none )

    SlidHue hue ->
        ( { model | hue = hue }, Cmd.none )

    SlidRipple ripple ->
        ( { model | ripple = ripple }, Cmd.none )

    SlidNoise noise ->
        ( { model | noise = noise }, Cmd.none )


selectUrl : String -> Status -> Status
selectUrl url status =
  case status of
    Loaded photos _ ->
      Loaded photos url

    Loading ->
      Loading

    Errored errorMessage ->
      Errored errorMessage


urlPrefix : String
urlPrefix =
  "http://elm-in-action.com/"


type Msg
  = ClickedPhoto String
  | ClickedSize ThumbnailSize
  | ClickedSurpriseMe
  | GotRandomPhoto Photo
  | GotPhotos (Result Http.Error (List Photo))
  | SlidHue Int
  | SlidRipple Int
  | SlidNoise Int


view : Model -> Html Msg
view model =
  div [ class "content" ] <|
    case model.status of
        Loaded photos selectedUrl ->
          photoList photos selectedUrl model

        Loading ->
          []

        Errored errorMessage ->
          [ text ("Error: " ++ errorMessage) ]

photoList : List Photo -> String -> Model -> List (Html Msg)
photoList photos selectedUrl model =
  [ h1 [] [ text "Photo Groove" ]
      , button
          [ onClick ClickedSurpriseMe ]
          [ text "Surprise Me!" ]
      , div [ class "filters" ]
            [ imageFilter SlidHue "Hue" model.hue
            , imageFilter SlidRipple "Ripple" model.ripple
            , imageFilter SlidNoise "Noise" model.noise
            ]
      , h3 [] [ text "Thumbnal Size:" ]
      , div [ id "choose-size" ] <|
          List.map sizeChooser [ Small, Medium, Large ]
      , div [ id "thumbnails", class (sizeToClass model.chosenSize) ] <|
          List.map (thumbnail selectedUrl) photos
      , img
          [ class "large" 
          , src (urlPrefix ++ "large/" ++ selectedUrl)
          ]
          []
      ]    

thumbnail : String -> Photo -> Html Msg
thumbnail selectedUrl image =
  img 
      [ src (urlPrefix ++ image.url)
      , title (image.title ++ " [" ++ String.fromInt image.size ++ " KB]")
      , classList [ ( "selected", selectedUrl == image.url ) ]
      , onClick (ClickedPhoto image.url)
      ]
      []

sizeChooser : ThumbnailSize -> Html Msg
sizeChooser size =
  label []
    [ input [ type_ "radio", name "size", onClick (ClickedSize size) ] []
    , text (sizeToString size)
    ]


rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
  node "range-slider" attributes children


imageFilter : (Int -> msg) -> String -> Int -> Html msg
imageFilter toMsg name magnitude =
  div [ class "filter-slider" ]
      [ label [] [ text name ]
      , rangeSlider
          [ Attr.max "11"
          , Attr.property "val" (Encode.int magnitude)
          , onSlide toMsg
          ]
          []
      , label [] [ text (String.fromInt magnitude) ]
      ]

sizeToString : ThumbnailSize -> String
sizeToString size =
  case size of
    Small ->
      "small"
    
    Medium ->
      "medium"
    
    Large ->
      "large"

sizeToClass : ThumbnailSize -> String
sizeToClass size =
  case size of
    Small ->
      "small"
    
    Medium ->
      "med"
    
    Large ->
      "large"


onSlide : (Int -> msg) -> Attribute msg
onSlide toMsg =
  at [ "detail", "userSlidTo" ] int
    |> Decode.map toMsg
    |> on "slide"

main : Program () Model Msg
main =
  Browser.element
    { init = \_ -> ( initialModel, fetchPhotos ) 
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }
