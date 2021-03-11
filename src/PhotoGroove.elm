port module PhotoGroove exposing (Model, Photo, main, update, photoDecoder, Msg(..), initialModel, view, urlPrefix, Status(..))

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


port setFilters : FilterOptions -> Cmd msg

port activityChanges : (String -> msg) -> Sub msg

type alias FilterOptions =
  { url : String
  , filters : List { name : String, amount : Float }
  }

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
  , activity : String
  , chosenSize : ThumbnailSize
  , hue : Int
  , ripple : Int
  , noise : Int
  }

initialModel : Model
initialModel =
  { status = Loading
  , activity = ""
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
       applyFilters { model | status = selectUrl url model.status }
    
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
        applyFilters { model | status = selectUrl photo.url model.status }

    GotActivity activity ->
        ( { model | activity = activity }, Cmd.none )

    GotPhotos (Ok photos) ->
        case photos of
          _ :: _ ->
            applyFilters
              { model 
                  | status =
                      case List.head photos of 
                          Just photo -> 
                            Loaded photos photo.url
                            
                          Nothing ->
                            Loaded [] ""
              }
          
          [] ->
            ( { model | status = Errored "0 photos found" }, Cmd.none )
        
    GotPhotos (Err _) ->
        ( { model | status = Errored "Server error!" } , Cmd.none )

    SlidHue hue ->
        applyFilters { model | hue = hue }

    SlidRipple ripple ->
        applyFilters { model | ripple = ripple }

    SlidNoise noise ->
        applyFilters { model | noise = noise }


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
  case model.status of
    Loaded _ selectedUrl ->
      let
        filters =
          [ { name = "Hue", amount = toFloat model.hue / 11 }
          , { name = "Ripple", amount = toFloat model.ripple / 11 } 
          , { name = "Noise", amount = toFloat model.noise / 11 }
          ]
        
        url =
          urlPrefix ++ "large/" ++ selectedUrl

      in
      ( model, setFilters { url = url, filters = filters } )
    
    Loading ->
      ( model, Cmd.none )

    Errored _ ->
      ( model, Cmd.none )


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
  | GotActivity String
  | GotPhotos (Result Http.Error (List Photo))
  | SlidHue Int
  | SlidRipple Int
  | SlidNoise Int


view : Model -> Html Msg
view model =
  div [ class "content" ] <|
    case model.status of
        Loaded photos selectedUrl ->
          [ h1 [] [ text "Photo Groove" ]
          , button
              [ onClick ClickedSurpriseMe ]
              [ text "Surprise Me!" ]
          , div [ class "activity" ] [ text model.activity ]
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
          , canvas
              [ id "main-canvas", class "large" ] [] 
          ]

        Loading ->
          []

        Errored errorMessage ->
          [ text ("Error: " ++ errorMessage) ] 

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

main : Program Float Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


init : Float -> ( Model, Cmd Msg )
init flags =
  let
    activity =
      "Initializing Pasta v" ++ String.fromFloat flags
    
  in
  ( { initialModel | activity = activity }, fetchPhotos )

subscriptions : Model -> Sub Msg
subscriptions _ =
  activityChanges GotActivity
