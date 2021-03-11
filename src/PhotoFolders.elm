module PhotoFolders exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Dict exposing (Dict)
import Html.Attributes exposing (target)

type Folder =
    Folder
        { name : String
        , photoUrls : List String
        , subfolders : List Folder
        , expanded : Bool
        }

type alias Model =
    { selectedPhotoUrl: Maybe String
    , photos : Dict String Photo
    , root : Folder
    }

initialModel : Model
initialModel =
    { selectedPhotoUrl = Nothing
    , photos = Dict.empty
    , root = Folder { name = "Loading...", photoUrls = [], subfolders = [], expanded = True }
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Http.get
        { url = "http://elm-in-action.com/folders/list"
        , expect = Http.expectJson GotInitialModel modelDecoder
        }
    )

modelDecoder : Decoder Model
modelDecoder =
    Decode.succeed
        { selectedPhotoUrl = Just "trevi"
        , photos = Dict.fromList
            [ ( "trevi"
              , { title = "Trevi"
                , relatedUrls = [ "coli", "fresco" ]
                , size = 34
                , url = "trevi"
                }
              )
            , ( "fresco"
              , { title = "Fresco"
                , relatedUrls = [ "trevi" ] 
                , size = 46
                , url = "fresco"}
              )
            , ( "coli"
              , { title = "Coliseum"
                , relatedUrls = [ "trevi", "fresco" ]
                , size = 36
                , url = "coli"
                }
              )
            ]
        , root =
            Folder
                { name = "Photos", photoUrls = []
                , subfolders =
                    [ Folder
                        { name = "2016", photoUrls = [ "trevi", "coli" ]
                        , subfolders =
                            [ Folder
                                { name = "outdoors"
                                , photoUrls = [], subfolders = []
                                , expanded = True
                                } 
                            , Folder
                                { name = "indoors"
                                , photoUrls = [ "fresco" ], subfolders = []
                                , expanded = True
                                }
                            ]
                        , expanded = True
                        }
                    , Folder
                        { name = "2017", photoUrls = []
                        , subfolders =
                            [ Folder
                                { name = "outdoors"
                                , photoUrls = [], subfolders = []
                                , expanded = True
                                }
                            , Folder
                                { name = "indoors"
                                , photoUrls = [], subfolders = []
                                , expanded = True
                                }
                            ]
                        , expanded = True
                        }
                    ]
                , expanded = True
                }
        }

type FolderPath
    = End
    | Subfolder Int FolderPath

toggleExpansion : FolderPath -> Folder -> Folder
toggleExpansion path (Folder folder) =
    case path of
        End ->
            Folder { folder | expanded = not folder.expanded }
        
        Subfolder targetIndex remainingPath ->
            let
                subfolders : List Folder
                subfolders =
                    List.indexedMap transform folder.subfolders
                
                transform : Int -> Folder -> Folder
                transform currentIndex currentSubfolder =
                    if currentIndex == targetIndex then
                        toggleExpansion remainingPath currentSubfolder

                    else
                        currentSubfolder
            in
            Folder { folder | subfolders = subfolders }

type Msg
    = ClickedPhoto String
    | GotInitialModel (Result Http.Error Model)

type alias Photo =
    { title : String
    , size : Int
    , relatedUrls : List String
    , url : String
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | selectedPhotoUrl = Just url }, Cmd.none )
        
        GotInitialModel (Ok newModel) ->
            ( newModel, Cmd.none )

        GotInitialModel (Err _) ->
            ( model, Cmd.none )

urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"

renderSelectedPhoto : Photo -> Html Msg
renderSelectedPhoto photo =
    div
        [ class "selected-photo" ]
        [ h2 [] [ text photo.title ] 
        , img [ src (urlPrefix ++ "photos/" ++ photo.url ++ "/full") ] []
        , span [] [ text (String.fromInt photo.size ++ "KB" ) ]
        , h3 [] [ text "Related" ]
        , div [ class "related-photos" ] <|
            List.map renderRelatedPhoto photo.relatedUrls
        ]

renderRelatedPhoto : String -> Html Msg
renderRelatedPhoto url =
    img
        [ class "related-photo"
        , onClick (ClickedPhoto url)
        , src (urlPrefix ++ "photos/" ++ url ++ "/thumb")
        ]
        []

renderFolder : Folder -> Html Msg
renderFolder (Folder folder) =
    let
        subfolders =
            List.map renderFolder folder.subfolders
    in
    div [ class "folder" ]
        [ label [] [ text folder.name ]
        , div [ class "subfolders" ] subfolders
        ]
 
view : Model -> Html Msg
view model =
    let
        photoByUrl : String -> Maybe Photo
        photoByUrl url =
            Dict.get url model.photos
        
        selectedPhoto : Html Msg
        selectedPhoto =
            case Maybe.andThen photoByUrl model.selectedPhotoUrl of 
                Just photo ->
                    renderSelectedPhoto photo
                
                Nothing ->
                    text ""
    in
    div [ class "content" ]
        [ div [ class "folders" ] 
            [ h1 [] [ text "Folders" ]
            , renderFolder model.root
            ] 
        , div [ class "selected-photo" ] [ selectedPhoto ] 
        ]

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
