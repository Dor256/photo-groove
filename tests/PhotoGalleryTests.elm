module PhotoGalleryTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, string)
import Test exposing (..)
import Json.Decode exposing (decodeValue)
import Json.Encode as Encode
import PhotoGallery exposing (Model, Msg(..), Photo, initialModel, update, view, urlPrefix, Status(..))
import Html.Attributes as Attr
import Test.Html.Query as Query
import Test.Html.Selector exposing (tag, attribute)
import Test.Html.Event as Event


decoderTest : Test
decoderTest =
    fuzz2 string int "Title defaults to (untitled)" <|
    \url size ->
        [ ( "url", Encode.string url )
        , ( "size", Encode.int size )
        ]
            |> Encode.object
            |> decodeValue PhotoGallery.photoDecoder
            |> Result.map .title
            |> Expect.equal (Ok "(untitled)")


sliders : Test
sliders =
    describe "Slider sets the desired field in the Model"
        [ testSlider "SlidHue" SlidHue .hue
        , testSlider "SlidRipple" SlidRipple .ripple
        , testSlider "SlidNoise" SlidNoise .noise
        ]


testSlider : String -> (Int -> Msg) -> (Model -> Int) -> Test
testSlider description toMsg amountFromModel =
    fuzz int description <|
        \amount ->
            initialModel
                |> update (toMsg amount)
                |> Tuple.first
                |> amountFromModel
                |> Expect.equal amount


noPhotosNoThumbnails : Test
noPhotosNoThumbnails =
    test "No thumbnails render when there are no photos to render." <|
        \_ ->
            initialModel
                |> view
                |> Query.fromHtml
                |> Query.findAll [ tag "img" ]
                |> Query.count (Expect.equal 0)

thumbnailRendered : String -> Query.Single msg ->  Expectation
thumbnailRendered url query =
    query
        |> Query.findAll [ tag "img", attribute (Attr.src (urlPrefix ++ url)) ]
        |> Query.count (Expect.atLeast 1)

photoFromUrl : String -> Photo
photoFromUrl url =
    { url = url, size = 0, title = "" }

thumbnailsWork : Test
thumbnailsWork =
    fuzz urlFuzzer "URLs render as thumbnails" <|
        \urls ->
            let
                thumbnailChecks : List (Query.Single msg -> Expectation)
                thumbnailChecks =
                    List.map thumbnailRendered urls
            in
            { initialModel | status = Loaded (List.map photoFromUrl urls) "" }
                |> view
                |> Query.fromHtml
                |> Expect.all thumbnailChecks

urlFuzzer : Fuzzer (List String)
urlFuzzer =
    Fuzz.intRange 1 5
        |> Fuzz.map urlsFromCount

urlsFromCount : Int -> List String
urlsFromCount urlCount =
    List.range 1 urlCount
        |> List.map (\num -> String.fromInt num ++ ".png")

clickThumbnail : Test
clickThumbnail =
    fuzz3 urlFuzzer string urlFuzzer "Clicking a thumbnail selects it" <|
        \urlsBefore urlToSelect urlsAfter ->
            let
                url =
                    urlToSelect ++ ".jpeg"

                photos =
                    (urlsBefore ++ url :: urlsAfter)
                        |> List.map photoFromUrl
                
                srcToClick =
                    urlPrefix ++ url
            in
            { initialModel | status = Loaded photos "" }
                |> view
                |> Query.fromHtml
                |> Query.find [ tag "img", attribute (Attr.src srcToClick) ]
                |> Event.simulate Event.click
                |> Event.expect (ClickedPhoto url)
       