module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode
import PhotoGroove exposing (Model, Msg(..), Photo, initialModel, update, view)
import Html.Attributes as Attr exposing (src)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text, tag, attribute)


decoderTest : Test
decoderTest =
    fuzz2 string int "Title defaults to (untitled)" <|
    \url size ->
        [ ( "url", Encode.string url )
        , ( "size", Encode.int size )
        ]
            |> Encode.object
            |> decodeValue PhotoGroove.photoDecoder
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
                